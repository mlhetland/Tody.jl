# A minimalist parser combinator framework. Grammars may be specified using
# strings, regular expressions and the operators & and | (the formner with a
# couple of variants). For example, a simple subset of arithmetic might be
# described as follows:
#
#   number    = r"\d+"
#   product() = number  & ( "*" & product | ϵ )
#   sum()     = product & ( "+" & sum     | ϵ )
#
# Here ϵ represents the empty string. The sequential composition (&) skips
# over any whitespace; there is a version (&̲, i.e., &\underbar) that does not
# skip over any whitespace. This is useful for more specific whitespace
# matching, either for tight juxtaposition, for requiring whitespace (using
# the sp parser), or for matching horizontal/non-breaking space (nbsp if space
# is required, or nbspᵋ if it's optional).
#
# Note that product and sum are specified as functions rather than variables,
# because they access parsers that are not yet available. Such functions are
# automatically called to retrieve the actual parsers to use.
#
# Normally, matched regular expressions become part of the parse result, while
# strings are discarded; the parse result may be altered with the semantic
# action combinator (^), which wraps the result of a subparser, using a
# function or constructor. For example, one might specify
#
#   number ^ s->parse(Int, s)
#
# as part of a rule, to get an actual integer in the parse result, rather than
# its string representation. This functionality can be used to build up custom
# parse trees.

import Base: cmp, push!

const Wrapper = Union{Function, Type}
abstract type OpPolicy end

abstract type Cmb end
const Parser = Union{Regex, AbstractString, Function, Cmb}

# Parsing context
mutable struct Ctx
    grm     :: Parser
    ops     :: Union{OpPolicy, Nil}
    str     :: AbstractString
    idx     :: Int
    log     :: Vector{AbstractString}
end
Ctx(grm, ops, str) = Ctx(grm, ops, str, 1, [])

rewind!(c::Ctx, i::Int) = (c.idx = i; c)
addmsg!(c::Ctx, m) = push!(c.log, m)

function missed!(c::Ctx, p)
    s = c.str
    n = lastindex(s)
    i = min(c.idx, n)
    j = min(nextind(s, min(c.idx + 20, n)), n)
    txt = s[i:j]
    j < n && (txt = string(txt, "..."))
    msg = "$p at index $(c.idx): \"$txt\""
    addmsg!(c, msg)
end

clrmsg!(c::Ctx)    = empty!(c.log)
errmsg(c::Ctx)     = string("syntax error\n",
                     "The following attempted matches failed:\n",
                     join(unique(c.log), "\n"))

# Delegating any special handling of binary operators (used, e.g., in custom
# precedence-based parsing):
isop(x, c::Ctx) = isop(x, c.ops)
node(op, lhs, rhs, c::Ctx) = node(op, lhs, rhs, c.ops)

cmp(a, b, c::Ctx) = cmp(a, b, c.ops)
push!(c::Ctx, lt::Pair) = push!(c.ops, lt)

# Sequential parser combinator
struct Seq <: Cmb
    sub     :: Vector{Parser}
end

# Disjunctive parser combinator
struct Alt <: Cmb
    sub     :: Vector{Parser}
end

# Semantic action combinator: Pass result through callable wrapper
struct Wrap <: Cmb
    p       :: Parser
    w       :: Wrapper
end

function tryparse(parser::Parser, oppolicy::OpPolicy, input::AbstractString;
                  force=false)
    c = Ctx(parser, oppolicy, input)
    res = parse(parser, c)
    if res ≢ nil
        @assert length(res) == 1
        res = first(res)
    end
    (res ≡ nil && force) ? error(join(Parsers.errmsg(c))) : res
end
parse(parser::Parser, oppolicy::OpPolicy, input::AbstractString) =
    tryparse(parser, oppolicy, input, force=true)

parse(f::Function, c::Ctx) = parse(f(), c)

# The parse functions return lists of subexpressions, and this list is spliced
# into the current sequential results, resulting in somewhat flattened parse
# trees. Single subexpressions should thus be returned as singleton lists, and
# dropped results as empty lists.
#
# Wrappers used with the Wrap combinator can return nothing to trigger Wrap to
# return an empty list (rather than, say, a singleton list containing an empty
# list).

# Match a string a the current index, and drop the result
function parse(p::AbstractString, c::Ctx)
    if !startswith(SubString(c.str, c.idx), p)
        missed!(c, p)
        return nil
    end
    c.idx = c.idx + sizeof(p)
    [] # String matches are dropped
end

# Match a regex at the current index, and return the result
function parse(p::Regex, c::Ctx)
    m = match(p, c.str, c.idx)
    if m ≡ nil || m.offset ≠ c.idx
        missed!(c, p)
        return nil
    end
    c.idx = c.idx + sizeof(m.match)
    [m.match]
end

# Parse with subparsers in sequence and return concatenate results
function parse(p::Seq, c::Ctx)
    res = []
    for s in p.sub
        r = parse(s, c)
        r ≡ nil && return nil
        append!(res, r)
    end
    return res
end

# Try parsing with each subparser in turn; return the first successful result
function parse(p::Alt, c::Ctx)
    i = c.idx
    for s in p.sub
        r = parse(s, c)
        r ≢ nil && (clrmsg!(c); return r)
        rewind!(c, i)
    end
    return nil
end

# Parse with subparser and wrap the result using the wrapper
function parse(p::Wrap, c::Ctx)
    r = parse(p.p, c)
    r ≡ nil && return nil
    w = p.w(r, c)
    w ≡ nil ? [] : w isa AbstractVector ? w : [w]
end

## BNF-like syntactic sugar for grammar specifications

# Utility functions for flattening/n-ary parsing of the grammar specification
# itself: Wrap parsers of different kinds in a list, but splice in parsers of
# the same kind (e.g., sequential parsers being sequentially composed).
inseq(x::Seq) = x.sub
inseq(x)      = Parser[x]

inalt(x::Alt) = x.sub
inalt(x)      = Parser[x]

# Wrap a parser; parse result is passed through a function or constructor
(^)(p::Parser, w::Wrapper) = Wrap(p, w)

# Add a descriptor, for use in error messages
(..)(s::AbstractString, p::Parser) = Desc(p, s)

# Use as wrapper, to drop result
drop(x, c) = nil

# Some useful constants (dropped from parse result)
const ϵ        =  ""                         # Empty string (\epsilon)
const sp       =  r"\s+"           ^ drop    # Whitespace
const nbsp     =  r"[^\S\r\n]+"    ^ drop    # Non-breaking space
const spᵋ      =  r"\s*"           ^ drop    # Optional space
const nbspᵋ    =  r"[^\S\r\n]*"    ^ drop    # Optional horizontal space
const bof      =  r"^"             ^ drop    # Beginning of file
const eof      =  r"$"             ^ drop    # End of file
const eol      =  r"([\r\n]|\r\n)" ^ drop    # End of line

# Sequential parsing, whitespace permitted
(&)(l::Parser, r::Parser) = Seq([inseq(l)..., spᵋ, inseq(r)...])

# Sequential parsing, no whitespace permitted (&\underbar)
(&̲)(l::Parser, r::Parser) = Seq([inseq(l)...,      inseq(r)...])

# Alternative parsing
(|)(l::Parser, r::Parser) = Alt([inalt(l)...,      inalt(r)...])
