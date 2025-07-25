## Expression parsing ########################################################

# Very simple, overly permissive S-expression parsing, without involving the
# parser framework. Does not permit empty expresions. If rpn is true, the head
# is assumed to be last rather than first in each expression.

function parse(::Type{Ex}, str::AbstractString; rpn=false)

    token = r"\( | \) | (\"([^\"\\]|\\[\"\\])*\") | [^\s()]+"x

    stack = [[], []]

    function close()
        v = pop!(stack)
        if rpn
            head = last(v)
            args = length(v) > 1 ? v[1:end-1] : []
        else
            head = first(v)
            args = length(v) > 1 ? v[2:end] : []
        end
        push!(last(stack), Call(head, args))
    end

    for m in eachmatch(token, str)
        t = m.match
        if t == "("
            push!(stack, [])
        elseif t == ")"
            close()
        elseif m[1] ≢ nil
            push!(last(stack), unescape_string(t[2:end-1]))
        else
            n = tryparse(Float64, t)
            push!(last(stack), n ≡ nil ? Symbol(t) : n)
        end
    end

    length(last(stack)) > 1 && close()

    return last(stack)[1]

end

## Expression pretty-printing ################################################

# Display rules loosely based on
# https://github.com/TotalVerb/SExpressions.jl/blob/master/src/Lists/show.jl

ppfmt(x)         = string(x)
ppfmt(x::Nil)    = "nil"
ppfmt(x::Number) = string(isinteger(x) ? Int(x) : x)
ppfmt(x::Bool)   = string(x) # To override Number
ppfmt(x::String) = repr(x)
ppfmt(x::Ex)     = string("(", join(ppfmt.([head(x), args(x)...]), " "), ")")

function pprint(io::IO, x; hang=false, indent=0)
    str = ppfmt(x)
    hang || print(io, " "^indent)
    print(io, str)
end

function pprint(io::IO, x::Ex; hang=false, indent=0)

    hang || print(io, " "^indent)

    ## Short enough to fit on the current line?

    str     = ppfmt(x)
    width   = textwidth(str)
    _, cols = displaysize(io)

    if indent + width ≤ cols && width ≤ cols/2  # Heuristic
        print(io, str)
        return
    end

    ## Otherwise, we must break it up:

    print(io, "(")

    headstr = ppfmt(head(x))                    # Never break the head
    print(io, headstr)

    headw = textwidth(headstr)
    sameln = headw ≤ 3                          # Heuristic
    extra = sameln ? headw + 2 : 3              # Extra indentation

    for arg in args(x)
        print(io, sameln ? " " : "\n")
        pprint(io, arg, hang=sameln, indent=indent + extra)
        sameln = false
    end

    print(io, ")")

end
