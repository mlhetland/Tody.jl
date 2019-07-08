module Tody

export @pseudo_str

import Base: pop!, push!, parse, run

const Nil = Nothing
const nil = nothing

using Base.Iterators
import Base: ==, show, parse, push!, peek, isempty, length, last, insert!

## Expressions ###############################################################

abstract type Expression end

const Ex = Expression

parts(ex::Ex) = ex.parts
parts!(ex::Ex, p) = (empty!(ex.parts); append!(ex.parts, p); ex.parts)

isempty(ex::Ex) = isempty(parts(ex))
length(ex::Ex) = length(parts(ex))
push!(ex::Ex, v) = push!(parts(ex), v)
insert!(ex::Ex, i, v) = insert!(parts(ex), i, v)
head(ex::Ex) = parts(ex)[1]
args(ex::Ex) = @view parts(ex)[2:end]
argn(ex::Ex) = length(args(ex))

==(x::Ex, y::Ex) = head(x) == head(y) && args(x) == args(y)

show(io::IO, x::Ex) = pprint(io, x)

## Function applications #####################################################

struct Call <: Expression
    parts    :: AbstractVector
end
Call() = Call([])
Call(head, args) = Call([head, args...])

## Incremental evaluations ###################################################

## Environments

struct Env
    vars    :: Dict{Symbol, Any}
    par     :: Union{Env, Nil}
end
Env(par=nil) = Env(Dict(), par)

locate(e::Env, name)      = haskey(e.vars, name) ? e : locate(e.par, name)
locate( ::Nil, name)      = error("unknown variable $name")

trylocate(e::Env, name)   = haskey(e.vars, name) ? e : trylocate(e.par, name)
trylocate( ::Nil, name)   = nil

defined(e::Env, name)     = haskey(e.vars, name) ? true : defined(e.par, name)
defined( ::Nil, name)     = false

lookup(e::Env, name)      = locate(e, name).vars[name]

define(e::Env, name, val) = e.vars[name] = val
delete(e::Env, name)      = delete!(e.vars, name)

# Try to rebind existing variable. Define variable if not found:
function assign(e::Env, name, val)
    env = trylocate(e, name)
    env ≡ nil && (env = e)
    define(env, name, val)
end

parent(e::Env) = e.par
parent!(e::Env, par) = e.par = par

## An expression with some of its components evaluated

mutable struct Eval <: Expression
    src     :: Call
    tgt     :: Call
    env     :: Env
    par     :: Union{Eval, Nil}
    idx     :: Int
    frame   :: Bool
end

Eval(ex::Call; env=Env(), par=nil, frame=false) =
    Eval(ex, Call(), env, par, 0, frame)

lookup(e::Eval, name)      = lookup(e.env, name)
define(e::Eval, name, val) = define(e.env, name, val)
assign(e::Eval, name, val) = assign(e.env, name, val)

defined(e::Eval, name)     = defined(e.env, name)

head(e::Eval) = isempty(e.tgt) ? head(e.src) : head(e.tgt)

args(e::Eval) = flatten((args(e.tgt), drop(args(e.src), argn(e.tgt))))
argn(e::Eval) = argn(e.src)

parent(e::Eval)  = e.par
parent!(e::Eval, par) = e.par = par
isframe(e::Eval) = e.frame

done(e::Eval) =      e.idx == length(e.src)
peek(e::Eval) =      (@assert !done(e); parts(e.src)[e.idx + 1])
last(e::Eval) =      last(parts(e.tgt))
skip!(e::Eval) =     (e.idx += 1; e)
push!(e::Eval, v) =  (@assert !done(e); e.idx += 1; push!(e.tgt, v))
rewind!(e::Eval) =   (e.idx = 1; parts!(e.tgt, [head(e.tgt)]); e)

function rewind!(e::Eval, n::Int)
    @assert 1 ≤ n < e.idx
    # Might still trigger indexing error, if some args have been skipped.
    e.idx -= n
    parts!(e.tgt, parts(e.tgt)[1:end-n])
    return e
end

return!(::Nil) = nil

function return!(ev::Eval, v)
    (par = parent(ev)) ≡ nil && return v
    push!(par, v)
    return par
end

## One step in a stepwise evaluation

step!(ev::Eval) = done(ev) ? apply!(ev) : eval!(ev)
eval!(ev::Eval) = eval!(ev, head(ev), peek(ev))
apply!(ev::Eval) = apply!(ev, head(ev), args(ev))

# By default, the head and all arguments are evaluated eagerly. Note that when
# hd ≡ ex, this dispatches on the pre-evaluated head (e.g., a symbol), and for
# the arguments, it dispatches on the evaluated head (e.g., a function).
eval!(ev::Eval, hd, ex)     = eval!(ev, ex)
eval!(ev::Eval, ex)         = noeval!(ev, ex)
eval!(ev::Eval, ex::Symbol) = noeval!(ev, lookup(ev, ex))
eval!(ev::Eval, ex::Call)   = Eval(ex, par=ev, env=ev.env)

apply!(ev::Eval, func::Function, args) = return!(ev, func(args...))

noeval!(ev::Eval, ex)       = (push!(ev, ex); ev)
noapply!(ev::Eval)          = return!(ev, ev)


run(ex::Ex) = Eval(ex, env=BUILTINS())

##############################################################################

include("util/expr.jl")
include("util/orders.jl")
include("parser/Parsers.jl")
include("core/prelude.jl")

const GRAMMAR  = Parsers.program
const OPPOLICY = Parsers.Ops()
BUILTINS() = prelude(Env())

prelude(OPPOLICY)

function parse(str::AbstractString)

    compacted = replace(str, r"\s*"s => "")
    compacted = compacted == "{}" ? "" : compacted
    isempty(compacted) && return Call([:do])
    res = parse(GRAMMAR, OPPOLICY, str)
    res = res isa Ex ? res : Call([:do, res])

    return res
end

macro pseudo_str(s)
    :($(parse(s)))
end

end
