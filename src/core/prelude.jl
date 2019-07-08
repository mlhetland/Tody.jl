# Default configuration of operators and functions

import .Parsers: Op, Un, OpPolicy
import Base: show

function prelude(ops::OpPolicy)

    prec_levels = [
        Op.([:.]),
        Op.([:^]),
        Un.([:+, :-, :!, :~]),
        Op.([:*, :/, :%, :÷]),
        Op.([:-]),
        Op.([:+]),
        Op.([:(:)]),
        Op.([:<<, :>>, :>>>]),
        Op.([:<, :<=, :≤, :>, :>=, :≥]),
        Op.([:(==), :!=, :≠]),
        Op.([:&]),
        Op.([:⊻]),
        Op.([:|]),
        Op.([:&&]),
        Op.([:||]),
        Op.([:(=), :(:=)]),
    ]

    for (lvl1, lvl2) in zip(prec_levels[1:end-1], prec_levels[2:end])
        for op1 in lvl1, op2 in lvl2
            push!(ops, op2 => op1)
        end
    end

    return ops

end

function prelude(env::Env)

    # Named literals

    define(env, Symbol("true"),  true)   # :true and :false don't work
    define(env, Symbol("false"), false)
    define(env, :nil,            nil)

    # Operators

    for op in [:^, :+, :-, :!, :~, :*, :/, :%, :÷, :-, :+, :(:), :<<, :>>,
               :>>>, :<, :<=, :≤, :>, :>=, :≥, :(==), :!=, :≠, :&, :⊻, :|]
        define(env, op, getfield(Base, op))
    end

    # Basic functions

    for func in [
                 # Rounding functions
                 :round, :floor, :ceil, :trunc,

                 # Division functions
                 :div, :fld, :cld, :rem, :mod, :mod1, :mod2pi, :divrem,
                 :fldmod, :gcd, :lcm,

                 # Sign and absolute value functions
                 :abs, :abs2, :sign, :signbit, :copysign, :flipsign,

                 # Powers, logs and roots
                 :sqrt, :cbrt, :hypot, :exp, :expm1, :ldexp, :log, :log2,
                 :log10, :log1p, :exponent, :significand,

                 # Trigonometric and hyperbolic functions
                 :sin, :cos, :tan, :cot, :sec, :csc, :sinh, :cosh, :tanh,
                 :coth, :sech, :csch, :asin, :acos, :atan, :acot, :asec,
                 :acsc, :asinh, :acosh, :atanh, :acoth, :asech, :acsch, :sinc,
                 :cosc,

                 # Trigonometric functions using degrees
                 :sind, :cosd, :tand, :cotd, :secd, :cscd, :asind, :acosd,
                 :atand, :acotd, :asecd, :acscd,
                ]
        define(env, func, getfield(Base, func))
    end

    # Output

    define(env, :stdout,    IOBuffer())

    # Special forms

    define(env, :do,        Do())
    define(env, :.,         Dot())
    define(env, :at,        At())
    define(env, :print,     Print())
    define(env, :&&,        And())
    define(env, :||,        Or())
    define(env, :if,        If())
    define(env, :while,     While())
    define(env, :for,       For())
    define(env, :(:=),      Define())
    define(env, :(=),       Assign())
    define(env, :procedure, ProcDef())
    define(env, :return,    Return())
    define(env, :break,     Break())
    define(env, :continue,  Continue())

    return env

end


# Special forms, which require different evaluation than functions

abstract type Form end
abstract type Sequential <: Form end
abstract type Loop <: Form end

show(io::IO, form::Form) = print(io::IO, nameof(typeof(form)), "()")

apply!(ev::Eval, ::Sequential, args) = return!(ev, last(ev))

# The assumption here is that the special form is accessed via a symbol, so
# that the evaluation of the head itself is of the form
# eval!(ev, ::Symbol, ex). Thus the calls to eval! here are assumed to only
# apply to the arguments.

# apply! should return the Eval object for the next execution step, if any.

## do: really just a function, but grouped with other "statements" ###########

struct Do       <: Sequential end

## print: needs to access stdout #############################################

struct Print    <: Form       end

pfmt(x)         = ppfmt(x)
pfmt(x::String) = x # To override ppfmt

apply!(ev::Eval, ::Print, args) =
    (print(lookup(ev, :stdout)::IO, pfmt.(args)...); return!(ev, nil))

## ||, &&: short-circuit evaluation ##########################################

struct And      <: Sequential end
struct Or       <: Sequential end

conj(ev, ex, halt) = halt ? return!(ev, last(ev)) :
                            Eval(ex, par=ev, env=ev.env)

eval!(ev::Eval, ::Or, ex)  = conj(ev, ex, argn(ev.tgt) ≥ 1 &&  last(ev))
eval!(ev::Eval, ::And, ex) = conj(ev, ex, argn(ev.tgt) ≥ 1 && !last(ev))

## if: evaluates only relevant clauses #######################################

struct If       <: Sequential end

function eval!(ev::Eval, ::If, ex)

    # As arguments to if, both if and else are treated as "decorative noise
    # words", and are ignored.
    (ex == :if || ex == :else) && return skip!(ev)

    vals = args(ev.tgt)
    pos  = length(vals) + 1

    if isodd(pos) # A condition or an unconditional final else clause

        # A final, unconditional else clause
        pos == argn(ev) && !vals[pos-2] && return eval!(ev, ex)

        # No condition succeeded yet, so evaluate this:
        (pos == 1 || !vals[pos-2]) && return eval!(ev, ex)

        # The previous condition succeeded:
        return apply!(ev)

    else # A clause

        # Our condition succeeded:
        vals[pos-1] && return eval!(ev, ex)

        # Skipping this clause:
        return eval!(ev, nil)

    end

end

## while loops: evaluates a variable number of times #########################

struct While    <: Loop end

function eval!(ev::Eval, ::While, ex)

    ev.idx == 1 && return eval!(ev, ex)

    @assert ev.idx == 2

    if !first(args(ev.tgt))
        return!(ev, nil)
    else
        Eval(ex, par=ev, env=Env(ev.env))
    end

end

apply!(ev::Eval, ::While, args) = rewind!(ev)

## Assignment: rebinds one, evaluates the other ##############################

abstract type Assignment <: Form end
struct Define   <: Assignment end
struct Assign   <: Assignment end

struct LHS{T} <: Expression
    ref::T
end

lhseval!(ev::Eval, ex::Symbol) = noeval!(ev, ex)
lhseval!(ev::Eval, ex::Call) = LHS(eval!(ev::Eval, ex))

# Left-hand-side calls are evaluated as normal, except for the application:
step!(lhs::LHS) = done(lhs.ref) ? noapply!(lhs.ref) : LHS(eval!(lhs.ref))

eval!(ev::Eval, ::Assignment, ex) =
    ev.idx == 1 ? lhseval!(ev, ex) : eval!(ev, ex)

apply!(ev::Eval, ::Define, args) =
    ((x, y) = args; define(ev, x, y); return!(ev, y))

function apply!(ev::Eval, ::Assign, (x, y))
    if x isa Symbol
        assign(ev, x, y)
    elseif head(x) isa Dot
        a = collect(args(x))
        lhs = foldl(getfield, a[1:end-1])
        setfield!(lhs, a[end], y)
    else
        @assert head(x) isa At
        a = collect(args(x))
        obj, idx = a[1], int.(a[2:end])
        obj[idx...] = y
    end
    return!(ev, y)
end

## Procedure objects #########################################################

struct Proc     <: Sequential
    name    :: Symbol
    pars    :: Vector{Symbol}
    body    :: Expression
    env     :: Env
end

show(io::IO, proc::Proc) = print(io, "Proc(:$(proc.name))")

function apply!(ev::Eval, proc::Proc, args)

    env = Env(proc.env)
    for (par, arg) in zip(proc.pars, args)
        define(env, par, arg)
    end

    Eval(proc.body, env=env, par=ev.par, frame=true)

end

## Procedure definitions #####################################################

struct ProcDef  <: Form end

eval!(ev::Eval, ::ProcDef, ex) = noeval!(ev, ex)

function apply!(ev::Eval, ::ProcDef, (sign, body))

    name = head(sign)
    pars = args(sign)

    proc = Proc(name, pars, body, ev.env)

    define(ev, name, proc)

    push!(parent(ev), nil)
    parent(ev)

end

## return ####################################################################

struct Return   <: Form end

function apply!(ev::Eval, ::Return, args)

    v = collect(args)
    @assert length(v) ≤ 1
    res = isempty(v) ? nil : first(args)

    ret = parent(ev)
    while !isframe(ret)
        ret = parent(ret)
    end

    return!(ret, res)

end

## break #####################################################################

struct Break    <: Form end

function apply!(ev::Eval, ::Break, ())

    ret = parent(ev)
    while !(head(ret) isa Loop)
        ret = parent(ret)
    end

    return!(ret, nil)

end

## continue ##################################################################

struct Continue <: Form end

function apply!(ev::Eval, ::Continue, ())

    ret = parent(ev)
    while !(head(ret) isa Loop)
        ret = parent(ret)
    end

    rewind!(ret)
    ret

end

## for #######################################################################

# for i in vals { dosomething }
#
# Can be rendered differently if vals is an OrdinalRange (e.g., "for i = 1 to
# n" or "for i = n downto 1 by 2" or the like) than if it's any other iterable
# object.
#
# Note that unless/until negative numbers are handled differently from
# unary operators, things like n:-1:1 won't work. Use n:(-1):1 or n : -1 : 1.

struct For      <: Loop end

function eval!(ev::Eval, ::For, ex)

    ev.idx == 2 && @assert ex == :in
    ev.idx <= 2 && return skip!(ev)
    ev.idx == 3 && return eval!(ev, ex)

    @assert ev.idx == 4

    i_name = first(args(ev.src))

    # Defined in the environment ev has inherited, so it might clash with
    # other loops, i.e., be reused by them, but that doesn't matter.
    i_state_name = Symbol("$i_name iteration state")

    range = first(args(ev.tgt))

    env = Env(ev.env)

    if haskey(ev.env.vars, i_state_name)
        i_state = ev.env.vars[i_state_name]
        next = iterate(range, i_state)
    else
        next = iterate(range)
    end

    if next ≡ nil
        # Clean up after us:
        haskey(ev.env.vars, i_state_name) && delete(ev.env, i_state_name)
        return return!(ev, nil)
    end

    i, i_state = next

    define(env, i_name, i)
    define(ev, i_state_name, i_state)

    Eval(ex, par=ev, env=env)

end

apply!(ev::Eval, ::For, args) = rewind!(ev, 1)

## attribute access ##########################################################

struct Dot      <: Form end

eval!(ev::Eval, ::Dot, ex) = ev.idx == 1 ? eval!(ev, ex) : noeval!(ev, ex)
apply!(ev::Eval, ::Dot, args) = return!(ev, foldl(getfield,  args))

## indexing ##################################################################

struct At       <: Form end

int(x)         = x
int(x::Number) = Int(x)

apply!(ev::Eval, ::At, args) = return!(ev, getindex(int.(args)...))
