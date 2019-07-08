import Base: cmp, push!

struct Op
    val     :: Symbol
    unary   :: Bool
end
Op(x::Symbol) = Op(x, false)
Op(x::Vector) = Op(Symbol(x[1]))
Op(x, c::Ctx) = Op(x)

# A symbol can have a different precedence if it acts as a unary operator
Un(x::Symbol) = Op(x, true)
Un(x::Vector) = Un(Symbol(x[1]))
Un(x, c::Ctx) = [nil, Un(x)] # Dummy left operand

l1(x) = length(x) == 1

U(x, c) = Call(x[1], x[2:end]) # Unary
I(x, c) = Call(:at, x)         # Indexing
C(x, c) = Call(nil, x)         # Temporary marker for call

G(x, c) = (!l1(x)
              || first(x) isa Symbol) ?
              Call(x[1], x[2:end]) : x         # Group
B(x, c) = l1(x) ? x : Call(:do, x)             # Block
E(x, c) = l1(x) ? x : infixtoprefix(x, c)      # Infix operators

Na(x, c) = Symbol(x[1])                        # Name
Nu(x, c) = parse(Float64, x[1])                # Number
St(x, c) = unescape_string(x[1][2:end-1])      # String
Ln(x, c) = G(x, c)                             # Line

function combine(x, c)
    isempty(x)     && return nil
    length(x) == 1 && return x[1]
    lhs, rhs = x
    head(rhs) ≡ nil && return Call(lhs, args(rhs))
    insert!(rhs, 2, lhs)
    return rhs
end

struct Ops <: OpPolicy
    prec    :: Order
end
Ops() = Ops(Order(Op))

isop(op,     ops::Ops) = false
isop(op::Op, ops::Ops) = true
node(op, lhs, rhs, ops::Ops) = Call(op, Any[lhs, rhs])

cmp(a::Op, b::Op, c::Ops) = cmp(a, b, c.prec)
push!(ops::Ops, lt::Pair{Op}) = push!(ops.prec, lt)

cleanup(x, c) = (@assert length(x) == 1; cleanup(first(x)))
cleanup(x) = x
cleanup(x::Op) = x.val
cleanup(x::Ex) = Call(cleanup(head(x)), cleanup.(args(x)[args(x) .≢ nil]))
