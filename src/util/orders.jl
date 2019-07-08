# A partial order

import Base: cmp, in, push!

struct Order{T}
    rel     :: Set{Pair{T}}
end
Order(T::Type) = Order(Set{Pair{T}}())

function push!(P::Order{T}, lt::Pair{T}) where {T}

    a, b = lt

    push!(P.rel, lt)
    push!(P.rel, a => a)
    push!(P.rel, b => b)

    # Update transitive closure:
    src, tgt = Set{T}(), Set{T}()
    for p in P.rel
        p.second == a && push!(src, p.first)
        p.first == b && push!(tgt, p.second)
    end
    for u in src, v in tgt
        push!(P.rel, u => v)
    end

    for (u, v) in P.rel
        (v => u) in P.rel && u ≠ v && error("irreflexive: $a vs $b")
    end

end

in(lt::Pair{T}, P::Order{T}) where {T} = lt in P.rel

function cmp(a::T, b::T, P::Order{T}) where {T}
    if a == b
        return 0
    elseif (a => b) ∈ P
        return -1
    elseif (b => a) ∈ P
        return +1
    else
        error("undefined: $a vs $b")
    end
end
