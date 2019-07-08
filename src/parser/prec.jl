## Precedence climbing, for re-arranging infix to prefix

head(x) = nil
peek(x) = isempty(x) ? nil : last(x)

function infixtoprefix(x, c)
    y = reverse(x)
    isempty(y) && return nil
    climb(pop!(y), y, nil, c)
end

function climb(lhs, rest, curop, ctx)

    while true

        nextop = peek(rest)

        isop(nextop, ctx) || break
        curop ≡ nil || curop == nextop || cmp(curop, nextop, ctx) < 0 || break

        op  = pop!(rest)
        rhs = pop!(rest)

        while true

            nextop = peek(rest)

            isop(nextop, ctx) || break
            cmp(op, nextop, ctx) < 0 || break

            rhs = climb(rhs, rest, nextop, ctx)

        end

        if lhs isa Ex && head(lhs) == op
            push!(lhs, rhs)
        else
            lhs = node(op, lhs, rhs, ctx)
        end

    end

    return lhs

end
