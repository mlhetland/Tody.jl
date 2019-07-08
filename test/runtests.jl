using Test, Tody

include("test.jl")

using Tody: Ex, Eval, define, step!, done, run, lookup, @pseudo_str

# Shortcut
macro p_str(str)
    :(@pseudo_str($str))
end

@testset TodyTestSet "Tody" begin

    # The second round takes basically no time, but makes sure things are sort
    # of idempotent
    for i = 1:2

    @testset TodyTestSet "Orders" begin

        using Tody: Order

        P = Order(Int)

        @test cmp(1, 1) == 0
        @test_throws ErrorException cmp(1, 2, P)

        push!(P, 1 => 2)

        @test cmp(1, 2, P) == -1
        @test cmp(2, 1, P) == +1

        push!(P, 3 => 4)

        @test_throws ErrorException cmp(1, 4, P)

        push!(P, 2 => 3)

        @test cmp(1, 4, P) == -1
        @test_throws ErrorException push!(P, 4 => 2)

    end

    @testset TodyTestSet "Parsing" begin

        using Tody: Ex

        ## Parsing the syntax

        # Corner cases
        @test p" " isa Ex
        @test p"1" isa Ex
        @test p" { } " isa Ex
        @test p"if true {} else {}" isa Ex

        @test string(p"break; return") == "(do (break) (return))"

        @test string(p"-x^2") == "(- (^ x 2))"

        @test_throws ErrorException parse("1 / 2 * 3")

        @test string(p"1 + -2 - +3") == "(+ 1 (- (- 2) (+ 3)))"

        # Reluctant search, from *Pessimal Algorithms and Simplexity Analysis* by
        # Broder and Stolfi.

        ex = pseudo"""

        procedure research(X, i, j) {
            if i > j {
                return -1
            }
            if i == j && X == A {
                return i
            } else {
                return -1
            }
            m = (i + j) / 2
            if X ≤ A[m] {
                k = research(X, m+1, j)
                if k == -1 {
                    return research(X, i, m)
                } else {
                    return k
                }
            } else {
                k = research(X, i, m)
                if k == -1 {
                    return research(X, m+1, j)
                } else {
                    return k
                }
            }
        }

        """

        expected = """
        (procedure
           (research X i j)
           (do (if (> i j) (return (- 1)))
               (if (&& (== i j) (== X A))
                   (return i)
                   else
                   (return (- 1)))
               (= m (/ (+ i j) 2))
               (if (≤ X (at A m))
                   (do (= k (research X (+ m 1) j))
                       (if (== k (- 1))
                           (return (research X i m))
                           else
                           (return k)))
                   else
                   (do (= k (research X i m))
                       (if (== k (- 1))
                           (return (research X (+ m 1) j))
                           else
                           (return k))))))"""

        @test string(ex) == expected

        ## Parsing S-expressions

        @test ex == parse(Ex, expected)

    end

    @testset TodyTestSet "Partial eval" begin

        using Tody: Ex, peek, Eval, done

        input = "(+ (* 2 2) (- 3))"

        ex = parse(Ex, input)
        ev = Eval(ex)

        @test !done(ev)

        @test string(ev) == string(ex) == input

        push!(ev, :+)
        @test string(ev) == input

        @test string(peek(ev)) == "(* 2 2)"
        push!(ev, 4)
        @test string(ev) == "(+ 4 (- 3))"

        @test string(peek(ev)) == "(- 3)"
        push!(ev, -3)
        @test string(ev) == "(+ 4 -3)"

        @test done(ev)

    end

    output(ex::String; trace=false) = output(parse(ex), trace=trace)
    output(ex::Ex; trace=false) = output(run(ex), trace=trace)
    function output(ev::Eval; trace=false)
        trace && println()
        cur = ev
        while cur isa Ex
            trace && println(cur)
            cur = step!(cur)
        end
        String(take!(Tody.lookup(ev, :stdout)))
    end

    @testset TodyTestSet "Short-circuit" begin

        @test output(p"1 + 1 == 2 && print(1)") == "1"
        @test output(p"2 + 2 == 5 && print(1)") == ""
        @test output(p"1 + 1 == 2 || print(1)") == ""
        @test output(p"2 + 2 == 5 || print(1)") == "1"

    end

    @testset TodyTestSet "if/else if/else" begin

        ex = pseudo"""

            if true {
                print(1)
            } else {
                print(0)
            }

            if false {
                print(0)
            } else {
                print(1)
            }

            if false {
                print(0)
            } else if true {
                print(1)
            } else {
                print(0)
            }

            if false {
                print(0)
            } else if false {
                print(0)
            } else {
                print(1)
            }

        """

        @test output(ex) == "1111"

    end

    @testset TodyTestSet "while" begin

        ex = pseudo"""

            a = 93
            b = 42

            while b ≠ 0 {
                tmp = b
                b = a % b
                a = tmp
            }

            print(a)

        """

        @test output(ex) == "3"

    end

    @testset TodyTestSet "break" begin

        ex = p"""

            while true {
                print(1)
                break
                print(0)
            }

        """

        @test output(ex) == "1"

    end

    @testset TodyTestSet "continue" begin

        ex = p"""

            i = 1
            while i ≤ 3 {
                print(i)
                i = i + 1
                if i == 2 {
                    continue
                }
                print(i)
            }

        """

        @test output(ex) == "12334"

    end

    @testset TodyTestSet "for" begin


        ex = p"""

            for i in 0:9 {
                if i == 2 {
                    continue
                }
                print(i)
                if i == 8 {
                    break
                }
            }

        """

        @test output(ex) == "01345678"

        ex = p"""

            for x in A {
                print(x)
            }

        """

        ev = run(ex)

        define(ev, :A, [1, 2, 3])

        @test output(ev) == "123"


    end

    @testset TodyTestSet "definition" begin

        ex = p"""

            i = 1
            j = 1

            while true {
                i := 2
                j = 2
                break
            }

            print(i, j)

        """

        @test output(ex) == "12"

    end

    @testset TodyTestSet "Proc" begin

        using Tody: Proc, Env, BUILTINS

        env = BUILTINS()

        mysum = Proc(:mysum, [:x, :y], p"x + y", Env(env))

        define(env, :mysum, mysum)

        ex = p"print(mysum(2^1, 1+1))"
        ev = Eval(ex, env=env)

        @test output(ev) == "4"

    end

    @testset TodyTestSet "ProcDef" begin

        ex = p"""

        procedure mypow(x, y) {

            res = 1
            i = 0

            while i < y {
                res = res * x
                i = i + 1
            }

            return res

        }

        print(mypow(2, 3))

        """

        @test output(ex) == "8"

    end

    @testset TodyTestSet "return" begin

        ex = p"""


        procedure f() {
            if true {
                return
            }
            print(0)
        }

        print(1)
        f()
        print(2)

        """
        @test output(ex) == "12"

    end

    @testset TodyTestSet "recursion" begin

        ex = p"""

        procedure myfact(n) {
            if n == 0 {
                1
            } else {
                n * myfact(n - 1)
            }
        }

        print(myfact(5))

        """

        @test output(ex) == "120"

    end

    @testset TodyTestSet "closures" begin

        ex = p"""

        procedure outer() {

            var = "inside"

            procedure inner() {
                print(var)
            }

            return inner

        }

        var = "outside"

        proc = outer()

        proc()

        """

        @test output(ex) == "inside"

    end

    @testset TodyTestSet "attributes" begin

        ex = p"""

            print(e.h.π.d)
            print(e.h.d)

        """

        ev = run(ex)

        s = (d=0, π=nothing)
        v = (d=1, π=s)
        e = (t=s, h=v)

        define(ev, :e, e)

        @test output(ev) == "01"

        ex = p"""

            print(a.b.head)
            a.b.head = y
            print(a.b.head)

        """

        ev = run(ex)

        a = (b=Expr(:x),)
        define(ev, :a, a)
        define(ev, :y, :y)
        define(ev, :z, :z)

        @test output(ev) == "xy"

    end

    @testset TodyTestSet "indexing" begin

        ex = p"""

            print(A[3, 2])
            A[1, 1] = 4
            print(A[1, 1])

        """

        ev = run(ex)

        define(ev, :A, collect(reshape(1:16, 4, 4)))

        @test output(ev) == "74"

    end

    end

end
