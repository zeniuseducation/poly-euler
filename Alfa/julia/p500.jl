using Memoize
using Lazy

include ("common.jl")

function square (x::Int)
    x * x
end

#stack overflow
function sol500a (target :: Int, lim :: Int, nprime :: Int)
    bahan = primes (lim)
    modi :: Int = 500500507
    powers = map (x -> collect (rest (takewhile (k -> k < lim, iterate (square,x))))  , primes (nprime))
    reduce ((a,b) -> (a*b) % modi ,take (target,apply (list,sort (vcat (bahan, apply (vcat, powers))))))
end

function sol500 (target :: Int, lim :: Int)
    # generate the primes up to a limit, this is ugly because primes
    # function returns not the n number of primes but primes below a
    # certain input, thus we need to "guess" the upper limit
    dprimes = primes (lim)
    upper :: Int = last (dprimes)

    # this is an array for all the powers of a prime smaller than lim
    powers = Int []
    i :: Int = 1

    # where we generate those powers, and finally sort it, only for
    # p^n where n >= 2 and p^2 <= lim 
    while true
        p = dprimes [i]
        if square (p) > upper
            break
        end
        curi :: Int = p*p
        while true
            push! (powers,curi)
            curi *= curi
            if curi > upper
                break
            end
        end
        i += 1
    end
    
    sort! (powers)
    i = 1
    j :: Int = 1
    res :: Int = 1
    counter :: Int = 0
    modi :: Int = 500500507
    while counter < target
        # we use the smaller of the two first element of either
        # dprimes or powers
        tmpi = dprimes [i]
        tmpj = powers [j]
        if tmpi < tmpj
            res = mod (res*tmpi, modi)
            i += 1
        else
            res = mod (res*tmpj, modi)
            j += 1
        end
        counter += 1
    end
    return res
end


