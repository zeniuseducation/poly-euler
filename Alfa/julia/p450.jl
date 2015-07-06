using Lazy
using Memoize

include ("common.jl")

function max_primes (lim :: Int)
    primes = trues (lim)
    llim :: Int = isqrt (lim)
    modi :: Int =
    maxprimes = Array (Int)
    for i = 2:lim
        if primes [i]
            if i <= llim
                for j = i*i:i:lim
                    primes [j] = false
                end
            end
            push! (maxprimes, max_expt (i,modi))
        end
    end
end

function sol407 (lim :: Int)
    nref = zeros (Int,lim)
    for a = 2:lim
        asd = a*a-a
        diva = divisors (asd)
        for n in diva
            if n <= a
                continue
            end
            if n > lim 
                break
            end
            if a > nref [n]
                nref [n] = a
            end
        end
    end
    sum (nref)
end


