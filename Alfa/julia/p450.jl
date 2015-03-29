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


