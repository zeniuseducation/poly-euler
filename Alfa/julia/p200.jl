using Memoize
using Lazy

include("common.jl")

function sol216 (lim::Int)
    res :: Int = 0
    for i = 2:lim
        tmp = 2*i*i - 1
        if prime (tmp)
            res += 1
        end
    end
    return res 
end

function sol169 (n::Int)
    powers = map (x -> ^(2,x), range ());
    bahan = takewhile (x -> x <= n, powers)
    raws = sort (apply (vcat,[bahan,bahan]))
    return raws
end

function sol187l (lim :: Int)
    primes = trues (lim)
    refs = zeros (Int,lim)
    res :: Int = 0
    for i = 2:lim
        if primes [i]
            for j = i*2:i:lim
                primes [j] = false
                d :: Int = 1
                while (j % (i^d)) == 0
                    d += 1
                end
                refs [j] = refs [j] + d - 1
            end
        else
            if refs [i] == 2
                res += 1
            end
        end
    end
    return res
end

function sol187b (lim :: Int)
    primes = trues (lim)
    llim :: Int = isqrt (lim)
    res :: Int = 0
    for i = 2:lim
        if primes [i]
            for j = i*2:i:lim
                primes [j] = false
                tmp = div (j,i)
                if tmp <= i
                    if primes [tmp]
                        res += 1
                    end
                end
            end
        end
    end
    return res
end

function sol187(lim :: Int)
    prs = primes(div (lim,2))
    len :: Int = length (prs)
    res :: Int = 0
    for i = 1:len
        for j = i:len
            if prs[i]*prs[j] > lim
                break
            end
            res += 1
        end
    end
    return res
end








