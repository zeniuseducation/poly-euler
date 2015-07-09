using Memoize
using Lazy

include ("common.jl")

function modex (a, m , modi)
    if m == 0
        return 1
    elseif m == 1
        return a
    else
        next = modex (a, div (m,2),modi)
        if 0 == rem (m,2)
            return rem (next*next,modi) 
        else
            return rem (next*next*a, modi)
        end
    end
end

function bigdecpow (a, m)
    if m == 0
        return 1
    elseif m == 1
        return a
    else
        next = bigdecpow(a, div (m,2))
        if 0 == rem (m,2)
            return next*next
        else
            return next*next*a
        end
    end
end

function fibo (i :: Int)
    if i == 0
        return 0
    elseif i == 1
        return 1
    else
        a = 1
        b = 0
        for j = 2:i
            tmp = a
            a = a+b
            b = tmp
        end
        return a
    end
end

function bigfibo (i :: Int)
    if i == 0
        return BigInt (0)
    elseif i == 1
        return BigInt (1)
    else
        sqrt5 = sqrt (5)
        Phi = (1+sqrt5)/2
        phi = (1-sqrt5)/2
        tmp :: BigFloat = (bigdecpow(BigFloat (Phi), i) - bigdecpow(BigFloat (phi), i)) / BigFloat (sqrt5)
        return  round (BigFloat (tmp))
    end
end

function sol304 (lim :: Int, foo :: Int, mul::Int)
    size = lim + foo * mul
    modi = BigFloat (1234567891011)
    llim = isqrt (size)
    refs = sieve (llim)
    refs = refs [2:end]
    rsize = length (refs)
    primes = trues (foo*mul)
    function pget (idx)
        primes [idx-lim]
    end 
    function pset (idx,value)
        primes [idx-lim] = value
    end
    uprimes = Int []
    for i = 1:rsize
        prime = refs [i]
        start = lim
        if 0 == rem (i,100000)
            println ("sieve", i)
        end
        tmp = rem (lim, prime)
        start = lim+prime-tmp
        if 0 == rem (start,2)
            start += prime
        end
        for j = start:2*prime:size
            pset (j, false)
        end
    end
    for i in lim+1:2:size
        if pget (i)
            push! (uprimes,i)
        end
    end
    res :: BigFloat = BigFloat (0)
    for i in 1:foo
        if i < 10
            println (uprimes [i])
        end
        res += bigfibo (uprimes [i])
        res = rem (res,modi)
        if 0 == rem (i,5000)
            println (i)
        end
    end
    res 
end
   



