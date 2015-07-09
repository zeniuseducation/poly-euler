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

@memoize function sumkuad (a :: Int, b :: Int)
    tmp :: Int = a*b
    res :: Int = 0
    for x in 1:(a-1)
        for y in 1:(b-1)
            if (b*x + a*y) < tmp
                res += 1
            end 
        end
    end
    res
end

function sol504 (m :: Int)
    res :: Int = 0
    for i in 1:m
        for j in 1:m
            ji = sumkuad (i,j) + (i-1)
            for k in 1:m
                jk = sumkuad (j,k) + (j-1)
                for l in 1:m
                    kl = sumkuad (k,l) + (k-1)
                    li = sumkuad (l,i) + (l-1)
                    total = 1+ji+jk+kl+li
                    if ispsqr (total)
                        res += 1
                    end
                end
            end
        end
    end
    res
end

function sol504b (m :: Int)
    refs = zeros (Int, (m,m))
    function isumkuad (a :: Int, b :: Int)
        tmpi = refs [a,b]
        if tmpi == 0 
            tmp :: Int = a*b
            resi :: Int = 0
            for x in 1:(a-1)
                for y in 1:(b-1)
                    if (b*x + a*y) < tmp
                        resi += 1
                    end 
                end
            end
            refs [a,b] = resi
            return resi 
        else
            return tmpi
        end
    end
    res :: Int = 0
    for i in 1:m
        for j in 1:m
            ji = isumkuad (i,j) + (i-1)
            for k in 1:m
                jk = isumkuad (j,k) + (j-1)
                for l in 1:m
                    kl = isumkuad (k,l) + (k-1)
                    li = isumkuad (l,i) + (l-1)
                    total = 1+ji+jk+kl+li
                    if ispsqr (total)
                        res += 1
                    end
                end
            end
        end
    end
    res
end

function p512 (lim :: Int)
    primes = sieve (lim) [2:end]
    tots = [1:lim]
    for p in primes
        tots [p] = p-1
        for i = (3*p):(2*p):lim
            tmp = tots [i]
            tots [i] = div (tmp*(p-1),p)
        end
    end
    1+sum (x -> tots [x], 3:2:lim)
end

function p512b (lim :: Int)
    primes = trues (lim)
    tots = [1:lim]
    llim :: Int = isqrt (lim)
    res :: Int = 1
    for i = 3:2:lim
        if primes [i]
            if i <= llim
                for j = (i*i):i*2:lim
                    primes [j] = false
                end 
            end
            for j = 3*i:2*i:lim
                tots [j] = div ((tots [j])*(i-1), i)
            end
            res += (i-1)
        else
            res += tots [i]
        end
    end
    res
end











