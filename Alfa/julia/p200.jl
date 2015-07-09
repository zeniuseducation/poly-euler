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

function sol179a (lim :: Int)
    dprime = fill (Set (), lim)
    llim :: Int = isqrt (lim)
    for i = 2:lim
        if i <= llim
            for j = i*2:i:lim
                if i*i == j
                    tmp = dprime [j]
                    dprime [j]= push!(tmp,i)
                else
                    dctr :: Int = 0
                    tmpj :: Int = j
                    tmps = Set ()
                    while true
                        if 0 == tmpj % i
                            dctr += 1
                            push! (tmps,i^dctr)
                            push! (tmps,div (j, (i^dctr)))
                            tmpj = div (tmpj,i)
                        else
                            break
                        end
                    end
                    tmp = dprime [j]
                    dprime [j]= union(tmp, tmps)
                end
            end
        end
    end

    res ::Int = 0
    for i = 2:lim-1
        if (length (dprime [i])) == (length (dprime [i+1]))
            println (i, dprime [i])
            res += 1
        end
    end
    return res
end

function sol179c (lim :: Int)
    dprime = ones (Int, lim)
    llim :: Int = div(lim,2)
    for i = 2:lim
        if i <= llim
            for j = i*2:i:lim
                dprime [j] += 1
            end
        end
    end

    res :: Int = 0
    
    prev = dprime [2]
    for i=3:lim
        tmp = dprime [i]
        if tmp == prev
            res += 1
        end
        prev = tmp
    end
    return res
end
    

function sol179 (lim :: Int)
    # initial divisors of 2
    prev :: Int = prod (map (x -> x+1,(values (factor (2)))))
    # counter 
    res :: Int = 0
    
    for i = 3:lim
        curi :: Int = prod (map (x-> x+1, (values (factor (i)))))
        if prev==curi
            res += 1
        end
        prev = curi
    end
    return res
end

function sol191a (lim :: Int)
    function prize (xs, late, n :: Int)
        if n == lim
            tmp = length (filter (x-> x == "a", xs))
            if late || (any (x-> x=="l",xs))
                if tmp == 2
                    return 1
                else
                    return 2
                end
            else
                if tmp == 2
                    return 2
                else
                    return 3
                end
            end
        else
            tmp = length (filter (x-> x == "a", xs))
            if late
                if tmp == 2
                    bahan = [(xs [2]),"o"]
                    return prize (bahan,late,n+1)
                else
                    bahan = list ([(xs [2]),"a"],[(xs [2]),"o"])
                    return sum (map (x-> prize (x, late,n+1), bahan))
                end
            else
                if tmp == 2
                    bahan = list([(xs [2]), "o"], [(xs [2]), "l"])
                    return sum (map (x-> prize (x, late, n+1), bahan))
                elseif any (x-> x == "l", xs)
                    bahan = list([(xs [2]), "o"], [(xs [2]), "a"])
                    return sum (map (x-> prize (x, true, n+1), bahan))
                else
                    bahan = list([(xs [2]),"o"], [(xs [2]),"a"], [(xs [2]),"l"])
                    return sum (map (x-> prize (x, late, n+1), bahan))
                end
            end
        end
    end
    start = list(["a","a"],["a","o"],["a","l"],["o","a"],["l","a"],["o","l"],["l","o"],["o","o"])
    sum (map (x-> prize (x,false,3), start))
end

function sol191 (lim :: Int)
    function getlate (st::Int)
        (div (st,10)==3) || (rem (st,10) == 3)
    end

    function rakit (st::Int, nday::Int)
        rem (st,10) * 10 + nday
    end
            
    @memoize function prize (st :: Int, n :: Int, late :: Bool)
        if n == lim
            if late || getlate (st)
                if st == 22
                    return 1
                else
                    return 2
                end
            else
                if st == 22
                    return 2
                else
                    return 3
                end
            end
        else
            if late || getlate (st)
                if st == 22
                    return prize (rakit (st,1), n+1,true)
                else
                    return prize (rakit (st,2), n+1, true)+prize (rakit (st,1), n+1, true)
                end
            else
                if st == 22
                    return sum (map (x-> prize (rakit (st,x), n+1,late), [1,3]))
                else
                    return sum (map (x-> prize (rakit (st,x),n+1,late), 1:3))
                end
            end
        end
    end

    sum (map (x-> prize (x,3,false), [11,12,13,21,22,23,31,32]))
end

# Imperative style
function sol173 (lim :: Int)
    function lamina (n :: Int)
        res :: Int = 0
        for i = n-2:-2:1
            if square (n) - square (i) <= lim
                res += 1
            else
                break
            end
        end
        return res
    end
    
    evenres::Int=0
    oddres::Int=0
    for i = 4:2:(div (lim,4)+2)
        evenres += lamina (i)
    end
    for i = 3:2:(div (lim,4)+2)
        oddres += lamina (i)
    end
    return evenres+oddres
end

# Functional style
function sol173f (lim::Int)
    function lamina (n::Int)
        function interesting (b::Int)
            b > 0 && 0 < square (n)-square (b) <= lim
        end
        length (takewhile (interesting, iterate (x->x-2,n-2)))
    end

    sum (map (lamina,4:2:(div (lim,4)+2))) + sum (map (lamina,3:2:(div (lim,4)+1)))
end

function sol174 (lim :: Int)
    refs = zeros (Int,lim)
    function lamina (n :: Int)
        res :: Int = 0
        for i = n-2:-2:1
            tmp = square (n) - square (i)
            if tmp <= lim
                refs [tmp] += 1
            else
                break
            end
        end
    end
    
    for i = 3:(div (lim,4)+2)
        lamina (i)
    end
    resi :: Int = 0
    for i = 1:lim
        if 1 <= refs [i] <= 10
            resi += 1
        end
    end
    return resi
end

function crazy (lim::Int)
    refs = trues (lim)
    llim :: Int = isqrt (lim)
    res :: Int = 0
    for i = 2:lim
        if refs [i]
            if i <= llim
                for j = (i*i):i:lim
                    refs [j] = false
                end
            end
            res += 1
        else
            if i % 100000 == 0
                println (i," ", log (i), " ", i/res)
            end
        end
    end
end

function sol193a (lim::Int)
    refs = trues (lim)
    llim = isqrt (lim)
    res :: BigInt = BigInt (ifloor (0.75 * lim * lim))
    r = 0
    for i = 3:2:lim
        if refs [i]
            for j = i*2:i:lim
                refs [j] = false
            end
            isq = i * i
            res = ifloor (res * (isq - 1) / isq)
            r = i
        end
    end
    println (r)
    return res
end

function drs (n::Int)
    tmp = n % 9
    if tmp == 0
        return 9
    else
        return tmp
    end
end

function sol159 (lim :: Int)  
    llim :: Int = isqrt (lim)
    refs = map (drs, 1:lim)
    res :: Int = 0
    for i = 2:lim
        ci = refs [i]
        if i <= llim
            for j = i*i:i:lim
                tmp = div (j,i)
                curj = refs [j]
                posj = ci + refs [tmp]
                if posj > curj
                    refs [j]=posj
                end
            end
        end
        res += ci
    end
    return res
end

function sol188 (a:: Int, b::Int, modi::Int)
    function hyper_expt (n :: Int, m::Int)
        if m == 1
            return n
        else
            return powermod (a,hyper_expt (a,m-1),modi)
        end
    end

    hyper_expt (a,b)
end

function sol187 (lim :: Int)
    primes = sieve (div (lim,2))
    ctr = length (primes)
    llim = isqrt (lim)
    res = 0
    j = ctr
    for i = 1:ctr
        pi = primes [i]
        if pi > llim
            return res
        end
        while true
            pj = primes [j]
            tmp = pi*pj
            if tmp < lim
                res += j-i+1
                break
            end
            j -= 1
        end
    end
    res
end

function cutpaper (lst)
    len = length (lst)
    if len == 1
        fpap = lst [1]
        for i = fpap:4
            push! (lst, i+1)
        end
        return lst [2:end]
    else
        tmp = rand (1:len)
        fpap = lst [tmp]
        if fpap == 5
            return vcat (lst [1:tmp-1], lst [tmp+1:end])
        else
            for i = fpap:4
                push! (lst, i+1)
            end
            return vcat (lst [1:tmp-1], lst [tmp+1:end])
        end
    end
end

function batchjob ()
    pope = Int [1]
    ctr :: Int = 0
    for i = 1:15
        if i == 1
            pope = cutpaper (pope)
        else
            if length (pope) == 1
                ctr += 1
            end
            pope = cutpaper (pope)
        end
    end
    return ctr
end

function sol151 (lim :: Int)
    res :: Dict = {0=>0, 1=>0, 2=>0, 3=>0, 4=>0}
    for i = 1:lim
        tmp = batchjob ()
        res [tmp] += 1
    end
    tots :: Int = sum (values (res))
    ctr :: Real = 0.0
    for i in keys (res)
        ctr += i*(res [i])/tots
    end
    ctr
end













