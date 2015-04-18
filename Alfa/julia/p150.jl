using Memoize
using Lazy

include ("common.jl")

# This is intended for problem no 114 and 115
@memoize function blocks (blacks :: Int)
    min_reds :: Int = 50
    if blacks == 0
        return 1
    elseif blacks < min_reds
        return 0
    elseif blacks == min_reds
        return 1
    else
        res :: Int = 1
        for reds = min_reds : blacks
            for position = 0:(blacks-reds)
                next_blacks = blacks - reds - position -1
                if next_blacks >= 0
                    res = res + 1 + blocks (next_blacks)
                end
            end
        end
        return res
    end
end

function sol115 (lim :: Int)
    first (drop_while (x -> blocks (x) < lim, range (60)))
end

function sol145 (lim :: Int)
    res :: Int = 0
    for i = 1:lim
        j = reverse (numcol (i))
        if (first (j)) != 0
            tmp = i + colnum (j)
            if all(isodd, numcol (tmp))
                println (tmp)
                res += 1
            end
        end
    end
    return res
end

function diop (n::Int)
    return div (cdivs (n*n) + 1,2)
end

function sol108 (target::Int)
    bahan = reductions (*, apply (list,primes (100)))
    ires :: Int = 0
    for i in 1:length (bahan)
        tmp = bahan [i]
        res = diop (tmp)
        if res > target
            ires = bahan [i-1]
            break
        end
    end
    while true
        for i in range (1)
            tmp = diop (i*ires)
            if tmp > target
                return i*ires
            end
        end
    end
end

function gradient (p1,p2)
    /(p2 [:y] - p1 [:y], p2 [:x]-p1 [:x])
end


function sol102()
    bahan = map(x -> map(int,split (x,",")), map (chop,open (readlines,"p102.txt")))
    bahan = map (x-> map (k -> {:x => (x [k]), :y => (x [k+1])}, 1:2:5), bahan)
    
    for triangle in bahan
        (p1,p2,p3) = triangle
    end     
end


function sol104 ()
    a :: BigInt = BigInt (1)
    b :: BigInt = BigInt (1)
    i :: Int = 2

    function pandig (xs)
        [1:9] == sort (xs)
    end
    
    while a < 10^9
        tmp :: BigInt = BigInt(a)
        a += b
        b = tmp
        i += 1
    end

    while true
        diga = digits (a)
        depa = diga [1:9]
        bela = diga [end-8:end]
        if (pandig (bela)) && (pandig (depa))
            break
        end
        i += 1
        tmp = a
        a += b
        b = tmp
    end
    return i
end

function sol112 (lim)
    bouncy :: Int = 0
    nonbouncy :: Int = 99
    i :: Int = 100
    while true
        dig = digits (i)
        len = length (dig)
        if all (x -> dig [x] >= dig [x+1], 1:len-1) || all (x -> dig [x] <= dig [x+1], 1:len-1)
            nonbouncy += 1
        else
            bouncy += 1
        end
        if (bouncy / (bouncy+nonbouncy)) >= lim
            break
        end
        i += 1
    end
    return i
end

function diop (n :: BigInt)
    mps = factor (n)
    res :: BigInt = BigInt (1)
    for (n,e) in mps
        res = res * ((e * 2) + 1)
    end
    return div (res+1,2)
end

function insertby (f,x,xs)
    fx = f (x)
    for i = 1:length (xs)
        if fx > f (i)
            return vcat (xs [1:i],[x],xs [i+1:end])
        end
    end
    return push(xs,x)
end

function sol110a (lim ::Int)
    start :: BigInt = prod (primes (BigInt (35)))
    i ::Int = 1
    cur :: BigInt = BigInt (start)
    curdiop = diop (cur)
    
    while curdiop < lim
        cur = start * i
        curdiop = diop (cur)
        i += 1
    end
    return cur 
end

function caldiop (dic)
    div(prod (map (x -> x^(get (dic,x,0)), keys (dic))) + 1, 2)
end

function numdiop (dic)
    dprimes = sum (values (dic))
    prs = primes (15*dprimes) [1:dprimes]
    sdic = sort (collect(keys (dic)), rev=true)
    res :: Int = 1
    idx :: Int = 0
    for m in sdic
        ctr = get (dic, m,0)
        e = div (m-1,2)
        for n = idx+1:idx+ctr
            res *= prs [n] ^e
        end
        idx += ctr
    end
    return res
end

function create_nexts (dic)
    res = Dict []
    push! (res, merge (dic, {3 => (get (dic,3,0)) + 1}))
    bahan = filter ((x,y) -> y > 0, dic)
    for i in bahan
        (k,v) = i
        if k > 3
            nextkv = {k+2 => (get (dic, k+2, 0))+1, k => (get (dic,k,0))-1}
            push! (res, merge (dic, nextkv))
        else
            nextkv = {k+2 => (get (dic, k+2, 0))+1}
            push! (res, merge (dic, nextkv))
        end
    end
    return res
end

function sol110 (lim :: Int, limlen :: Int)
    @memoize function satisfy (st, n :: Int)
        if n > limlen
            tmp = caldiop (st)
            if tmp > lim
                return numdiop (st)
            else
                return 999999999999
            end
        else
            tmp = caldiop (st)
            if tmp > lim
                return numdiop(st)
            else
                nextst = create_nexts (st)
                return minimum(map (x -> satisfy (x,n+1), nextst))
            end
        end
    end
    return satisfy ({3 => 1}, 1)
end

function factdiop (mp)
    div(prod (map (x -> x [2]*2 + 1, mp))+1,2)
end

function factnum (mp)
    res :: Int = 1
    for m in mp
        (p,e) = m
        res *= p^e
    end
    return res
end

function usol110 (lim :: Int)
    i :: Int = 2
    prev = Dict ()
    prev2 = Dict ()
    prev3 = Dict ()
    prev4 = Dict ()
    divs = Dict ()
    while true
        prev4 = prev3
        prev3 = prev2
        prev2 = prev
        prev = divs
        divs = merge(divs,factor (i))
        cdivs = factdiop (divs)
        if cdivs > lim
            break
        end
        i += 1
    end
    println (factdiop (prev4))
    println (factnum (prev4))
    divs = prev4
    cdiv = divs
    i = 2
    while true
        divi = factor (i)
        for k in sort(collect (keys (divi))) 
            v = get (divi,k,0)
            vdiv = get (cdiv,k,0)
            cdiv = merge(cdiv, {k => (vdiv+v)})
            if factdiop (cdiv) > lim
                return factnum (cdiv)
            end
        end
        i += 1
    end
end

# Runtime 1.6ms
function sol116 (n :: Int)
    # i is the size (2..4)
    # blacks is the available black spots
    @memoize function ways (blacks :: Int, i :: Int)
        if blacks < i
            return 0
        elseif blacks == i
            return 1
        else
            res :: Int = 0
            for p = 0:(blacks-i)
                res += 1 + ways (blacks-(i+p),i)
            end
            return res
        end
    end
    # Sum for all the available sizes 2 to 4
    sum (map (x -> ways (n, x), 2:4))
end

# Runtime 1.6ms
function sol117 (n :: Int)
    # i is the size (2..4)
    # blacks is the available black spots
    @memoize function ways (blacks :: Int, i :: Int)
        if blacks < i
            return 0
        elseif blacks == i
            return 1
        else
            res :: Int = 0
            # iterate for different position and size of the next colored-blocks
            for p = 0:(blacks-i)
                res += 1 + sum (map (x -> ways (blacks-(i+p),x), 2:4))
            end
            return res
        end
    end
    # Sum for all the available sizes 2 to 4
    1 + sum (map (x -> ways (n, x), 2:4))
end

function sol119t (n :: Int)
    i :: Int = 10
    an :: Int = 0
    while true
        curi = sumdig (i)
        exp :: Int = 1
        while true
            tmp = curi^exp
            if tmp == 1
                break
            end
            
            if tmp > i
                break
            elseif tmp == i
                an += 1
                if an >= n
                    return i
                end
                break
            end
            exp += 1
        end
        i += 1
    end
end

# Search the powers instead of the numbers
function sol119 (n::Int, lim :: Int)
    res = Int []
    for i = 3:lim
        for j = 1:n
            tmp = i^j
            # Check whether sumdig^j equal to the number
            if (i == sumdig (tmp)) && (tmp > 9)
                push! (res,tmp)
            end
        end
    end
    return sort (res) [n]
end

function spec120 (a :: Int)
    res :: Int = 0
    rmax :: Int = 0
    i :: Int = 1
    asq = a*a
    while true
        tmp = rem (powermod (a-1,i,asq) + powermod (a+1,i,asq), asq)
        if res == tmp
            return rmax
        else
            if tmp > rmax
                rmax = tmp
            end
        end
        if i == 1
            res = tmp
        end
        i += 1
    end
    return rmax
end

function play120 (a :: Int)
    res = Int []
    asq = a*a
    for i = 1:a
        tmp = rem (powermod (a-1,i,asq) + powermod (a+1,i,asq), asq)
        push! (res, tmp)
    end
    return res
end

function sol120 (lim::Int)
    sum (map (spec120, 3:lim))
end

# Runs in 2sec
function sol124a (lim :: Int, nth :: Int)
    # array for sieve
    primes = ones (Int,lim)
    for i in 2:lim
        if primes [i] == 1
            for j in i:i:lim
                tmp = primes [j]
                primes [j] = tmp * i
            end
        end
    end
    bahan = map (x -> [(primes [x]),x], 1:lim)
    sort (bahan, by=first) [nth]
end

function sol124 (lim :: Int, nth :: Int)
    i :: Int = 2
    n :: Int = 1
    while true
        prevn :: Int = n
        tmp = factor (i)
        if all (x-> x == 1, values (tmp))
            tmpn = ifloor (log (i,lim))
            n += tmpn
            if n > nth
                println (n)
                return map (x -> i^x, 1:tmpn)
            elseif n == nth
                println (n)
                return i^tmpn
            end
        end
        i += 1
    end
end

function ispalin (n::Int)
    tmp = numcol (n)
    tmp == reverse (tmp)
end

function pyramid (n::Int)
    div (n*(n+1)*(2*n+1),6)
end

function sol125 (lim::Int)
    squares = map (pyramid,0:isqrt (lim))
    res = Set ()
    lsquare :: Int = length (squares)
    for i = 3:lsquare
        tmp  = squares [i]
        for j = i-2:-1:1
            suma = tmp - squares [j]
            if suma > lim
                break
            elseif ispalin (suma)
                push! (res,suma)
            end
        end
    end
    return sum (res)
end

function reversible (lim :: Int)
    if rem (lim,10) == 0
        return false
    else
        tmp = numcol (lim)
        rtmp = colnum (reverse (tmp))
        return all (isodd,numcol (rtmp+lim))
    end
end

function sol145 (start::Int,lim::Int)
    res :: Int = 0
    for i = start :lim
        if reversible (i)
            res += 1
        end
    end
    return res
end

function sol113(lim :: Int)
    function up (n :: Int, dig :: Int )
        if dig == 1
            return 1
        else
            return sum (map (x -> up (x,dig-1), collect (n:9)))
        end
    end

    function down (n::Int, dig ::Int)
        if dig==2
            if n==0
                return 1
            else
                return n
            end
        else
            return sum (map (x-> down (x, dig-1), collect (0:(n-1))))
        end
    end
    
    res :: Int = 9
    for i = 2:lim
        res = res + (sum (map (x-> up (x, i),1:9))) + (sum (map (x-> down (x,i), 1:9)))
    end
    return res
end


function sol123 (target :: Int, lim ::Int)
    # Generates the necessary primes
    prs = primes (lim)
    
    for i = 2:(length (prs))
        p = prs [i]
        psq :: Int = p*p

        # When the interesting number was hit, simply return the value
        # We're using modular-exponent to make it faster
        if rem(powermod (p-1,i,psq)+powermod (p+1,i,psq), psq) > target
            return [i,p]
        end
    end
end


function sol136 (lim::Int)
    refs = zeros (Int,lim-1)
    for a = 1:lim
        for b = (div (a,4)):a-1
            n = a*(4*b-a)
            if 0 < n < lim
                refs [n] += 1
            elseif n >= lim 
                break
            end
        end
    end
    length (filter (x-> x==10, refs))
end

function sol127 (lim::Int)
    prs = trues (lim)
    refs = ones (Int,lim)
    llim = isqrt (lim)
    for i = 2:llim
        if prs [i]
            for j = i*i:i:lim
                prs [j]= false
            end
        end
    end
    for i = 2:lim
        if prs [i]
            for j = i:i:lim
                refs [j] *= i
            end
        end
    end
    res :: Int = 0
    for a = 1:div (lim,2)
        rada :: Int = refs [a]
        for b = a+1:lim
            c = a+b
            if c >= lim
                break
            end
            if (gcd (a,b)==1)
                if (rada*refs [b]*refs [c]) < c
                    res += c
                end
            end
        end
    end
    return res
end

function sol139 (lim :: Int)
    refs = zeros (Int,lim)
    for m = 2:div (lim,2)
        msqr = m*m
        for n = 1:(m-1)
            nsqr = n*n
            a = msqr-nsqr
            b = 2*m*n
            c = msqr+nsqr
            peri = a+b+c
            if peri > lim
                break
            end
            if (iseven (m) || iseven (n)) && (gcd (m,n) == 1)
                tmp1 = 2*a*b
                tmp2 = isqrt (c*c-tmp1)
                if 0 == c % tmp2
                    for i = peri:peri:lim
                        refs [i] += 1
                    end
                end
            end
        end
    end
    sum (refs)
end

function maxi (refs)
    if isempty (refs)
        return 0
    else
        x = first (refs)
        nmax = maxi (rest (refs))
        if nmax > x
            return nmax
        else
            return x
        end
    end
end

function repdiv (n :: Int)
    if gcd (n,10) == 1
        x,k :: Int = 1,1
        while true
            if x == 0
                return k
            else
                x = (1+10*x) % n
                k += 1
            end
        end
    else
        return 0
    end
end

function sol130 (lim::Int)
    i :: Int = 3
    res :: Int = 0
    ctr :: Int = 0
    while true
        if ctr == lim
            return res
        else
            if !prime (i)
                an = repdiv (i)
                if an != 0
                    if 0 == (i-1) % an
                        ctr += 1
                        res += i
                    end
                end
            end
            i += 1
        end
    end
end

function sol129 (lim::Int)
    i :: Int = lim+1
    while true
        if repdiv (i) > lim
            return i
        else
            i += 2
        end
    end
end

function sol142tol ()
    i :: Int = 4
    solved = false
    while !solved
        a = i*i
        j :: Int = 3
        while j < i && !solved
            c = j * j
            f = a-c
            if f <= 0 || isqrt (f) == sqrt (f)
                j += 1
            else
                kstart = (j % 2 == 1) ? 1 : 2
                for k = kstart:2:(j-1)
                    d = k*k
                    e = a-d
                    b = c-e
                    if b <= 0 || e <= 0 
                        j += 1
                    elseif (isqrt (b) != sqrt (b)) || isqrt (e) != sqrt (e)
                        j += 1
                    else
                        x = div (d+c, 2)
                        y = div (e+f, 2)
                        z = div (c-d, 2)
                        return x+y+z
                    end
                end
            end
        end
        i += 1
    end
end

    































