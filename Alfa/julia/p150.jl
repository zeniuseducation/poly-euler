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


function sol102()
    map(x -> map(int,split (x,",")), map (chop,open (readlines,"p102.txt")))
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
    


function sol110 (lim :: Int)
    function satisfy (st)
        tmp = caldiop (st)
        if tmp > lim
            return numdiop(st)
        else
            nextst = create_nexts (st)
            return minimum(map (satisfy, nextst))
        end
    end
    return satisfy ({3 => 1})
end








