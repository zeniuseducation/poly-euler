using Memoize

include ("common.jl")

function sol1 (lim::Int)
    res::Int = 0
    for i = 1:(lim-1)
        if (0 == i % 3) || (0 == i % 5)
            res += i
        end
    end
    res
end

# 0.008msec

function sol2 (lim::Int)
    res = 0
    a,b,tmp = 2,1,0
    while a < lim
        if iseven (a)
            res += a
        end
        tmp = a
        a += b
        b = tmp
    end
    res
end

# 0.004 ms

function isprime (p::Int)
    if p == 2
        return true
    elseif iseven (p)
        return false
    else
        i = 3
        lim = isqrt (p)
        while i <= lim
            if p % i == 0
                return false
            end
            i += 2
        end
        return true
    end
end

function oddprime (p::Int)
    i::Int = 3
    while (i*i) <= p
        if p % i == 0
            return false
        end
        i += 2
    end
    return true
end


function sol3a (n::Int)
    function looper (p)
        if iseven (p)
            return looper (div (p,2))
        elseif oddprime (p)
            return p
        else
            a = 3
            while p % a != 0
                a += 2
                while !oddprime (a)
                    a += 2
                end
            end
            return looper (div (p,a))
        end
    end
    looper (n)
end

# 0.1ms

function nth_sieve (tar::Int)
    lim::Int=tar * 13
    llim::Int = isqrt (lim)
    refs = trues (lim+1)
    counter::Int = 1
    i::Int = 3
    while counter < tar
        if refs [i]
            if i < llim
                for j = i*i:i*2:lim
                    refs [j] = false
                end
            end
            i += 2
            counter += 1
        else i += 2
        end
    end
    return i-2
end


function sum_sieve (lim::Int)
    llim::Int = isqrt (lim)
    refs = trues (lim+1)
    res::Int = 2
    for i = 3:2:lim
        if refs [i]
            if i < llim
                for j = i*i:i*2:lim
                    refs [j] = false
                end
            end
            res += i
        end
    end
    return res
end

@memoize function cdiv (n::Int)
    lim::Int = isqrt (n)
    res::Int = 2
    if iseven (n)
        for i = 2:lim
            if i == lim
                if i*i == n
                    return res+1
                elseif rem (n,i)==0
                    return res+2
                else
                    return res
                end
            end
            if rem (n,i) == 0
                res += 2
            end 
        end
    else
        for i = 3:2:lim
            if i == lim
                if i*i == n
                    return res + 1
                elseif rem (n,i) == 0
                    return res + 2
                else
                    return res
                end
            else
                if rem (n,i) == 0
                    res += 2
                end
            end
        end
    end
    return res
end

function sol12 (tar::Int)
    i::Int = 10
    tmp::Int = *(cdiv (div (i,2)), cdiv (i+1)) 
    while tmp <= tar
        i += 1
        if iseven (i)
            tmp = (cdiv (div (i,2))) * (cdiv (i+1))
        else
            tmp = (cdiv (div (i+1,2))) * (cdiv (i))
        end
    end
    return div (i*(i+1),2)
end


function sol13 (fname)
    f = open (fname)
    h = readlines (f)
    cnt = length (h)
    res = BigInt (0)
    for i = 1:cnt
        str = h [i]
        res += BigInt (str [1:50])
    end
    tmp = length (string (res))
    return div (res, BigInt (10^(BigInt (tmp-10))))
end


function sol8(fname)
    f = open (fname)
    g = readlines (f)
    h = map ((x-> x [1:50]), g)
    i = map (x->map (y-> int (x [y:y]), 1:50),h)
    j = reduce (append!,i)
    k = map (i-> j [i:i+12], 1:988)
    return maximum(map (prod,k))
end

function sol67(fname)
    f = open (fname)
    g = readlines (f)
    return g
end


function idem (n::Int)
    i::Int = n-1
    while i != (i*i) % n
        i -= 1
    end
    return i
end

function solp (lim::Int)
    sum (pmap (idem,1:lim))
end

function soln (lim::Int)
    res::Int = 0
    for i = 1:lim
        res += idem (i)
    end
    return res
end

function collatz (n::Int)
    if n == 1
        return 1
    elseif iseven (n)
        return 1 + collatz (div (n,2))
    else return 1 + collatz (1+ 3*n)
    end
end

function maxby (f,coll)
    resi = 0
    res = 0
    tmp = 0
    for i in coll
        tmp = f (i)
        if tmp > res
            res = tmp
            resi = i
        end
    end
    return resi
end


function sol14 (lim::Int)
    res::Int = 0
    tmp::Int = 0
    resi::Int = 0
    for i = 1:lim
        tmp = collatz (i)
        if tmp > res
            res = tmp
            resi = i
        end
    end
    return resi
end

function sol14p (lim::Int)
    return maxby (first,pmap (x->(collatz (x), x), 1:lim))
end

function sol88(lim::Int)
    ires = IntSet ([])
    for i = 2:lim
        r::Int = i+1
        res = []
        while length (res) == 0
            r += 1
            res = filter (x->sum (x)==prod (x), collect (partitions (r,i)))
        end
        push! (ires,sum(first (res)))
    end
    return sum(ires)
end

function sol88a(start::Int,lim::Int)
    ires = Array (Array,0)
    for i = start:lim
        r::Int = i+1
        res = []
        while length (res) == 0
            r += 1
            res = filter (x->sum (x)==prod (x), collect (partitions (r,i)))
        end
        push! (ires,first (res))
    end
    return ires
end

function sol425 (lim)
    refs = Array (Array,lim)
    primes = trues (lim)
    llim = sqrt (lim)
    for i = 3:2:llim
        for j = i*i:i*2:lim
            primes [j] = false
        end
    end
end

function cube (n :: Int)
    n * n *n
end

function quad (n :: Int)
    n * cube (n)
end

# Runs in 0.15sec
function sol87 (lim::Int)
    # The array to mark the number reached by the loop
    refs = falses (lim)

    # Top loop for squares
    for i in primes (isqrt (lim))

        # The loop for cubes
        for j in primes (int (cbrt (lim)))

            # And the fourth
            for k in primes (int (sqrt (sqrt (lim))))
                tmp :: Int = square (i)+cube (j)+quad (k)
                if tmp < lim
                    refs [tmp] = true
                else
                    break
                end
            end
        end
    end
    res :: Int = 0
    sum (refs)
end



    
    


