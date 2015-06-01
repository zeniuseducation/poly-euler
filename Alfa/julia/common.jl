using Memoize

function square (n:: Int)
    n * n
end

function prime (n :: Int)
    if n == 1
        return false
    elseif n==2
        return true
    elseif iseven (n)
        return false
    else
        lim :: Int = isqrt (n)
        i :: Int = 3
        while i <= lim
            if 0 == n % i
                return false
            else
                i += 2
            end
        end
    end
    return true
end

function nextprime (n::Int)
    if iseven (n)
        if prime (n+1)
            return n+1
        else
            return nextprime (n+1)
        end
    elseif prime (n+2)
        return n+2
    else
        return nextprime (n+2)
    end
end

function prevprime (n::Int)
    if n < 2
        return 2
    elseif n == 2
        return 2
    elseif n== 3
        return 2
    elseif iseven (n)
        if prime (n-1)
            return n-1
        else
            return prevprime (n-1)
        end
    elseif prime (n-2)
        return n-2
    else
        return prevprime (n-2)
    end
end


function cdivs (n :: Int)
    res :: Int = 2
    lim :: Int = isqrt (n)
    if iseven (n)
        for i = 2:lim
            if 0 == n % i
                if i == lim
                    res += 1
                else
                    res += 2
                end
            end
        end
    else
        for i = 3:2:lim
            if 0 == n % i
                if i == lim
                    res += 1
                else
                    res += 2
                end
            end
        end
    end
    return res
end

function maxby (f,xs)
    res = f (first (xs))
    ires = first (xs)
    for i in xs
        tmp = f (i)
        if tmp > res
            res = tmp
            ires = i
        end
    end
    return ires
end

function minby (f,xs)
    res = f (first (xs))
    ires = first (xs)
    for i in xs
        tmp = f (i)
        if tmp < res
            res = tmp
            ires = i
        end
    end
    return ires
end

function numcol (n::Int)
    map (x -> int (x) - 48, collect ( string (n)))
end

function sumdig (n :: BigInt)
    res :: Int = 0
    i :: BigInt = n
    while i >= 10
        res += i % 10
        i = div (i,10)
    end
    return res+i
end

function sumdig (n :: Int)
    res :: Int = 0
    i :: Int = n
    while i >= 10
        res += i % 10
        i = div (i,10)
    end
    return res+i
end


function sumdifact (n :: Int)
    res :: Int = 0
    i :: Int = n
    while i >= 10
        tmp = i % 10
        res += factorial (tmp)
        i = div (i,10)
    end
    return res+factorial (i)
end

function sumdigsquare (n :: Int)
    res :: Int = 0
    i :: Int = n
    while i >= 10
        tmp :: Int = i % 10
        res += tmp * tmp
        i = div (i,10)
    end
    return res+(i*i)
end


function pdivisors (n::Int)
    res :: Int = 1
    lim :: Int = isqrt (n)
    if iseven (n)
        for i = 2:lim
            if 0 == n % i
                if i*i == n
                    res += i
                else
                    res += i + div (n,i)
                end
            end
        end
    else
        for i = 3:2:lim
            if 0 == n % i
                if i*i == n
                    res += i
                else
                    res += i + div (n,i)
                end
            end
        end
    end
    return res
end


function colnum (xs)
    res :: Int = 0
    lxs :: Int = length (xs)
    for i in 1:lxs
        res = (res*10) + xs [i]
    end
    return res
end


function triangle (n :: Int)
    div (n*(n+1),2)
end

function tri_numbers (lim :: Int)
    map (triangle,1:lim)
end


function ispentagon (n :: Int)
    tmp = (sqrt (1+(24*n)) + 1) / 6
    tmp == int (tmp)
end

function pentagon (n :: Int)
    div (n * ((3*n) -1),2)
end

function ishexagon (n :: Int)
    tmp = (sqrt (1 + (8*n)) + 1) / 4
    tmp == int (tmp)
end

function ispalin (n :: Int)
    xs = numcol (n)
    return xs == reverse (xs)
end


function dpfactors (n::Int)
    p :: Int = n
    res = Int []
    if iseven (p)
        push! (res,2)
    end
    while iseven (p)
        p = div (p,2)
    end
    i :: Int = 3
    while (!prime (p)) && (p != 1)
        while 0 != (p % i)
            i = nextprime (i)
        end
        push! (res,i)
        while 0 == (p % i)
            p = div (p,i)
        end
    end
    if p == 1
        return res
    else
        return push! (res,p)
    end
end


function listequal (xs)
    fst = first (xs)
    for nxt in xs [2:end]
        if fst != nxt
            return false
        end
    end
    return true
end


function isperm (xs)
    listequal(map (x -> colnum(sort (numcol (x))), xs))
end

function istwoperm (a,b)
    colnum (sort (numcol (a))) == colnum (sort (numcol (b)))
end


function iscubic (n::Int)
    tmp = cbrt (n)
    int (n) == tmp
end

function cubperm (n::Int)
    tmp = sort (numcol (n*n*n))
    [(colnum (tmp)),(length (tmp))]
end

function ispandig (xs :: Array)
    m = length (xs)
    sort (xs) == [1:m]
end

function pandig9 (xs :: Array)
    sort (xs) == [1:9]
end

function permutes(xs, times :: Int)
    res = map (x -> [x], xs)
    for i  = 1:times-1
        tmpres = []
        for r in res
            tmpvcat = map (x -> vcat (r, [x]), xs)
            tmpres = vcat (tmpres, tmpvcat)
        end
        res = tmpres
    end
    return res
end

function hexagon (n :: Int)
    n * (2*n-1)
end

function heptagon (n :: Int)
    div (n * (5*n-3), 2)
end

function octagon (n :: Int)
    n * (3*n-2)
end

function sieve (lim :: Int)
    is_prime :: Array = trues (lim)
    llim :: Int = isqrt (lim)
    result :: Array = [2]
    for i = 3:2:lim
        if is_prime [i]
            if i <= llim
                for j = i*i:2*i:lim
                    is_prime [j] = false
                end
            end
            push! (result,i)
        end
    end
    return result
end

function sumprimes(lim :: Int)
    is_prime :: Array = trues (lim)
    llim :: Int = isqrt (lim)
    result :: Int = 2
    for i = 3:2:lim
        if is_prime [i]
            result += i
            if i <= llim
                for j = i*i:2*i:lim
                    is_prime [j] = false
                end
            end
        end
    end
    return result
end

