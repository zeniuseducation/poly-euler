using Memoize
using Lazy

include("common.jl")

function istricky (xs,b)
    xs == colnum (sort (numcol (b)))
end

# 76ms 
function sol52 (start :: Int)
    i :: Int = start
    tar :: Int = 6
    while true
        check = false
        j :: Int = 2
        itmp = colnum (sort (numcol (i)))
        while !check
            if ! istricky (itmp,i*j) 
                check = true
            elseif j == tar
                return i
            else j += 1
            end
        end
        i += 1
    end
end


function sol58l (lim :: Int, tar)
    llim :: Int = isqrt (lim)
    primes = trues (lim)
    cprimes :: Int = 0
    corners :: Int = 1
    for i = 3:2:lim
        if primes [i]
            if i <= llim
                for j = (i*i):(2*i):lim
                    primes [j] = false
                end
            end
        else
            tmp = sqrt (i)
            itmp ::Int = int (tmp)
            if tmp == itmp
                dtmp :: Int = itmp - 1
                cprimes += length (filter (x -> primes [x], [i-dtmp,i-(2*dtmp), i-(3*dtmp)]))
                corners += 4
                if (cprimes/corners) < tar 
                    return itmp
                end
            end
        end
    end
end

function sol58 (tar :: Real)
    cprimes :: Int = 0
    corners :: Int = 1
    i :: Int = 3
    while true
        isqr = i*i
        dtmp :: Int = i - 1
        cprimes += length (filter (x -> prime (x), [isqr-dtmp,isqr-(2*dtmp), isqr-(3*dtmp)]))
        corners += 4
        if (cprimes/corners) < tar 
            return [i,isqr]
        end
        i += 2
    end
end

function buildpascal (n :: Int , lim :: Int)
    res = 2
    ntmp = div((n+1),2)
    i :: Int = 1
    ads :: Int = (n+1) % 2
    while true
        if i >= ntmp
            return res + ads
        else
            tmp = binomial (n,i)
            if tmp <= lim
                res += 2
            else
                return res 
            end 
        end
        i += 1
    end
end 

function sol53 (n :: Int, lim :: Int)
    res :: Int = triangle (n+1) - 1
    for i = 1:n
        res -= buildpascal (i,lim)
    end
    return res 
end

function sol56 (lower :: Int , upper :: Int)
    res :: Int = 0
    for i= lower:upper
        for j = lower : upper
            tmp = sumdig (^(BigInt (i), j))
            if tmp > res
                res = tmp
            end
        end
    end
    return res
end


function sol56l (lim :: Int)
    res :: Int = 0
    i :: Int = lim
    while true 
        j :: Int = lim
        ilog = log10 (i)
        slin = 8 * ceil(j * ilog)
        if slin < res
            return res
        end 
        check = false
        while !check
            slin = 8 * ceil(j * ilog)
            if slin < res
                check = true
            else
                tmp = sumdig (^(BigInt (i),j))
                if tmp > res
                    res = tmp
                end
            end
            j -= 1
        end
        i -= 1
    end
end

function sol56f (lower :: Int, upper :: Int)
    reduce (max, [(sumdig (^(BigInt (i), j))) for i = lower:upper, j = lower:upper])
end

function sol59l ()
    bahan = map (int,split (chop (open (readall,"p59.txt")),","))
    start :: Int = 97
    lim :: Int = 97+25
    testcase = take (50,apply(list,bahan))
    for i = start:lim
        for j = start:lim
            for k = start:lim
                tmp = apply(string,
                            map (l -> char (l),
                                 map ((x,y) -> x $ y, testcase, cycle ([i,j,k]))))
                if contains (tmp," the ")
                    return (apply(string, map (char,[i,j,k])) , (sum (map ((x,y) -> x $ y, bahan, cycle ([i,j,k])))))
                end 
            end
        end
    end
end

function sol59 ()
    bahan = map (int,split (chop (open (readall,"p59.txt")),","))
    start :: Int = 97
    lim :: Int = 97+25
    testcase = take (100,apply(list,bahan))
    for i = start:lim
        for j = start:lim
            tmp = apply(string, map (l -> char (l), map ((x,y) -> x $ y, testcase, cycle ([i,j,start]))))
            if (contains (tmp, " a")) || (contains (tmp, "a "))
                for k = start:lim
                    tmp = apply(string, map (l -> char (l), map ((x,y) -> x $ y, testcase, cycle ([i,j,k]))))
                    if contains (tmp," the ")
                        return (apply(string, map (char,[i,j,k])) , (sum (map ((x,y) -> x $ y, bahan, cycle ([i,j,k])))))
                    end
                end
            end
        end
    end
end


function allprimes (x :: Int,xs)
    for i in xs
        tmp = numcol (i)
        check1 = colnum (vcat ([x],tmp))
        if prime (check1)
            check2 = colnum (vcat (tmp,[x]))
            if !prime (check2)
                return false
            end
        else
            return false
        end
    end
    return true
end

function sol60 (lim :: Int)
    i :: Int = 3
    xs = [3,7,109,673]
    while i < lim
        if allprimes (i,xs)
            return sum(vcat ([i],xs))
        end
        i = nextprime (i)
    end
end

function cube (n::Int)
    n*n*n
end

function cubperm (n::Int)
    tmp = sort (numcol (n*n*n))
    [(colnum (tmp)),(length (tmp))]
end

function sol62 (start::Int,lim::Int,n::Int)
    tmp = group_by (cubperm, range (start, lim))
    map (x -> map (cube,x), filter (x -> length (x) == n, collect (values (tmp))))
end

function sol62l (start::Int,n::Int)
    refs = Dict ()
    check = false
    i :: Int = start
    while !check
        tmp = cubperm (i)
        ci = cube (i)
        m = get!(refs,tmp,[ci])
        if m != [ci]
            if length (m) == (n-1)
                return push! (m,ci)
            end
            push! (m,ci)
            merge!(refs, {tmp => m})
        end
        i += 1
    end
end

function powerdigit (a::Int,m::Int)
    tmp = ^(BigInt (a),m)
    if tmp < (^(BigInt (10), m-1))
        return false
    else
        return true
    end
end

function ispowdig (a::Int,b::Int)
    ceil(b * log10 (a)) == b
end

function sol63 (lim::Int)
    res :: Int = 0
    for i = 2:lim
        j :: Int = 1
        while ispowdig (i,j)
            res += 1
            j += 1
        end
    end
    return res + 1 
end

function readp67 ()
    tmp = map (chop, open (readlines,"p67.txt"))
    tmp = map (x -> split (x, ' '), tmp)
    tmp = map (x -> map (int,x), tmp)
end

function sol67 ()
    tmp = reverse (readp67 ())
    literate = length (tmp) - 1
    for i = 1:literate
        for j = 1:(length (tmp [i+1]))
            resi = tmp [i]
            a = resi [j]
            b = resi [j+1]
            if a > b
                resa = tmp [i+1] [j]
                tmp [i+1] [j] = resa + a
            else
                resa = tmp [i+1] [j]
                tmp [i+1] [j] = resa + b
            end
        end
    end
    return first (last (tmp))
end

function sol69 (lim::Int)
    last (takewhile (x -> x < lim, (reductions (*, filter (prime,range ())))))
end

function sol71 (a:: Int, b::Int, lim :: Int)
    tmp = first (drop_while (x -> 0 != (x % b), iterate (x -> x-1, lim)))
    divs = div (tmp,b)
    m = (a*divs) - 1
    sort (map (x -> m/x, tmp:-1:tmp-10))
end

function totient (n :: Int, ps)
    tmp = prod (map (x -> x-1, ps))
    return div ((n* tmp), prod (ps))
end

function sol72 (lim :: Int)
    refs = collect(1:lim)
    primes = trues (lim)
    res :: Int = 0
    for i = 2:lim
        if primes [i]
            for j = (i*2):i:lim
                primes [j] = false
                refs [j] = (div (refs [j],i)) * (i-1)
            end
            res += (i-1)
        else
            res += refs [i]
        end
    end
    return res 
end

function sol73 (lim :: Int)
    primes = trues (lim)
    p1 = zeros (Int,lim)
    p2 = zeros (Int,lim)
    semiprime = trues(lim)
    ires :: Int = 0
    res  = 9999999999.0
    for i = 2:lim
        if primes [i]
            for j = (i*2):i:lim
                primes [j] = false
                tmp = p1 [j]
                if 0 == tmp
                    p1 [j] = i
                else
                    tmp2 = p2 [j]
                    if 0 == tmp2
                        p2 [j] = i
                    else
                        semiprime [j] = false
                    end
                end
            end
        else
            if i > 5000000
                tmp1 = p1 [i]
                tmp2 = p2 [i]
                if (semiprime [i]) && (tmp2 != 0)
                    tmp = (tmp1-1) * (tmp2-1)
                    if istwoperm (i,tmp)
                        tr = i/tmp
                        if tr < res
                            res = tr
                            ires = i
                        end
                    end
                end
            end
        end
    end
    return ires 
end

@memoize function intpartition (amount::Int, n::Int)
    if amount == 0
        return 1
    elseif n == 1
        return 1
    else 
        i :: Int = 0
        res :: Int = 0
        nextn :: Int = n-1
        while (i*n) <= amount
            res += intpartition(amount-(i*n), nextn)
            i += 1
        end
        return res 
    end
end

function sol76 (lim :: Int)
    intpartition (lim,lim-1)
end

function sol73 (lim::Int)
    res :: Int = 0
    for i = 5:lim
        d1 :: Int = div (i,3)
        lower = d1/i
        while lower <= 1/3
            d1 += 1
            lower = d1/i
        end
        d2 :: Int = 1 + div (i,2)
        upper = d2 / i
        while upper >= 1/2
            d2 -= 1
            upper = d2 / i
        end
        for j = d1:d2
            if gcd (i,j) == 1
                res += 1
            end
        end
    end
    return res
end

function mygcd (a::Int,b::Int)
    if a == 0
        return b
    elseif b==0
        return a
    elseif (a == 1)
        return 1
    elseif (b == 1)
        return 1
    elseif a > b
        return mygcd (a-b,b)
    else
        return mygcd (b-a,a)
    end
end

function countfractions (i :: Int)
    d1 :: Int = div (i,3)
    res :: Int = 0
    lower = d1/i
    while lower <= 1/3
        d1 += 1
        lower = d1/i
    end
    d2 :: Int = 1 + div (i,2)
    upper = d2 / i
    while upper >= 1/2
        d2 -= 1
        upper = d2 / i
    end
    for j = d1:d2
        if mygcd (i,j) == 1
            res += 1
        end
    end
    return res
end

function sol73p (lim :: Int)
    sum (pmap (countfractions,5:lim))
end
    

function sol81 ()
    refs = map(int,map (x -> split (x,","), (map (chop,open (readlines,"p81.txt")))))
    @memoize function findmin (a::Int, b::Int)
        if a == 80
            return sum (map (x -> refs [a] [x], b:80))
        elseif b == 80
            return sum (map (x -> refs [x] [80], a:80))
        else
            right :: Int = findmin (a,b+1)
            down :: Int = findmin (a+1,b)
            if right < down
                return right + refs [a] [b] 
            else
                return down + refs [a] [b] 
            end
        end
    end
    return findmin (1,1)
end

function modpartition (amount :: Int , n :: Int)
    if amount == 0
        return 1
    elseif n == 1
        return 1
    else 
        i :: Int = 0
        res :: Int = 0
        nextn :: Int = n-1
        while (i*n) <= amount
            res = res + ((modpartition ((amount-(i*n)), nextn)) % 1000000)
            i += 1
        end
        return res 
    end
end

function sol78(tar::Int)
    i :: Int = 1000
    while true
        if (intpartition (i,i-1) % tar) == 0
            return i
        end
        i += 1
    end
end

function sol82 ()
    refs = map(int,map (x -> split (x,","), (map (chop,open (readlines,"p82.txt")))))
    @memoize function findmin (a::Int, b::Int)
        if a == 80
            right  = findmin (a,b+1)
            up  = findmina (a-1,b)
            tmp = min (right,up)
            return tmp + refs [a] [b]
        elseif b == 80
            return refs [a] [b]
        elseif a == 1
            right = findmin (a,b+1)
            down  = findminb (a+1,b)
            tmp  = min (right,down)
            return tmp + refs [a] [b]
        else
            right = findmin (a,b+1)
            down  = findminb (a+1,b)
            up  = findmina (a-1,b)
            tmp = minimum ([right,down,up])
            return refs [a] [b] + tmp
        end
    end
    @memoize function findmina (a::Int, b::Int)
        if b == 80
            return refs [a] [b]
        elseif a == 1
            right = findmin (a,b+1)
            return right + refs [a] [b]
        else
            right = findmin (a,b+1)
            up = findmina (a-1,b)
            tmp = min(right,up)
            return refs [a] [b] + tmp
        end
    end
    @memoize function findminb (a::Int, b::Int)
        if a == 80
            return findmin (a,b+1) + refs [a] [b]
        elseif b == 80
            return refs [a] [b]
        else
            right = findmin (a,b+1)
            down  = findminb (a+1,b)
            tmp  = min(right,down)
            return refs [a] [b] + tmp
        end
    end
    return findminb (1,1)
end

function lock (a::Int, b :: Int)
    b - a + 1
end

function block (a::Int, b::Int)
    res :: Int = 0
    for i = 1:a
        for j = 1:b
            res += lock (i,a) * lock (j,b)
        end
    end
    return res
end

function minby (f , xs)
    res :: Int = 999999999
    ires  = first (xs)
    for i in xs
        tmp = f (i)
        if tmp < res
            res = tmp
            ires = i
        end
    end
    return ires
end

function block (a::Int, b::Int)
    res :: Int = 0
    for i = 1:a
        for j = 1:b
            res += lock (i,a) * lock (j,b)
        end
    end
    return res
end

function sol85 (lim::Int, upper::Int)
    refs = apply (vcat,[((abs (block (a,b) - lim)) , (a*b))
                        for a in 1:upper, b in 1:upper])
    minby (first,refs)
end

@memoize function modex (a::Int, b :: Int, m :: Int)
    if b == 0
        return 1
    else
        tmp = modex (a,div (b,2),m) % m
        if iseven (b)
            return (tmp * tmp) % m
        else
            return (tmp * tmp * a) % m
        end
    end
end

function sol99 ()
    refs = map (x -> map(int,split (x,",")), map (chop,open (readlines,"p99.txt")))
    res = 0
    ires :: Int = 0
    j :: Int = 1
    for i in refs
        (a,b) = i
        tmp = b * log (a)
        if tmp > res
            ires = j
            res = tmp
        end
        j+=1
    end
    ires
end

function soltest (lim::Int)
    res :: Int = 2
    for i = 3:2:lim
        if prime (i)
            res += i
        end
    end
    return res
end

function onesol (lim::Int)
    res :: Int = 0
    for i = 2:lim
        tmp = 2*i*i - 1
        if prime (tmp)
            res += 1
        end
    end
    return res 
end


