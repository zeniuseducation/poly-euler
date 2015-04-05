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


function solamin (n :: Int)
    powers = map (x -> 2^x, range (0))
    bahan = takewhile (x -> x <= n, powers)
    raw = sort (apply (vcat,bahan,bahan))
    possi = filter(x -> sum (x) == n, distinct (apply (list, (apply (vcat,(partitions (raw)))))))
end

# The idea is to keep an array for the perimeter
# Loop a & b, and c 
# When a requirement is met then add 1 to the content for the given
# perimeter
# This is still very very very slow, need to upgrade
function sol75 (lim :: Int)
    # initialise the perimeter array
    peris = zeros (Int,lim)
    for a = 3:div (lim,4)
        asqr ::Int = a*a
        println (a)
        # setting the limit for traversing the b
        limb :: Int = div (lim,2) - a
        
        for b = (a+1):limb
            bsqr ::Int = b * b

            limc :: Int = lim-a-b
            for c :: Int = (b+1) : limc
                csqr = c*c
                if csqr == (asqr+bsqr)
                    peri :: Int = a+b+c

                    # Add one for this perimeter
                    peris [peri] += 1
                end
            end
        end
    end
    length ( filter(x -> peris [x] == 1, 3:lim))
end


# Runs in 28 ms

function sol55 (lim::Int)
    states = Int []
    @memoize function lychrel (n :: Int)
        if isempty (states)
            tmp = numcol (n)
            nn = colnum (reverse (tmp))
            emp = n + nn
            if ispalin (emp)
                states = Int []
                return false
            else
                push! (states,n)
                return lychrel (emp)
            end
        else
            if length (states) > 50
                shift! (states)
            end
            fstate = first (states)
            if fstate > lim
                states = Int[]
                return true
            else
                tmp = numcol (n)
                nn = colnum (reverse (tmp))
                emp = n + nn
                if ispalin (emp)
                    states = Int []
                    return false
                else
                    push! (states,n)
                    return lychrel (emp)
                end
            end
        end
    end
    res :: Int = 0
    for i = 1:lim
        if lychrel (i)
            res += 1
        end
    end
    return res
end


function sol65 (lim :: Int)
    # Generates the list of digits required
    bahan = apply (vcat, (map (x -> [1,x,1], 2:2:lim)))

    # The recursive function to calculate the continued fraction
    function contfract (n :: Int)
        if n == (lim - 1)
            return bahan [n]
        else
            return (bahan [n]) + (BigInt (1)//contfract (n+1))
        end
    end

    # Sum all of the digits
    return sumdig(num (2 + (BigInt (1)//contfract (1))))
end

function sol65a (lim :: Int)
    # Generates the list of digits required
    bahan = apply (vcat, (map (x -> [1,x,1], 2:2:lim)))

    # The recursive function to calculate the continued fraction
    function contfract (cur, n :: Int)
        if n == 0
            return 2 + (BigInt (1)//cur)
        else
            return contfract (bahan [n] + (BigInt (1)//cur), n-1)
        end
    end

    # Sum all of the digits
    return sumdig(num (contfract (bahan [lim],lim-2)))
end

# Still doesnt work!!
function sol79 ()
    tmp = apply (list,map (chop,open (readlines,"p79.txt")))
    pass = collect ( distinct(tmp))
    curr = ""
    curlen :: Int = length (pass)
    i :: Int = 0
    lcurr = curr
    while curlen > 20
        if curr == lcurr
            if i == 30
                i = 0
                pass = shuffle (collect (distinct (tmp)))
                curr = ""
                lcurr = ""
                curlen = length (pass)
            else
                i += 1
            end
        end
        cpass = first (pass)
        lcurr = curr
        if isempty (curr)
            curr = cpass
        else
            if any (x -> curr [x:(x+2)] == cpass, 1:(length (curr)-2))
                shift! (pass)
            elseif curr [1:2] == cpass [2:3]
                shift! (pass)
                curr = string (cpass [1], curr)
            elseif curr [1] == cpass [3]
                shift! (pass)
                curr = string (cpass [1:2], curr)
            elseif curr [end-1:end] == cpass [1:2]
                shift! (pass)
                curr = string (curr, cpass [3])
            elseif curr [end] == cpass [1]
                shift! (pass)
                curr = string (curr,cpass [2:3])
            else
                shift! (pass)
                push!(pass, cpass)
            end
        end
        println (curr)
        curlen = length (pass)
    end
    return curr
end


function sumdsqr (n :: Int)
    if n == 1
        return 0
    elseif n == 89
        return 1
    else
        tmp :: Int = sumdigsquare (n)
        return sumdsqr (tmp)
    end
end

function sol92 (lim :: Int)
    sum (map (sumdsqr,1:lim))
end

function sol74 (lim :: Int)
    refs = Int []
    res :: Int = 0
    for i = 1:lim
        ctr :: Int = 0
        curi :: Int = i
        check = false
        while !check
            if in (curi,refs)
                tmp = length (refs)
                if tmp == 60
                    res += 1
                    refs = Int []
                else
                    refs = Int []
                    check = true
                end
            else
                tmp = length (refs)
                if tmp > 60
                    refs = Int []
                    check = true
                else
                    push! (refs,curi)
                    curi = sumdifact (curi)
                end
            end
        end
    end
    return res
end

function square (x :: Int)
    x*x
end

function cube (x:: Int)
    x*square (x)
end

function quad (x::Int)
    square (x) * square (x)
end

# Runs in 0.3sec
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

    # Accumulate the ones that are true
    for i = 1:lim
        if refs [i]
            res += 1
        end
    end
    return res
end

function sol95a (lim::Int)
    refs = Int []
    function amics (i :: Int, n :: Int)
        if in (n,refs)
            refs = Int []
            return 0
        else
            tmp = pdivisors (n)
            if tmp > lim
                refs = Int []
                return 0
            else
                if i == tmp
                    refs = Int []
                    return 1
                else
                    push! (refs,n)
                    return 1 + amics (i,tmp)
                end
            end
        end
    end
    maxby (x -> amics (x,x) , 2:lim)
end

function sol95b (lim :: Int)
    # container for the current chain
    empty! (chain)

    visited = falses (lim)
    # container for keeping the last longest chain's smallest member
    longest :: Int = 0
    lchain :: Int = 0

    # we iterate for the whole number, but only do something for
    # unvisited i
    for i = 1:lim
        if !visited [i]
            push! (chain,i)
            curnum :: Int = pdivisors (i)
            if curnum > lim
                empty! (chain)
                break
            else
                check = false
                while !check
                    push! (chain, curnum)
                    visited [curnum] = true
                    if curnum == i
                        len :: Int = length (chain)
                        check = true
                        if len > longest
                            lchain = i
                            longest = len 
                        elseif (len == longest) && (i < lchain)
                            lchain = i
                        end
                        empty! (chain)
                    elseif in (curnum,chain)
                        idx :: Int = findfirst (x-> x==curnum, chain)
                        tempchain = chain [idx:end-1]
                        lenchain :: Int = length (tempchain)
                        lminima :: Int = curnum
                        if lenchain > 0
                            lminima = minimum (tempchain)
                        end
                        check = true 
                        if lenchain > longest 
                            lchain = lminima
                            longest = lenchain
                        elseif (lenchain == longest) && (lminima < lchain)
                            lchain = lminima
                            longest = lenchain
                        end
                        empty! (chain)
                    end
                    curnum = pdivisors (curnum)
                    if curnum > lim
                        empty! (chain)
                        check = true
                    end
                end
            end
        end
    end
    return longest
end


function sol95c (lim::Int)
    empty! (chain)
    longest :: Int = 0
    least_inchain :: Int = 0
    
    for i = 1 : lim
        curnum :: Int = pdivisors (i)
        if curnum < lim
            push! (chain,i)
            check = false
            while !check
                if curnum == i
                    len = length (chain)
                    if len > longest
                        least_inchain = i
                        longest = len
                    end
                    empty! (chain)
                    check = true 
                elseif in (curnum, chain)
                    empty! (chain)
                    check = true
                end
                push! (chain, curnum)
                curnum = pdivisors (curnum)
                if curnum > lim
                    empty! (chain)
                    check = true
                end
            end
        end
    end
    return least_inchain
end

# Runs in 178ms
function sol95(lim::Int)
    # divisors table
    divisors = ones (Int, lim)

    # keeping track of the ones already visited in previous chains
    visited = falses (lim)

    # keeping track of the numbers for the current chain
    chain = Int []

    # keeping track of the longest chain so far
    longest :: Int = 0
    least_inchain :: Int = 0

    # sieve for calculating divisors
    for i = 2:lim
        for j = (2*i):i:lim
            divisors [j] += i
        end
    end
    

    for i = 2 : lim
        if !visited [i]

            # start of the chain 
            curnum :: Int = divisors [i]

            # only interested with the one less than lim (1 mil)
            if curnum < lim
                push! (chain,i)
                check = false
                while !check

                    # when the chain back to its initial number
                    if curnum == i
                        for k in chain
                            visited [k] = true
                        end 
                        len = length (chain)
                        if len > longest
                            least_inchain = i
                            longest = len
                        end
                        empty! (chain)
                        check = true

                        # or if the chain back to a certain number
                        # already in chain
                    elseif in (curnum, chain)
                        empty! (chain)
                        check = true
                    end
                    push! (chain, curnum)
                    curnum = divisors [curnum]
                    if curnum > lim
                        empty! (chain)
                        check = true

                        # not really an interesting number if it's
                        # smaller than the initial number (i)
                    elseif curnum < i
                        empty! (chain)
                        check = true
                    end
                end
            end
        end
    end
    return least_inchain
end

function sol79 ()
    distinct (apply (list,map (chop,open (readlines,"p79.txt"))))
end

function sol57l (howmany :: Int)
    function confract (n :: Int, lim ::Int)
        if n == 0
            return 1 + (BigInt (1) // confract (n+1, lim)) 
        elseif n == lim
            return 2
        else
            return 2 + (BigInt (1) // confract (n+1, lim))
        end
    end
    
    function interesting (rat)
        denom = den (rat)
        numer = num (rat)
        return ceil (log10 (numer)) > ceil (log10 (denom))
    end
    
    length (filter (k-> interesting (k),map (x -> confract (0,x), 1:howmany)))
end


# Runs in 98ms bottom up approach
function sol57 (lim :: Int)
    # counter for the interesting expansion
    counter :: Int = 0

    # starting expansion (from the bottom up)
    current = 2
    
    for i = 1:lim

        # The current i-th expansion
        tmp = 1 + (BigInt (1) // current)

        # the part to check the denumerator and numerator for the
        # current term
        denom = den (tmp)
        numer = num (tmp)
        if ceil (log10 (numer))> ceil (log10 (denom))
            counter += 1
        end

        # update the current term for the next iteration
        current = 2 + (BigInt (1) // current)
    end
    return counter
end

# Still doesnt work!
function sol100 (target :: Int)
    blue :: Int = 85
    pblue :: Int = 15
    camp :: Int = 120
    pcamp :: Int = 21
    rat = blue/pblue
    while true
        pblue = blue
        blue = ifloor(rat * blue)
        blue2 :: Int = blue-1
        camp = ifloor(rat * camp) 
        camp2 :: Int = camp-1
        tmp = 0.0
        while true
            tmp = (blue//camp) * (blue2//camp2)
            if tmp < 1/2 
                break
            end
            camp += 1
            camp2 = camp-1
        end
        if (blue > target) && (tmp == 1/2)
            break
        end
        rat = blue/pblue
    end
    return blue
end

# TOLOL
function sol77a (lim::Int)
    @memoize function sumprime (amount::Int, n::Int)
        if amount == 0
            return 1
        elseif amount < 2
            return 0
        elseif amount == 3
            return 1
        elseif amount == 2
            return 1
        elseif amount == n
            return 1
        elseif n == 2
            return 1
        else
            res :: Int = 0
            i :: Int = 0
            imn :: Int = i * n
            while imn <= amount
                ttr :: Int = amount-imn
                if ttr < 2 && ttr != 0
                    return 0
                else
                    if ttr < 2 && ttr != 0
                        res += 1
                    else
                        res += sumprime (ttr,prevprime (ttr))
                    end
                    i += 1
                    imn = i*n
                end
            end
            return res
        end
    end

    j :: Int = 10
    sumprime (j,prevprime (j))
end            

# This runs in 2ms 
function sol77 (target::Int)
    # Calculate the number of partitions for a given amount with a
    # particular prime n
    @memoize function parti (amount :: Int, n :: Int)
        if amount == 0
            return 1
        elseif (amount < 0) || (amount == 1)
            return 0
        elseif n == 2
            if 0 == amount % 2
                return 1
            else
                return 0
            end
        else
            pp :: Int = prevprime (n)
            return sum (x -> parti (amount-(n*x), pp), 0:div (amount,n))
        end
    end
    n :: Int = 10

    # Iterate until finding the prime
    while true
        if parti (n, prevprime (n)) > target
            return n
        end
        n += 1
    end
end

function stupidprime (n :: Int, tar :: Int)
    tmp = numcol (n)
    ltmp = length (tmp)
    for i = 0:ltmp
        for j = i+1:ltmp+1
            for k = j+1:ltmp+2
                pmp = Int []
                for x = 0:9
                    if i==0
                        imp = vcat ([x], tmp)
                        jmp = vcat (imp [1:j],[x],imp [j+1:end])
                        kmp = vcat (jmp [1:k],[x],jmp [k+1:end])
                        jpmp = colnum (kmp)
                        if prime (jpmp) && jpmp >= (10^(ltmp+2))
                            push! (pmp, jpmp)
                            end
                    else
                        imp = vcat (tmp [1:i],[x],tmp [i+1:end])
                        jmp = vcat (imp [1:j],[x],imp [j+1:end])
                        kmp = vcat (jmp [1:k],[x],jmp [k+1:end])
                        jpmp = colnum (kmp)
                        if prime (jpmp) && jpmp >= (10^(ltmp+2))
                            push! (pmp, jpmp)
                        end
                    end
                end
                if length (pmp) >= tar
                    return [(length (pmp)),(sort (pmp))]
                end
            end
        end
    end
    return [0,123]
end

function sol51 (tar :: Int)
    i :: Int = 2
    while true
        xs = stupidprime (i,tar)
        if (xs [1] >= tar) 
            return xs
        end
        i += 1
    end
end

function playcard (st)
    st = split (st," ")
    return ((st [1:5]),(st [6:10]))
end

function sol54 ()
    bahan = map(chop,open (readlines, "poker.txt"))
end

function sol66 (lim::Int)
    refs = zeros (Int,lim)
    for i = 2:lim*20
        for j = 2:i-1
            isqr=i*i
            jsqr=j*j
            if isqr % jsqr == 1
                tmp = div (isqr,jsqr)
                if tmp <= lim 
                    if refs [tmp] == 0
                        refs [tmp] = i
                    end
                end
            end
        end
    end
    maxby (x->refs [x],1:lim)
end

