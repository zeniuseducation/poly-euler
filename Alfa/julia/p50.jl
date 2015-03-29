using Memoize

include ("common.jl")

function sol1 (lim :: Int)
    function isat (n :: Int)
        (0 == n % 3) || (0 == n % 5)
    end
    res :: Int = 0
    for i = 3:lim
        if isat (i)
            res += i
        end
    end
    return res 
end

function sol2 (lim :: Int)
    a :: Int,b:: Int,tmp::Int, res::Int = 2,1,2,0,0
    while a < lim
        if iseven (a)
            res += a
        end
        tmp = a
        a += b
        b = tmp
    end
    return res
end




function sol3 (tar :: Int)
    function loop (p :: Int, i :: Int)
        if prime (p)
            return p
        else
            while 0 != p % i
                i = nextprime (i)
            end
            r :: Int = p
            while 0 == r % i
                r = div (r,i)
            end
            return loop (r,i)
        end
    end
    loop (tar,3)
end

function fibo (lim :: BigInt,
               a :: BigInt = BigInt(1),
               b :: BigInt = BigInt(0),
               i :: Int = 1)
    if a > lim
        return i
    else
        return fibo (lim, a+b, a, i+1)
    end
end

function sol4 (upper::Int)
    function iskali (x :: Int)
        i :: Int = upper
        res = false
        while (!res) && (i >= 900)
            if 0 == x % i
                tmp :: Int = div (x,i)
                res = (tmp <= upper) && (tmp != i)
            end
            i -= 1
        end
        return res 
    end

    function cpalin (x :: Int )
        int (string (x, reverse (string (x))))
    end 
    
    resi :: Int = 0
    j :: Int = upper
    
    while resi == 0
        tmpl :: Int = cpalin (j)
        if iskali(tmpl)
            resi = tmpl
        end
        j -= 1
    end
    return resi
end


function sol5 (lim :: Int)
    res = [1]
    for i = 2:lim
        tmp = reverse (res)
        tres = i
        for j in tmp
            if 0 == i % j
                tres = div (tres,j)
            end
        end
        if tres != 0
            push! (res,tres)
        end
    end
    return prod (res)
end

function sol6 (lim :: Int)
    tmp = (div (lim,2)) * (lim+1)
    (tmp*tmp) - sum (map (x -> x * x, 1:lim))
end

function sol7 (tar::Int)
    lim :: Int = 11*tar
    llim :: Int = isqrt (lim)
    refs = trues (lim)
    ctr :: Int = 1
    i :: Int = 3
    res :: Int = 2
    while ctr < tar
        if refs [i]
            if i <= llim
                for j = (i*i):(i*2):lim
                    refs [j] = false
                end
            end
            ctr += 1
            res = i
        end
        i += 2
    end
    return res
end

function sol8 (lim :: Int)
    bahan = replace (open (readall, "p8.txt"), "\n", "")
    tmp = map (x -> int (x) - 48, collect (bahan))
    maximum (map (x -> prod (sub (tmp,x:x+12)), x:(lim-12)))
end

function sol9 (lim :: Int)
    a :: Int, b :: Int = 3,4
    c :: Int = lim-a-b
    res = true
    tres :: Int = a*b*c
    while res && (a < c)
        b = a+1
        c = lim - a - b
        while (c > b) && res
            if (a^2 + b^2) == c^2
                res = false
                tres = a*b*c
            end
            b += 1
            c = lim - a - b
        end
        a += 1
    end
    return tres
end

function sol10 (lim::Int)
    llim :: Int = isqrt (lim)
    refs = trues (lim)
    res :: Int = 2
    for i = 3:2:lim
        if refs [i]
            if i <= llim
                for j = (i*i):(i*2):lim
                    refs [j] = false
                end
            end
            res += i
        end
    end
    return res
end

function sol11 ()
    bahan = map (x -> int (split (x," ")),map (chop, open (readlines,"p11.txt")))
    bahan1 = transpose (bahan)
    left = maximum (map (i -> maximum (map (x -> prod (sub (bahan[i], x:x+3)), 1:17)), 1:20))
    right = maximum (map (i -> maximum (map (x -> prod (sub (bahan1[i], x:x+3)), 1:17)), 1:20))
end



function sol12 (tar :: Int)
    res = true
    n :: Int = 7
    tmp :: Int = 0
    while res
        tmp = div (n*(n+1), 2)
        if iseven (n)
            if (cdivs (div (n,2))) * (cdivs (n+1)) > tar
                return tmp
            end
        else
            if (cdivs (div (n+1,2))) * (cdivs (n)) > tar
                return tmp
            end
        end
        n += 1
    end
end

function sol13 ()
    int (apply (string ,(sub ( collect(string (sum (map (BigInt,map (chop,open (readlines,"p13.txt")))))), 1:10))))
end

function sol14 (lim :: Int)
    function twist (n :: Int)
        if n == 1
            return 1
        elseif iseven (n)
            return 1 + twist (div (n,2))
        else
            return 1 + twist (3*n + 1)
        end
    end
    tmp :: Int, itmp :: Int = 0,1
    for i = 2:lim
        cres = twist (i)
        if cres > tmp
            tmp = cres
            itmp = i
        end
    end
    return itmp
end




function sol14b (lim :: Int)
    gc_disable ()
    function twist (n :: Int)
        if n == 1
            return [1,1]
        elseif iseven (n)
            (_,r) = twist (div (n,2))
            return [n,1+r]
        else
            (_,r) = twist (3*n + 1)
            return [n,1+r]
        end
    end
    res = maxby (x -> x [2], pmap (twist,600001:2:lim))
    gc_enable ()
    return res 
end

function sol15 (n :: Int)
    res :: Int = 0
    for i = 0:n
        tmp :: Int = binomial (n,i)
        res += tmp*tmp
    end
    return res
end



function sol16 (n::Int)
    sumdig (^(BigInt (2), n))
end



function words (n :: Int)
    refs = ["one", "two", "three","four",
            "five","six","seven","eight","nine"]
    refp = ["", "twenty","thirty","forty","fifty","sixty","seventy", "eighty", "ninety"]
    refu = vcat (refs,["ten","eleven","twelve","thirteen", "fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"])
    if 100 <= n <= 999
        dep = div (n,100)
        bel = n % 100
        if bel == 0
            return (refs [dep]) * "hundred"
        else
            return (refs [dep]) * "hundredand" * (words (n % 100))
        end
    elseif 20 <= n <= 99
        dep = div (n,10)
        return (refp [dep]) * (words (n % 10))
    elseif 0 == n
        return ""
    elseif n == 1000
        return "onethousand"
    else
        return
        refu[n]
    end
end

function sol17 (lim::Int)
    sum (map (x -> length(words (x)),1:lim))
end

function readp18 ()
    tmp = map (chop, open (readlines,"p18.txt"))
    tmp = map (x -> split (x, ' '), tmp)
    tmp = map (x -> map (int,x), tmp)
end

function sol18 ()
    tmp = reverse (readp18 ())
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



@memoize function fact (n::Int)
    n == 0 ? 1 : n * factorial (n-1)
end

function sol20 (n)
    sumdig (fact (BigInt (n)))
end



function sol21 (lim::Int)
    function isamic (n::Int)
        tmp :: Int = pdivisors (n)
        if tmp > n
            stmp :: Int = pdivisors (tmp)
            return stmp == n ? (tmp+n) : 0
        else
            return 0
        end
        
    end

    res :: Int = 0
    for i = 1:lim
        res += isamic (i)
    end
    return res
end

function readp22 ()
    tmp = open (readall, "p22.txt")
    tmp = chop (tmp)
    tmp = split (tmp, ",")
    tmp = map (x -> reverse (chop (reverse (chop (x)))) , tmp)
    tmp = sort (tmp)
end



function sol22 ()
    tmp = readp22()
    res :: Int = 0
    for i = 1:length (tmp)
        res += i * getscore (tmp [i])
    end
    return res 
end

function sol22b ()
    tmp = readp22 ()
    sum (map (x-> findfirst (tmp,x) * getscore (x), tmp))
end

function sol23 (lim::Int)
    abuns = falses (lim)
    sumabuns = trues (lim)
    for i = 1:lim
        tmp = pdivisors (i)
        if tmp > i
            abuns [i] = true
        end
    end
    res :: Int = div(lim * (lim+1),2)
    for i = 12:div (lim,2) 
        if abuns [i]
            j :: Int = i 
            tres = (i+j) <= lim
            while tres
                if abuns [j]
                    if sumabuns [i+j]
                        res -= (i+j)
                        sumabuns [i+j] =false
                    end
                end
                j += 1
                tres = (i+j) <= lim
            end
        end
    end
    return res
end

function sol24(lim::Int)
    refs = reverse (map (factorial,0:9))
    digs = Set (0:9)
    res = Array (Int,0)
    t1 :: Int = lim
    for i in refs
        j :: Int = 1
        while (t1 - (j * i)) > 0
            j += 1
        end
        if t1 > i
            t1 -= (j - 1) * i
        end
        tmp = sort (collect (digs))
        push! (res, tmp [j])
        delete! (digs,tmp [j])
    end
    return apply (string,res)
end

function sol25 (n :: Int)
    lim = ^(BigInt (10), n)
    a :: BigInt, b::BigInt, i::Int = 1,0,1
    while a < lim
        tmp = a
        a += b
        b = tmp
        i += 1
    end
    return i
end



function sol26 (lim :: Int)
    i :: Int = prev_prime (lim+2)
    maxi :: Int = 0
    res :: Int = i
    refs = falses (i)
    while maxi < i
        tres ::Int = 1000 % i
        ctr :: Int = 1
        while ! refs [tres]
            refs [tres] = true
            tres = (10*tres) % i
            ctr += 1
        end
        if ctr > maxi
            maxi = ctr
            res = i
        end
        i = prev_prime (i)
        refs = falses (i)
    end
    return res
end

# 0.03ms
function sol28 (lim  :: Int)
    res :: Int = 1
    t1 :: Int = 1
    for i = 3:2:lim
        tmp :: Int = i*i
        res += sum (tmp:-(i-1):(t1+1))
        t1 = tmp
    end
    return res
end

function sol29 (lim)
    refs = Array (BigInt,0)
    for a in 2:lim
        for b in 2:lim
            push! (refs,^(BigInt (a),b))
        end
    end
    return length (Set (refs))
end

function sumdfive (n :: Int)
    i :: Int = n
    res :: Int = 0
    while i > 0
        res += (i % 10)^5
        i = div (i,10)
    end
    return res
end


@memoize function sdfive (n::Int)
    if n < 10
        return n^5
    else
        return (sdfive (div (n,10))) + (sdfive (n % 10))
    end
end

function sol30 (upper :: Int)
    res :: Int = 0
    for i = 100:upper
        if i == sumdfive (i)
            res += i
        end
    end
    return res
end


function sumdfact (n :: Int)
    i :: Int = n
    res :: Int = 0
    while i > 0
        res += factorial(i % 10)
        i = div (i,10)
    end
    return res
end


function sol34 (lim::Int)
    res :: Int = 0
    for i = 10:lim
        if i == sumdfact (i)
            res += i
        end
    end
    return res
end

# 13ms

cs = [200,100,50,20,10,5,2,1]

@memoize function coins (amount::Int, n::Int)
    coin = cs [n]
    if amount == 0
        return 1
    elseif n == 8
        return 1
    else 
        i :: Int = 0
        res :: Int = 0
        while (i*coin) <= amount
            res += coins (amount-(i*coin), n+1)
            i += 1
        end
        return res 
    end
end

function sol32 (lim :: Int)
    pandig = collect (1:9)
    res = Array (Int,0)
    llim :: Int = isqrt (lim)
    for i = 2:llim
        digi = digits (i)
        for j = i:lim
            digj = digits (j)
            tmp = vcat (digi,digj,digits (i*j))
            if length (tmp) == 9
                if all (x-> in (x,tmp), pandig)
                    push! (res,i*j)
                end
            end
        end
    end
    return sum (Set (res))
end




function cirprime (n::Int)
    dxs = digits (n)
    lxs = length (dxs)
    if prime (n)
        return all (prime, map (x -> colnum (circshift (dxs,x)), 1:(lxs-1)))
    else
        return false
    end
end

function cprime (n::Int)
    dxs = digits (n)
    lxs = length (dxs)
    res = prime (n)
    i :: Int = 1
    while res && (i < lxs)
        tp = colnum (circshift (dxs,i))
        res = prime (tp)
        i += 1
    end
    return res
end

function sol35 (lim :: Int)
    # 20 ms
    ctr :: Int = 4
    bahan = [1,3,7,9]
    tmp = map (x->[x], bahan)
    for i in 2:lim
        tmp = Set (collect ([(vcat (xs,[b])) for xs in tmp , b in bahan]))
        ctr += length (filter (x -> cprime (colnum (x)), tmp))
    end
    return ctr
end

function binpalin (n :: Int)
    tmp = bin (n)
    tmp == reverse (tmp)
end


function sol36 (lim :: Int)
    # 1.3ms
    res :: Int = 0
    for i = 1:lim
        td = dec (i)
        tr = reverse (td)
        t1 = int (td*tr)
        t2 = int ((chop (td)) * tr)
        if binpalin (t1)
            res += t1
        end
        if binpalin (t2)
            res += t2
        end
    end
    return res
end

function lprime (dn)
    res = isprime (colnum (dn))
    ln = length (dn)
    i :: Int = 2
    while res && (length (dn) >= 2)
        dn = dn [2:end]
        tn = colnum (dn)
        res = isprime (tn)
        i += 1
    end
    return res
end

function rprime (dn)
    res = isprime (colnum (dn))
    ln = length (dn)
    i :: Int = 1
    while res && (i < ln)
        dn = sub (dn, 1:(ln-i))
        tn = colnum (dn)
        res = isprime (tn)
        i += 1
    end
    return res
end

function ltprime (dn)
    isprime (colnum (dn))
end

function rtprime (dn)
    ln = length (dn)
    if ln == 1
        return isprime (colnum (dn))
    elseif isprime (colnum (dn))
        return rtprime (dn [1:(ln-1)])
    else
        return false
    end
end


function tprime (tar :: Int)
    res = [2,3,5,7]
    sumares = Array (Int,0)
    bahan = [1,3,7,9]
    while length (sumares) < tar
        res = filter (ltprime,collect ([(vcat ([b],xs)) for xs in res , b in bahan]))
        sumares = vcat (sumares, filter (rprime, res))
    end
    return sum (map (colnum, sumares))
end

function ispandig (xs)
    sort (xs) == [1,2,3,4,5,6,7,8,9]
end


function sol38 ()
    bahan = apply (vcat ,map ( x -> collect (permutations (x)),collect (combinations ([2,3,5,7],3))))
    lbahan = reverse (sort (map (x-> colnum (vcat (x,[9])), bahan)))
    res = false
    i :: Int = 1
    t1 :: Int, t2 ::Int = 0,0
    while !res
        t1 = lbahan [i]
        t2 = 2*t1
        res = ispandig (vcat (digits (t1), digits (t2)))
        i += 1
    end
    return string (t1,t2)
end

function digpast (n :: Int)
    if n < 10
        return n
    else
        m :: Int = 9
        i :: Int = 1
        t :: Int = n
        acum :: Int = 0
        while t > (i*m)
            t -= m*i
            acum += m
            i += 1
            m *= 10
        end
        tmp = acum + div (t+1,i)
        return (numcol (tmp)) [((t-1) % i)+1]
    end
end

function sol40 (lim :: Int)
    tmp :: Int = 1
    res = Array (Int,0)
    tres = Array (Int,0)
    for i in 1:lim
        push! (res, digpast (tmp))
        tmp *= 10
    end
    return prod (res) 
end

function sol41 ()
    bahan = reverse (sort (map (colnum, collect (permutations ([1,2,3,4,5,6,7])))))
    res = false
    i ::Int = 1
    while !res
        res = isprime (bahan [i])
        i += 1
    end
    return bahan [i-1]
end

function istriangle (n::Int)
    res = (sqrt (1+8*n) - 1) / 2
    res == int (res)
end

function getscore (word :: String)
    refs = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    res :: Int = 0
    for letter in word
        res += findfirst (refs,letter)
    end
    return res
end

file_content = split (replace (open (readall, "p42.txt"), "\"" , ""), ",")

function sol42 ()
    res :: Int = 0
    for word in file_content
        if istriangle(getscore (word))
            res += 1
        end
    end
    return res 
end

function sol42b ()
    res :: Int = 0
    lookup_triangle = falses (500)
    for i in tri_numbers (20)
        if i < 500
            lookup_triangle [i] = true
        end
    end
    for nama in file_content
        if lookup_triangle [(getscore (nama))]
            res += 1
        end
    end
    return res 
end


function sol43l ()
    refs = 0:9
    res = apply (vcat, (map (x -> collect(permutations (x)),collect (combinations (refs,3)))))
    i :: Int = 1
    for p in [2,3,5,7,11,13,17]
        res = filter (x -> ((colnum (x [i:end])) % p) == 0, res)
        next_res = Array []
        for r in res
            append! (next_res, [(vcat(r,[j])) for j in setdiff(refs,r)])
        end
        res = next_res
        i += 1
    end
    return sum (map (x -> colnum (vcat (last (x), x [1:end-1])), res))
end

function divbyprime (x :: Int, j :: Int, p::Int)
    tmp ::Int = div (x,j)
    (tmp > 9) && (tmp != 0) && (0 == (tmp % p))
end


function sol43 ()
    refs = 0:9
    res = filter (x -> x > 100,
                  map (colnum,
                       apply (vcat, (map (x -> collect(permutations (x)),
                                          collect (combinations (refs,3)))))))
    i :: Int = 1000
    j :: Int = 1
    for p in [17,13,11,7,5,3,2]
        res = filter (x-> divbyprime (x,j,p), res)
        nres = Int []
        for r in res
            append! (nres,[(m*i+r) for m in (setdiff (refs,digits (r)))])
        end
        res = nres
        i *= 10
        j *= 10
    end
    return sum (res)
end



function sol17b (lim::Int)
    refs = map (length,["one", "two", "three","four", "five","six","seven","eight","nine"])
    refp = map (length,["", "twenty", "thirty","forty","fifty","sixty","seventy", "eighty", "ninety"])
    refu = map (length, vcat(refs,["ten","eleven","twelve","thirteen", "fourteen","fifteen","sixteen","seventeen", "eighteen","nineteen"]))
    function cwords (n :: Int)
        
        if 100 <= n <= 999
            dep = div (n,100)
            bel = n % 100
            if bel == 0
                return refs [dep] + 7
            else
                return refs [dep] + 10 + cwords (n % 100)
            end
        elseif 20 <= n <= 99
            dep = div (n,10)
            return refp [dep] + cwords (n % 10)
        elseif 0 == n
            return 0
        elseif n == 1000
            return 11
        else
            return refu[n]
        end
    end
    
    sum (map (cwords,1:lim))
end

function sol17 (lim::Int) refs = ["one", "two", "three","four",
                                  "five","six","seven","eight","nine"]
    refp = ["", "twenty", "thirty","forty","fifty","sixty","seventy", "eighty", "ninety"]
    refu = vcat (refs,["ten","eleven","twelve","thirteen", "fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"])
    function words (n :: Int)
        if 100 <= n <= 999
            dep = div (n,100)
            bel = n % 100
            if bel == 0
                return (refs [dep]) * "hundred"
            else
                return (refs [dep]) * "hundredand" * (words (n % 100))
            end
        elseif 20 <= n <= 99
            dep = div (n,10)
            return (refp [dep]) * (words (n % 10))
        elseif 0 == n
            return ""
        elseif n == 1000
            return "onethousand"
        else
            return refu[n]
        end
    end
    sum (map (x -> length(words (x)),1:lim))
end




function sol44 ()
    res :: Int = 999999999
    checki = false
    i :: Int = 2
    while !checki
        j :: Int = i-1
        penti :: Int = pentagon (i)
        pentj :: Int = pentagon (j)
        sump = penti + pentj
        if sump > res
            checki = true
        end 
        checkj = false
        while (!checkj) && (j >= 1)
            if ispentagon (sump)
                diffp = penti-pentj
                if diffp > res
                    checkj = true
                else
                    if (ispentagon (diffp)) && (diffp < res)
                        res = diffp
                    end
                end
            end
            pentj = pentagon (j)
            sump = penti + pentj
            j -= 1
        end
        i += 1
    end
    return res
end



function sol45 (start :: Int)
    check = false
    i :: Int = start
    penta :: Int = 0
    while !check
        penta = pentagon (i)
        if ishexagon (penta)
            check = true
        end
        i += 1
    end
    return penta
end

# m3m3p4sm4l
function sol45fun (howmany :: Int)
    i :: Int = 1
    j :: Int = 0
    res = Int []
    penta :: Int = 0
    while j < howmany
        penta = pentagon (i)
        if ishexagon (penta)
            j += 1
            push!(res,penta)
        end
        i += 1
    end
    return res
end

function sol47 (tar :: Int, lim :: Int)
    llim :: Int = isqrt (lim)
    primes = trues (lim)
    m :: Int = 2
    check = false
    i :: Int = 2
    function nextsieve (n :: Int)
        im = n+2
        while ! primes[im]
            im += 2
        end
        return im
    end
    
    function pfactors (n::Int)
        p :: Int = n
        res = Int []    
        if iseven (p)
            push! (res,2)
        end
        while iseven (p)
            p = div (p,2)
        end
        imk :: Int = 3
        while (!primes [p]) && (p != 1)
            while 0 != (p % imk)
                imk = nextsieve (imk)
            end
            push! (res,i)
            while 0 == (p % imk)
                p = div (p,imk)
            end
        end
        if p == 1
            return res
        else
            return push! (res,p)
        end
    end
    while !check
        if primes [i]
            if i <= llim
                for j = (i*i):i:lim
                    primes [j] = false
                end
            end
            tmp :: Int = m+1
            while tmp <= (i-tar)
                if all (x -> length (pfactors (x)) == tar, tmp:(tmp+(tar-1)))
                    check = true
                    return tmp
                end
                tmp += 1
            end
            m = i
        end
        i += 1
    end
end

function sol48 (lim :: Int)
    tmp = 10^10
    res :: Int = 0
    for i = 1:lim
        res = (res+powermod (i,i,tmp)) % tmp
    end
    return res
end



function sol49 (diff :: Int, howmany :: Int)
    check = false
    i :: Int = 1001
    found :: Int = 1
    tmp = Int[]
    members = []
    while !check
        if (i+(2*diff)) > 9999
            return []
        end
        members = [i:diff:10000]
        if isperm (members)
            if all (prime,members)
                if found >= howmany 
                    check = true
                else
                    found += 1
                end
            end 
        end
        i = nextprime (i)
    end
    return apply (string,members)
end

function sol33 (lim :: Int)
    bahan = apply (vcat,[(a,b) for a in 10:lim , b in 10:lim])
    function satisfy (x)
        (a,b) = x
        if gcd (a,b) != 1 && (a % 10 != 0) && b < a
            na = numcol (a)
            nb = numcol (b)
            if any (x -> apply (==,x), apply (vcat ,[(i,j) for i in na, j in nb]))
                sda = setdiff (na,nb)
                sdb = setdiff (nb,na)
                if isempty (sda) || isempty (sdb)
                    return false
                else
                    return (a/b) == (first (sda))/(first (sdb))
                end
            else
                return false
            end
        else
            return false
        end
    end
    res = filter (satisfy,bahan)
    res1 = reduce(*,map (first,res))
    res2 = reduce(*,map (x -> first (reverse (x)), res))
    return div (res1,gcd (res1,res2))
end
        

function sol50 (lim :: Int)
    prs = primes (div (lim,100))
    reductions (rest,prs)
end

# This is a typical brute force with some simple filter of b (which
# must be primes), loop for a only when 1+a+b > 0

function sol27 (upper :: Int)
    res :: Int = 0
    maxpair :: Int = 0

    # ingredients for searching b
    bahanb = primes (upper) 
    for b in bahanb [2:end]
        check = true

        # loop the a starting from the largest odd number in the range
        a :: Int = upper-1
        while check
            checkinner = true
            ctr :: Int = 0
            n :: Int = 0
            while checkinner
                posp :: Int = n*n + n*a + b

                # exit when producing negative value
                if posp < 0
                    checkinner = false 
                elseif prime (posp)
                    ctr += 1
                else
                    checkinner = false
                end
                n += 1
            end
            if ctr > res
                res = ctr
                maxpair = a * b
            end
            a -= 2
            if a+b+1 < 3
                check = false
            end
        end
    end
    return maxpair
end

# The idea is to keep an array for the perimeter
# Loop a & b, and c 
# When a requirement is met then add 1 to the content for the given perimeter
function sol39 (lim :: Int)
    # initialise the perimeter array
    peris = zeros (Int,lim)
    for a = 3:div (lim,4)
        asqr ::Int = a*a

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
    maxby (x -> peris [x], 3:lim)
end

# Keep an array of odd numbers
# Iterate over prime numbers and squares
# Marks the one that can be represented as such
# After every checking for a certain prime then check whether there
# exist an odd composite that doesn't satisfy the requirement
function sol46 (lim::Int)
    odds = falses (3*lim)
    prms= primes(div (lim,2))
    squares = map (square,1:(isqrt (lim)))
    for p in prms
        for s in squares
            tmp = p + (2*s)
            odds [tmp] = true  
        end
        # This part is checking the odd numbers below p that doesn't
        # satisfy the requirements
        for i = 3:2:p
            if (odds [i] == false) && (!prime (i))
                return i
            end
        end
    end
end





