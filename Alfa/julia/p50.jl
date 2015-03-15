using Memoize

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

function prime (n :: Int)
    lim :: Int = isqrt (n)
    i :: Int = 3
    while i <= lim
        if 0 == n % i
            return false
        else
            i += 2
        end
    end
    return true
end

function nextprime (n::Int)
    if prime (n+2)
        return n+2
    else return nextprime (n+2)
    end
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

@memoize function cdivs (n :: Int)
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

function numcol (n::BigInt)
    map (x -> int (x) - 48, collect ( string (n)))
end

function words (n :: Int)
    refs = ["one", "two", "three","four", "five","six","seven","eight","nine"]
    refp = ["", "twenty", "thirty","forty","fifty","sixty","seventy", "eighty", "ninety"]
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
        return refu[n]
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

function sumdig (n :: BigInt)
    res :: Int = 0
    i :: BigInt = n
    while i >= 10
        res += i % 10
        i = div (i,10)
    end
    return res+i
end

function fact (n::BigInt)
    n == 1 ? 1 : n * factorial (n-1)
end

function sol20 (n)
    sumdig (fact (BigInt (n)))
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

function getscore (nama :: String)
    refs = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    sum (map (x -> findfirst (refs,x), collect (nama)))
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
    for i = 12:lim
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










