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




