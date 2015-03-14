using Memoize

function lattice (n :: Int)
    if n == 1
        return 1
    else
        if iseven (n)
            return 1 + lattice (div (n,2))
        else
            return 1 + lattice (3*n+1)
        end
    end
end

function sol14 (lim :: Int)
    res :: Int = 0
    tmp :: Int = 0
    for i = 1:lim
        cur = lattice (i)
        if cur > res
            tmp = i
            res = cur
        end
    end
    return tmp
end

function readp18 ()
    tmp = map (chop, open (readlines,"p67.txt"))
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

function fibo (lim)
    a :: BigInt, b :: BigInt = 1,0
    i :: Int = 1
    while a < lim
        tmp = a
        a += b
        b = tmp
        i += 1
    end
    return i
end









            
    
