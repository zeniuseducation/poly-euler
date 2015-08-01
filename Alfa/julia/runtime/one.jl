using Memoize
using Lazy

function sol1(lim :: Int)
    res :: Int = 0
    for i = 1:lim
        if 0 == rem (i,3) || 0 == rem (i,5)
            res += i
        end
    end
    return res
end

function sol1b(lim :: Int)
    sum (3:3:lim) + sum (5:5:lim) - sum (15:15:lim)
end

