using Memoize
using Lazy

include("Common.jl")

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

function sol5(lim::Int)
    res :: Int = 1
    faks :: Array{Int,1} = 1:20
    for i = 2:20
        p :: Int = faks[i]
        for j = 2*i:i:lim
            faks[j] = div(faks[j],p)
        end
        res *= p
    end
    res
end

function sol10(lim::Int)
    llim :: Int, res :: Int = isqrt(lim), 2
    hlim :: Int = 0 == rem(llim,2) ? llim+1 : llim + 2
    primes :: Array{Bool,1} = trues(lim)
    for i = 3:2:llim
        if primes[i]
            for j = i*i:2*i:lim
                primes[j] = false
            end
            res += i
        end
    end
    for i = hlim:2:lim
        if primes[i]
            res += i
        end
    end
    res
end

# runs in 2.5ms FIXME needs to put on github
function sol21(n::Int)
    lim :: Int = 3*n
    llim :: Int = isqrt(lim)
    faks :: Array{Int,1} = ones(Int,lim)
    for i = 2:llim
        isqr :: Int = i*i
        faks[isqr] += i
        for j = isqr+i:i:lim
            faks[j] += i + div(j,i)
        end
    end
    res :: Int = 0
    for i = 2:n
        itmp = faks[i]
        if faks[itmp] == i && i != itmp
            res += i
        end
    end
    res
end






































    #save
