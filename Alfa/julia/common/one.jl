using Lazy
using Memoize

function sieve (lim :: Int)
    refs = trues (lim)
    llim :: Int = isqrt (lim)
    res :: Array = Int [2]
    for i = 3:2:lim
        if refs [i]
            if i <= llim
                for j = i*i:2*i:lim
                    refs [j] = false
                end
            end
            push! (res,i)
        end
    end
    res
end

function sumsieve (lim :: Int)
    refs = trues (lim)
    res :: Int = 2
    llim :: Int = isqrt (lim)
    for i = 3:2:lim
        if refs [i]
            if i <= llim
                for j = i*i:2*i:lim
                    refs [j] = false
                end
            end
            res += i
        end
    end
    res
end

function pdivisors (lim::Int)
    refs = fill (Int [1],lim)
    llim :: Int = isqrt (lim)
    for i = 2:llim
        isqr = i*i
        for j = i*i:i:lim
            if j == isqr
                refs [j] = vcat (refs [j],[i])
            else
                refs [j] = vcat (refs [j],[i,(div (j,i))])
            end
        end
    end
    map (sort,refs)
end

function sumdivisors (lim::Int)
    refs = ones(Int,lim)
    llim :: Int = isqrt (lim)
    for i = 2:llim
        isqr = i*i
        for j = i*i:i:lim
            if j == isqr
                refs [j] += i
            else
                refs [j] += i + div (j,i)
            end
        end
    end
    refs
end




