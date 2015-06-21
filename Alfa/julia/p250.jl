using Memoize
using Lazy

include("common.jl")

function wall (ls)
    x = ls [1]
    if x == 3
        return [vcat([0],ls)]
    elseif x == 2
        return [vcat([0],ls)]
    elseif x == 1
        return []
    elseif x == 0
        return ls
    else
        mpa = wall (vcat ([x-2],ls))
        mpb = wall (vcat ([x-3],ls))
        if isempty (mpa)
            return mpb
        elseif isempty (mpb)
            return mpa
        else
            return collect (mpa,mpb)
        end
    end
end

function sol249 (lim :: Int)
    prs = sieve (lim)
    rprs = reverse (prs)
    refs = sieve (sum (prs))
    modi :: BigInt = ^(BigInt (10),16)
    @memoize function count (n)
        if n == 1
            return 0
        elseif n < 0
            return 0
        elseif n == 0
            return 1
        else
            res :: BigInt = 0
            for p in rprs
                r = n-p
                if r <= p
                    res += count (r)
                    if res > modi
                        res -= modi
                    end
                elseif r > p
                    res += count (r)
                    if res > modi
                        res -= modi
                    end
                end
            end
            return res
        end
    end
    resti :: BigInt = 0
    for p in refs
        resti += count (p)
        if resti > modi
            resti -= modi
        end
    end
    return resti
end






