using Memoize
using Lazy

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

