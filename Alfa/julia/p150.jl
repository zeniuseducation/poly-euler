using Memoize
using Lazy

include ("common.jl")

@memoize function blocks (blacks :: Int)
    min_reds :: Int = 3
    if blacks == 0
        return 1
    elseif blacks < 0
        return 0
    elseif blacks == mind_reds
        return 1
    else
        res :: Int = 0
    end
end

function moretest
end

