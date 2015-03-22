using Memoize
using Lazy

include("common.jl")

function sol216 (lim::Int)
    res :: Int = 0
    for i = 2:lim
        tmp = 2*i*i - 1
        if prime (tmp)
            res += 1
        end
    end
    return res 
end

