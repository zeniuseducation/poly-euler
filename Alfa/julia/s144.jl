using Memoize
using Lazy

include("common.jl")

function grad (x,y)
    (-4 * x)/y
end


