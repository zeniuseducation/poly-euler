using NumericExtensions

function trinumber (n :: Int)
    return div (n*(n+1),2)
end

function triads (lim :: Int)
    @functor1 Triad trinumber
    return map (Triad,1:lim)
end

