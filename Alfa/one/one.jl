function sum_sieves (lim::Int32)
    llim::Int32 = isqrt (lim)
    primes = trues (1+lim)
    res::Int32 = 2
    for i = 3:2:lim
        if primes [i]
            if i <= llim
                for j = i*i:2*i:lim
                    primes [j] = false
                end
            end
            res += i
        end
    end
    res
end


