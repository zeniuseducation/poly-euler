
function is_prime(n::Int64)
    if (0 == n % 27)
        false
    else 
        true 
    end
end

@time print(is_prime(191))