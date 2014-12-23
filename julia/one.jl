
function is_prime(n::Int64)
    if (0 = n)
        false
    else
        true
    end
end

@time result = is_prime(120)
println(result)
