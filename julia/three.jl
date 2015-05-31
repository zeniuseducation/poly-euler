using Lazy, Memoize

function is_prime (x :: Int)
	if x < 2
		return false
	elseif x == 2
		return true
	elseif iseven(x)
		return false
	else
		lim :: Int = isqrt(x)
		for i = 3:2:lim
			if 0 == rem(x,i)
				return false
			end
		end
		return true
	end
end

function sum_sieve(lim::Int)
	llim :: Int = isqrt(lim)
	refs :: Array = trues(lim)
	res :: Int = 2
	for i = 3:2:lim
		if refs[i]
			if i <= llim
				for j = (i*i):2*i:lim
					refs[j] = false
				end
			end
			res += i
		end
	end
	return res
end
