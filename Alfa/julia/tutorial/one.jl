using Lazy
using Memoize

function sqr (x)
	return x*x
end

function odd_prime(n :: Int)
	i :: Int = 3
	lim :: Int = isqrt(n)
	while true
		if i > lim
			return true
		elseif 0 == rem(n,i)
			return false
		else
			i += 2
		end
	end
end

function sol1 (lim :: Int)
	res :: Int = 0
	for i = 1:lim-1
		if (0 == rem(i,3)) || (0 == rem (i,5))
			res += i
		end
	end
	return res
end

function sum_sieve (lim :: Int)
	llim :: Int, res :: Int = isqrt(lim), 0
	refs = trues(lim)
	for i = 3:2:lim
		if refs[i]
			if i <= llim
				for j = i*i:i*2:lim
					refs[j] = false
				end
			end
			res += i
		end
	end
	return res
end
