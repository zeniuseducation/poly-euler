function square (x)
  x*x
end

square(12)

map(square, 1:10)

function sol1 (lim)
  kel3 = sum(3:3:lim)
  kel5 = sum(5:5:lim )
  kel15 = sum(15:15:lim)
  return kel3+kel5-kel15
end

sol1(1000)

function prime (n :: Int)
  lim :: Int = isqrt(n)
  function iter(i :: Int)
    if i > lim
      return true
    elseif 0 == rem(n,i)
      return false
    else
      return iter(i+2)
    end
  end
  iter(3)
end

@time 2+sum(filter(prime, 3:2:2000000))

function sumsieve (lim :: Int)
  primes = trues(lim)
  llim :: Int = isqrt(lim)
  res :: Int = 2
  for i = 3:2:lim
    if primes[i]
      if i < llim
        for j = i*i:i+i:lim
          primes[j] = false
        end
      end
      res += i
    end
  end
  return res
end

@time sumsieve(2000000)

m = [1:10]

m[1:3]
