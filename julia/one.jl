function odd_prime (p::Int64)
  i::Int64 = 3
  while (i*i <= p )
    if 0 == p % i
      return false
    else
      i += 2
    end
  end
  return true
end

function sum_primes (lim::Int64)
  i::Int64 = 3
  res::Int64 = 2
  while i < lim
    if odd_prime(i)
      res += i
    end
    i += 2
  end
  return res
end

@time sum_primes(2000000)

function fibo (lim)
  a = 1
  b = 0
  i = 0
  tlim = 10
  for t = 1:lim
    tlim *= (BigInt::10)
  end
  while a < tlim
    tmp = a
    a = a + b
    b = tmp
    i += 1
  end
  return i
end

@time fibo(34)

function sum_sieves(n::Int64)
  isprime = ones(Bool, n)
  isprime[1] = false
  res = 2
  check = isqrt(n)
  for i = 3:2:n
      if isprime[i]
        res += i
        if i < check
          for j in (i*i):i*2:n
            isprime[j] = false
          end
        end
      end
    end
  return res
end

@time sum_sieves(2000000)

function euler7(n::Int64)
  sizen = n * 15
  isprime = ones(Bool, sizen)
  isprime[1] = false
  res = 1
  i = 1
  check = isqrt(sizen)
  while res < n
    if isprime[i]
      res += 1
      if i < check
        for j in (i*i):i*2:sizen
          isprime[j] = false
        end
      end
    end
    i += 2
  end
  return i-2
end

@time euler7(10001)

function euler1(lim)
  res = 0
  for i = 1:(lim-1)
    if (0 == i % 3) || (0 == i % 5)
      res += i
    end
  end
  return res
end

@time euler1(1000)

function euler2 (lim)
  res, a, b, tmp = 0, 1, 0, 0
  while a < lim
    if 0 == a % 2
      res += a
    end
    tmp = a
    a += b
    b = tmp
  end
  res
end

@time euler2(4000000)

function is_prime (p)
  if p == 2
    return true
  elseif iseven(p)
    return false
  else
    i = 3
    while i*i <= p
      if 0 == p % i
        return false
      else
        i += 2
      end
    end
  end
  return true
end

function euler3 (n::Int64)
  p::Int64, m::Int64 = 3 , n
  while !(odd_prime (m))
    while 0 != m % p
      p += 2
      while !(odd_prime(p))
        p += 2
      end
    end
    m = div(m,p)
    p = 3
  end
  return m
end

@time euler3(600851475143)

macro memoize(args...)
    if length(args) == 1
        dicttype = :(ObjectIdDict)
        ex = args[1]
    elseif length(args) == 2
        (dicttype, ex) = args
    else
        error("Memoize accepts at most two arguments")
    end

    if !isa(ex,Expr) || (ex.head != :function && ex.head != symbol("=")) ||
       isempty(ex.args) || ex.args[1].head != :call || isempty(ex.args[1].args)
        error("@memoize must be applied to a method definition")
    end
    f = ex.args[1].args[1]
    ex.args[1].args[1] = u = symbol(string(f,"_unmemoized"))

    args = ex.args[1].args[2:end]

    # Extract keywords from AST
    kws = Any[]
    vals = copy(args)
    if length(vals) > 0 && isa(vals[1], Expr) && vals[1].head == :parameters
        kws = shift!(vals).args
    end

    # Set up arguments for tuple to encode keywords
    tup = Array(Any, length(kws)+length(vals))
    i = 1
    for val in vals
        tup[i] = if isa(val, Expr)
                if val.head == :... || val.head == :kw
                    val.args[1]
                elseif val.head == :(::)
                    val
                else
                    error("@memoize did not understand method syntax $val")
                end
            else
                val
            end
        i += 1
    end

    for kw in kws
        if isa(kw, Expr) && (kw.head == :kw || kw.head == :...)
            tup[i] = kw.args[1]
        else
            error("@memoize did not understand method syntax")
        end
        i += 1
    end

    # Set up identity arguments to pass to unmemoized function
    identargs = Array(Any, (length(kws) > 0)+length(vals))
    i = (length(kws) > 0) + 1
    for val in vals
        if isa(val, Expr)
            if val.head == :kw
                val = val.args[1]
            end
            if isa(val, Expr) && val.head == :(::)
                val = val.args[1]
            end
        end
        identargs[i] = val
        i += 1
    end
    if length(kws) > 0
        identkws = map(kws) do kw
            if kw.head == :kw
                key = kw.args[1]
                if isa(key, Expr) && key.head == :(::)
                    key = key.args[1]
                end
                Expr(:kw, key, key)
            else
                kw
            end
        end
        identargs[1] = Expr(:parameters, identkws...)
    end

    # Generate function
    # This construction is bizarre, but it was the only thing I could devise
    # that passes the tests included with this package and also frees the cache
    # when a function is reloaded. Improvements are welcome.
    quote
        $(esc(ex))
        isdef = false
        try
            $(esc(f))
            isdef = true
        end
        if isdef
            for i = 1
                $(esc(quote
                    local fcache
                    const fcache = ($dicttype)()
                    $(f)($(args...),) =
                        haskey(fcache, ($(tup...),)) ? fcache[($(tup...),)] :
                        (fcache[($(tup...),)] = $(u)($(identargs...),))
                end))
            end
        else
            $(esc(quote
                const $(f) = let
                    local fcache, $f
                    const fcache = ($dicttype)()
                    $(f)($(args...),) =
                        haskey(fcache, ($(tup...),)) ? fcache[($(tup...),)] :
                        (fcache[($(tup...),)] = $(u)($(identargs...),))
                end
            end))
        end
    end
end


function num_palin (n)
  m, b, tmp = n, 0, 0
  while m > 0
    tmp = m
    m = div(m,10)
    b = (b*10) + (tmp % 10)
  end
  if b == n
    return true
  else
    return false
  end
end

@time num_palin(123242342342)

@memoize function euler4(a,b)
  tmp = a*b
  if num_palin(tmp)
    return tmp
  else
    if tmp < 600000
      return 0
    else
      return max(euler4(a,b-1), euler4(a-1,b))
    end
  end
end

@time euler4(999,999)

@time lcm(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)

function sum_squares (from,to)
  res = 0
  for i = from:to
    res += i * i
  end
  return res
end

function square_sum(from,to)
  n = (to-from) + 1
  tmp=(div(n,2)) * (to+from)
  tmp*tmp
end

function euler6 (from,to)
  return square_sum(from,to) - sum_squares(from,to)
end

@time euler6(1,100)

function nth_sieve(n)
  size = n*15
  isprime = ones(Bool, size)
  isprime[1] = false
  res = 0
  i = 1
  check = isqrt(size)
  while res < n
    if isprime[i] == true
      res++
      if i <= check
        for j = i*i:size:i
          isprime[j] = false
        end
      end
    end
    i += 2
  end
  return i
end

gcd(12,3)

@memoize is_pita(a,b,c) = c*c == a*a + b*b

































































