
defmodule E100 do
	import Range
	import Enum
	import List

	def sol1(lim) do
		base = new(1,lim-1);
		sum(filter(base, fn (x) -> 0 === rem(x,3) or 0 === rem(x,5) end))
	end

	def sol1_a(lim, i \\ 3, res \\ 0) do
		cond do
			i >=  lim -> res
			0 == rem(i,3) or 0 == rem(i,5) -> sol1_a(lim, i+1, res+i)
			true -> sol1_a(lim, i+1,res)
		end
	end

	def sol2(lim, a \\ 1, b \\ 1, res \\ 0) do
		cond do
			a > lim -> res
			0 === rem(a,2) -> sol2(lim, a+b, a, res+a)
			true -> sol2(lim,a+b, a, res)
		end
	end

	def odd_prime(p, i \\ 3) do
		cond do
			i*i > p -> true
			0 === rem(p,i) -> false
			true -> odd_prime(p, i+2)
		end
	end

	def divise(p,i) do
		if 0 === rem(p,i) do
			divise(div(p,i),i)
		else
			p
		end
	end

	def sol3(n, p \\ 3) do
		cond do
			n === 1 -> p - 2
			odd_prime(p) -> sol3(divise(n,p), p+2)
			true -> sol3(n,p+2)
		end
	end

	def sum_sieve(lim) do
		refs = take(Stream.cycle([true]), lim+1)
		llim = Float.ceil(:math.sqrt(lim))
		iter_j = fn(j, step) ->
			if j > lim do
				false
			else
				replace_at(refs,j,false);
				iter_j.(j+step, step)
			end
		end
		iter_i = fn(i, res) ->
			cond do
				i > lim -> res
				at(refs,i) -> if i <= llim do
												iter_j.(i*i, 2*i);
												iter_i.(i+2,i+res);
											else
												iter_i.(i+2, i+res)
											end
				true -> iter_i.(i+2, res)
			end
		end
		iter_i(3,2)
	end

	def timex(f, i) do
		:timer.tc fn -> f.(i) end
	end

end

defmodule Math do
	def expt(a,m) do
		cond do
			m === 0 -> 1
			m === 1 -> a
			0 === rem(m,2) -> next = expt(a,div(m,2));
										next * next;
			true -> next = expt(a,div(m,2));
								a * next * next;
		end
	end

	def odd_prime(p, i \\ 3) do
		cond do
			i * i > p -> true
			0 === rem(p,i) -> false
			true -> odd_prime(p, i+2)
		end
	end

	def nth(xs,i) do
		[head | tail] = xs
		if i === 0 do
			head
		else
			nth(tail,i-1)
		end
	end

	def checking (i) do
		likechecking = fn (x) -> i * x end
		likechecking.(i*i)
	end

	def coin_sums(amount, coin \\ 7) do
		coins = [1,2,5,10,20,50,100,200]
		cond do
			amount === 0 -> 1
			amount < 0 -> 0
			coin === 0 -> 1
			true -> cvalue = nth(coins,coin);
							Enum.sum(Enum.map(Range.new(0,div(amount,cvalue)), fn(x) -> coin_sums(amount-(cvalue*x), coin-1) end));
		end
	end

	def fibo(lim, a \\ 1, b \\ 0, res \\ 0) do
		cond do
			a > lim -> res
			0 === rem(a,2) -> fibo(lim,a+b,a,res+a)
			true -> fibo(lim,a+b,a,res)
		end
	end

	def sol25(lim, a \\ 1, b \\ 0, i \\ 1) do
		if a > lim do
			i
		else
			sol25(lim,a+b,a,i+1)
		end
	end

end
