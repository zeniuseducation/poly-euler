open Math
open List

type big = IntInf.int;

val fint = Int.fromLarge;
val fbig = Int.toLarge;
val inc = fn x => x + 1;
val dec = fn x => x - 1;
val bdec = fn (x:big) => x - 1;

fun even (x: int) = (0 = (x mod 2));
fun even' (x : big) = (0 = (x mod 2));

fun expt a 0 = 1
  | expt a m = a * (expt a (dec m));

fun bexpt (a:big) 0 = 1
  | bexpt (a:big) m = (a * (bexpt a (bdec m)));

fun range i j =
    if i >= j
    then []
    else i :: (range (inc i) j);

fun brange i (j:big) =
    if i >= j
    then []
    else i :: (brange (binc i) j);

fun sum nil = 0
  | sum (x::nil) = x 
  | sum (x::xs) = x + sum xs;

fun prod nil = 0
  | prod (x::nil) = x
  | prod (x::xs) = x * prod xs;

fun srange i j k =
    if i > j
    then
        let fun bigger i =
                if i <= j then []
                else i :: (bigger (i+k))
        in bigger i
        end
    else
        let fun smaller i =
                if i >= j then []
                else i :: (smaller (i+k))
        in smaller i
        end;

fun prime' p =
    if p < 2 then false
    else if (p = 2) then true
    else if even p then false
    else
        let val lim = ceil (sqrt (real p))
            fun helper i =
                if i > lim then true
                else if (p mod i) = 0
                then false
                else helper (i + 2)
        in
            helper (3)
        end;

fun next_prime 1 = 2
  | next_prime 2 = 3
  | next_prime p =
    let fun helper n =
	    if prime' n then n else helper (n + 2)
    in
	if even p
	then if prime' (inc p)
	     then (inc p)
	     else next_prime (inc p)
	else helper (2 + p)
    end;

fun sum_primes x =
    let fun helper (i:int) (res : big) =
	    if i >= x
	    then res
	    else helper (next_prime i) ((fbig i) + res)
    in
	helper 3 (fbig 2)
    end;

fun nth_prime (i:int) =
    (* Returns the i-th first positive prime *)
    let
        fun helper (n : int) res =
            if n >= i then res
            else helper (inc n) (next_prime res)
    in
        helper 1 2
    end;

fun fibo i =
    if i = 1 then 1
    else if i = 2 then 1
    else
        let
            fun helper (n:int) (ls : big list) =
                if n > i then ls
                else helper (inc n)
                            [(hd ls) + (last ls),hd ls]
        in
            hd (helper 2 [1,1])
        end;

fun fibo_list 1 = [1]
  | fibo_list 2 = [1,1]
  | fibo_list (n : int) =
    let fun helper i (res : big list) =
            if i > n then res
            else helper (inc i) (((hd res)+(hd (tl res)))::res)
    in
        rev (helper 2 [1,1])
    end;

fun fibo_under 1 = []
  | fibo_under 2 = [1,1]
  | fibo_under n =
    let fun helper (res : big list) =
            if (hd res) > n then tl res
            else helper (((hd res) + (hd (tl res)))::res)
    in
        helper [1,1]
    end;

fun div' a m = 0 = a mod m;
fun bdiv' (a:big) (m:big) = 0 = a mod m;
				      
fun pfactors 2 = [2]
  | pfactors 3 = [3]
  | pfactors 1 = []
  | pfactors (n:big) =
    let fun helper p i res =
	    if prime' i
	    then i::res
	    else if div' i p
	    then helper 2 (i div p) (p::res)
	    else helper (next_prime p) i res
	fun helper' (i:big) p =
	    if (i mod (fbig p)) = 0
	    then (i div (fbig p))
	    else helper' i (next_prime p)
    in
	helper 2 (fint (helper' (helper' n 2) 2)) []
    end;

fun numcol (n : int) =
    let fun helper i res =
	    if i < 10
	    then i :: res
	    else helper (i div 10) ((i mod 10) :: res)
    in
	helper n []
    end;

fun palin' (n:int) =
    let val res = numcol n
    in
	res = rev res
    end;

fun max (a,m) = if a > m then a else m;

fun maxi (x::xs) = foldl max x xs;

fun palin_product [] ls = []
  | palin_product (x::xs) ls = (map (fn a => x * a) ls) @
			       palin_product xs ls;

fun sqr a = a * a;

fun pita (lim : int) =
    let fun helper a [] = helper (inc a) (range (inc a) (lim div 2))
	  | helper a (b::xs) = if (sqr a) + (sqr b) = (sqr (lim-b-a))
			       then a*b*(lim - b - a)
			       else helper a xs
    in
	helper 3 (range 4 (lim div 2))
    end;

fun triangle (n:int) = (n * (inc n)) div 2;

fun count_divs (n : int) =
    let fun feven i res =
	    if (i * i) > n
	    then res
	    else if (div' n i)
	    then if i = (n div i)
		 then (inc res)
		 else feven (inc i) (2 + res)
	    else feven (inc i) res
	fun fodd i res =
            if (i * i) > n
            then res
            else if (div' n i)
            then if i = (n div i)
                 then inc res
                 else fodd (2 + i) (2 + res)
            else fodd (2 + i) res
    in
	if even n then feven 3 4 else fodd 3 2
    end;

fun take_while f [] = []
  | take_while f (x::xs) = if f x then x :: (take_while f xs) else [];

fun drop_while f [] = []
  | drop_while f (x::xs) = if f x then (drop_while f xs) else xs;

fun first_triangle lim =
    let fun helper (i : int) =
	    if (count_divs (triangle i)) >= lim
	    then (triangle i)
	    else helper (inc i)
    in
	helper 100
    end;

fun binc (x:big) = x + 1;

fun collatz (lim : big) =
    let fun cal_col (n:big) = if even' n then n div 2 else binc(3*n)
    in
	let fun colls (i:big) (res : int) =
		if i = 1
		then inc res
		else colls (cal_col i) (inc res)
	    in
		let fun maxcol (m : big) (colmax : big*int) =
			if m >= lim
			then colmax
			else let val tmp = colls m 0
			     in
				 if tmp > (#2 colmax)
				 then (maxcol (binc m) (m, tmp)) 
				 else (maxcol (binc m) colmax) 
			     end
		in
		    maxcol 1 (1,1)
		end
	    end
    end;

fun sum_divs (n : int) =
    let fun feven i res =
	    if (i * i) > n
	    then res
	    else if (div' n i)
	    then if i = (n div i)
		 then (i + res)
		 else feven (inc i) (i + (n div i) + res)
	    else feven (inc i) res
	fun fodd i res =
            if (i * i) > n
            then res
            else if (div' n i)
            then if i = (n div i)
                 then i + res
                 else fodd (2 + i) (i + (n div i) + res)
            else fodd (2 + i) res
    in
	if even n then feven 3 (1+2+(n div 2)) else fodd 3 1
    end;

fun amic' n = 
    let val amicn = sum_divs n
    in
	if amicn = n
	then false
	else n = sum_divs amicn
    end;

fun sum_amic lim = sum (filter amic' (range 1 lim));

fun euler25 (lim:big) =
    let fun helper (x::y::ls) (i:int) =
	    if x >= lim
	    then i
	    else helper ((x+y)::x::y::ls) (inc i)
    in
	helper [1,1] 1
    end;

fun distinct ls =
    let fun helper lr [] = lr
	  | helper lr (x::xs) =
	    if exists (fn a => a = x) lr
	    then helper lr xs
	    else helper (x::lr) xs
    in
	helper [] ls
    end;

fun euler29 [] (ls : big list) = []
  | euler29 (x::xs) (ls:big list)
    = (map (fn (a:big) => bexpt x a) ls)
      @ (euler29 xs ls) ;

fun function x =
    let
        val t = Timer.startCPUTimer()
        val result = length (distinct (euler29 (brange 2 x) (brange 2 x)))
    in
        print (Time.toString(#usr(Timer.checkCPUTimer(t))) ^ "\n");
        result
    end;



