open Math;
open List;

val inc = fn x => x + 1;
val dec = fn x => x - 1;

fun is_even (x : IntInf.int) = (0 = (x mod 2));
fun even x = (0 = (x mod 2));

fun expt a 0 = 1
  | expt a m = (a * (expt a (dec m))) : IntInf.int;

fun sum' nil = 0
  | sum' (x::nil) = (x : IntInf.int)
  | sum' (x::xs) = (x + sum' xs) : IntInf.int;

fun fibo i =
    if i = 1 then 1
    else if i = 2 then 1
    else
	let
	    fun helper (n:int) ls =
		if n > i then ls
		else helper (inc n)
			    [IntInf.toLarge ((hd ls) +
					     (hd (tl ls))), hd ls]
	in
	    hd (helper 2 [1,1])
	end;

fun fibo_list 1 = [1]
  | fibo_list 2 = [1,1]
  | fibo_list (n : int) =
    let fun helper i res =
	    if i > n then res
	    else helper (inc i) ((IntInf.toLarge ((hd res) +
						  (hd (tl res))))::res)
    in
	rev (helper 2 [1,1])
    end;

fun fibo_under 1 = []
  | fibo_under 2 = [1,1]
  | fibo_under n =
    let fun helper res =
            if (hd res) > n then tl res
            else helper ((IntInf.toLarge ((hd res) + (hd (tl res))))::res)
    in
        helper [1,1]
    end;

fun append nil l2 = l2
  | append (x::nil) l2 = x :: l2
  | append (x::xs) l2 = x :: (append xs l2);

fun factors 1 = [1]
  | factors 2 = [1,2]
  | factors 6 = [1,2,3,6]
  | factors 12 = [1,2,3,4,6,12]
  | factors (n :IntInf.int) =
    let val lim = ceil (sqrt (real (Int.fromLarge n)))
	fun helper i res =
	    if i > lim then res
	    else let val rmod = (Int.fromLarge n) mod i
		     val rdiv = (Int.fromLarge n) div i
		     val rpair = (Int.fromLarge n) div rdiv
		 in
		     if 0 = rmod
		     then
			 if rpair=rdiv then rdiv :: res
			 else
			     if is_even n
			     then helper (inc i)  ([rdiv, rpair] @ res)
			     else helper (i+2) ([rdiv, rpair] @ res)
		     else
			 if is_even n
			 then helper (inc i) res
			 else helper (i+2) res
		 end
    in
	if is_even n
	then helper 2 [1,(Int.fromLarge n)]
	else helper 3 [1,(Int.fromLarge n)]
    end;

fun sqr x = x * x;

fun is_psquare x =
    let val xsqrt = sqrt (real x)
    in (ceil xsqrt) = (floor xsqrt)
    end;

fun is_prime (p:int) =
    if p < 2 then false
    else if (p = 2) then true
    else if even p then false
    else
        let val lim = ceil (sqrt (real p))
            fun prime_helper i =
                if i > lim then true
                else if (p mod i) = 0
                then false
                else prime_helper (i + 2)
        in
            prime_helper (3)
        end;

fun next_prime 2 = 3
  | next_prime n =
    if n <= 2 then 2
    else if even n
    then
	if is_prime (inc n)
	then inc n
	else next_prime n
    else
	let
	    fun helper i =
		if is_prime i then i
		else helper (i + 2)
	in
	    helper (n + 2)
	end;

fun suma_prima (x:int) =
(* returns the sum of primes under x *)
    let
	fun helper (i:int) (res:IntInf.int) =
	    if i > x then res
	    else helper (next_prime i) ((Int.toLarge i) + res)
    in
	helper 2 0
    end;

fun prima_lista (i:int) =
(* Returns the i-th first positive prime *)
    let
	fun helper (n : int) res =
	    if n >= i then res
	    else helper (inc n) (next_prime res)
    in
	helper 1 2
    end;


fun div' a b = 0 = (a mod b);

fun pfactors 2 = [2]
  | pfactors 3 = [3]
  | pfactors n =
    if n <= 1 then []
    else
	let val lim = ceil (sqrt (real n))
	    fun helper i p res =
		if i > lim then p :: res
		else if div' p i
		then
		    if is_prime (p div i)
		    then [i, (p div i)] @ res
		    else (helper 2 (p div i) (i :: res))
		else helper (next_prime i) p res
	in
	    helper 2 n []
	end;



