open Math;
open List;

type big = IntInf.int;

val fbig = IntInf.toLarge;
val inc = fn (x:big) => x + 1;
val dec = fn (x:big) => x - 1;

fun is_even (x : big) = (0 = (x mod 2));
fun div' a b = 0 = (a mod b);

fun expt a 0 = 1
  | expt a m = (a * (expt a (dec m))) : big;

fun sum' nil = 0
  | sum' (x::nil) = (x : big)
  | sum' (x::xs) = (x + sum' xs) : big;

fun fibo (i:big) =
    if i = 1 then 1
    else if i = 2 then 1
    else
	let
	    fun helper (n:big) ls =
		if n > i then ls
		else helper (inc n)
			    [fbig ((hd ls) + (hd (tl ls))), hd ls]
	in
	    hd (helper 2 [1,1])
	end;

fun fibo_list 1 = [1]
  | fibo_list 2 = [1,1]
  | fibo_list (n : big) =
    let fun helper (i:big) res =
	    if i > n then res
	    else helper (inc i) ((fbig ((hd res) + (hd (tl res))))::res)
    in
	rev (helper 2 [1,1])
    end;

fun fibo_under 1 = []
  | fibo_under 2 = [1,1]
  | fibo_under n =
    let fun helper res =
            if (hd res) > n then tl res
            else helper ((fbig ((hd res) + (hd (tl res))))::res)
    in
        helper [1,1]
    end;

fun append nil l2 = l2
  | append (x::nil) l2 = x :: l2
  | append (x::xs) l2 = x :: (append xs l2);

val divMod = IntInf.divMod;

fun sqr (x:big) = x * x;

fun factors 1 = [1]
  | factors 2 = [1,2]
  | factors 6 = [1,2,3,6]
  | factors 12 = [1,2,3,4,6,12]
  | factors (n : big) =
    let fun helper (i : big) res =
	    if (sqr i) > n then res
	    else let val tmp = divMod (n,i)
		     val rmod = (#2 tmp)
		     val rdiv = (#1 tmp)
		     val rpair = (#1 (divMod (n, rdiv)))
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
	then helper 2 [1,n]
	else helper 3 [1,n]
    end;



fun is_psquare x =
    let val xsqrt = sqrt (real x)
    in (ceil xsqrt) = (floor xsqrt)
    end;

fun is_prime (p:big) =
    if p < 2 then false
    else if (p = 2) then true
    else if is_even p then false
    else
        let fun prime_helper i =
                if (sqr i) > p then true
                else if (p mod i) = 0
                then false
                else prime_helper (i + 2)
        in
            prime_helper (3)
        end;

fun next_prime 2 = 3
  | next_prime n =
    if n <= 2 then 2
    else if is_even n
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

fun suma_prima (x:big) =
(* returns the sum of primes under x *)
    let
	fun helper (i:big) (res:big) =
	    if i > x then res
	    else helper (next_prime i) (i + res)
    in
	helper 2 0
    end;

fun prima_lista (i:big) =
(* Returns the i-th first positive prime *)
    let
	fun helper (n : big) res =
	    if n >= i then res
	    else helper (inc n) (next_prime res)
    in
	helper 1 2
    end;


 



