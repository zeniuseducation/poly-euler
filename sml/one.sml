open Math;
open List;

val sample = [1,2,3,4,5,6,7,8,9];

val inc = fn x => x + 1;
val dec = fn x => x - 1;

val floor = Real.floor;
val ceil = Real.ceil;
val round = Real.round;
val big = IntInf.toLarge;

fun is_even x = (0 = (x mod 2));

fun expt a m =
    if (m = 0) then 1 else a * big (expt a (dec m));

fun range (i:int) (j:int) =
    if i >= j then []
    else i :: (range (inc i) j);

fun sum nil = 0
  | sum (x::nil) = x 
  | sum (x::xs) = x + sum xs;

fun prod nil = 0
  | prod (x::nil) = x 
  | prod (x::xs) = x * prod xs;

fun srange (i:int) (j:int) (k:int) =
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

fun is_prime (p:int) =
    if p < 2 then false
    else if (p = 2) then true
    else if is_even p then false
    else
	let val lim = ceil (sqrt (real p))
	    fun prime_helper i =
		if i > lim then true
		else if (p mod (toLarge i)) = 0
		then false
		else prime_helper (i + 2)
	in
	    prime_helper (3)
	end;

fun primes_under x = filter is_prime (range 1 x);

fun fibo (i : int) =
    if i = 1 then 1
    else if i = 2 then 1
    else
	let
	    fun helper (n:int) ls =
		if n > i then ls
		else helper (inc n) [IntInf.toLarge ((hd ls) + (hd (tl ls))), hd ls]
	in
	    hd (helper 2 [1,1])
	end;

fun fibo_list 1 = [1]
  | fibo_list 2 = [1,1]
  | fibo_list (n : int) =
    let fun helper (i : int) res =
	    if i > n then res
	    else helper (inc i) ((IntInf.toLarge ((hd res) + (hd (tl res))))::res)
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
  | factors n =
    let val lim = ceil (sqrt (real n))
	fun helper i res =
	    if i > (fromLarge lim) then res
	    else let val rmod = n mod i
		     val rdiv = n div i
		     val rpair = n div rdiv
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

fun sqr x = x * x;

fun is_psquare x =
    let val xsqrt = sqrt (real x)
    in (ceil xsqrt) = (floor xsqrt)
    end;

fun next_prime 2 = 3
  | next_prime (n:int) =
    if n <= 2 then 2
    else if is_even n
    then
	if is_prime (inc n)
	then (inc n)
	else next_prime (inc n)
    else
	let
	    fun helper (i:int) =
		if is_prime i then i
		else helper (i + 2)
	in
	    helper (n + 2)
	end;

fun div' (a:int) (b:int) = 0 = (a mod b);

fun pfactors 2 = [2]
  | pfactors 3 = [3]
  | pfactors (n:int) =
    if n <= 1 then []
    else
	let val lim = ceil (sqrt (real n))
	    fun helper (i:int) (p:int) (res:int list) =
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














