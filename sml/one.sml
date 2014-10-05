open Math;
val sample = [1,2,3,4,5,6,7,8,9];

val inc = fn x => x + 1;
val dec = fn x => x - 1;

val floor = Real.floor;
val ceil = Real.ceil;
val round = Real.round;

fun is_even (x:int) = (0 = (x mod 2));

fun expt (a:int, m:int) =
    if (m = 0) then 1 else a * expt (a, dec m);

fun range (i:int) (j:int) =
    if i = j then []
    else i :: (range (inc i) j);

fun filter f nil = []
  | filter f (x::xs) =
    if f x then x :: (filter f xs)
    else (filter f xs);

fun is_prime (p:int) =
    if p < 2 then false
    else if (p = 2) then true
    else if is_even p then false
    else
	let val lim = ceil (sqrt (real p))
	    fun prime_helper (i:int) =
		if i > lim then true
		else if (p mod i) = 0
		then false
		else prime_helper (i + 2)
	in
	    prime_helper (3)
	end;

fun primes_under (x : int) = filter is_prime (range 1 x);

fun fibo (i : int) =
    if i = 1 then 1
    else if i = 2 then 1
    else
	let
	    fun fibo_helper (n:int, ls : int list) =
		if n > i then ls
		else fibo_helper (inc n, [(hd ls) + (hd (tl ls)), hd ls])
	in
	    hd (fibo_helper (2, [1,1]))
	end;

fun fibo_list 1 = [1]
  | fibo_list 2 = [1,1]
  | fibo_list (n : int) =
    let fun helper (i : int) (res: int list) =
	    if i > n then res
	    else helper (inc i) (((hd res) + (hd (tl res)))::res)
    in
	rev (helper 2 [1,1])
    end;

fun append nil l2 = l2
  | append (x::nil) l2 = x :: l2
  | append (x::xs) l2 = x :: (append xs l2);

fun factors 1 = [1]
  | factors 2 = [1,2]
  | factors 6 = [1,2,3,6]
  | factors 12 = [1,2,3,4,6,12]
  | factors (n:int) =
    let val lim = ceil (sqrt (real n))
	fun helper (i:int, res: int list) =
	    if i > lim then res
	    else let val rmod = n mod i
		     val rdiv = n div i
		     val rpair = n div rdiv
		 in
		     if 0 = rmod
		     then
			 if rpair=rdiv then rdiv :: res
			 else
			     if is_even n
			     then helper ( inc i, append [rdiv, rpair] res)
			     else helper ( i+2, append [rdiv, rpair] res)
		     else
			 if is_even n
			 then helper (inc i, res)
			 else helper (i+2, res)
		 end
    in
	if is_even n
	then helper (2, [1,n])
	else helper (3, [1,n])
    end;
	




