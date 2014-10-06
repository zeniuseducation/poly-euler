open Math;
open List;

val inc = fn x => x + 1;
val dec = fn x => x - 1;

fun is_even x = (0 = (x mod 2));

fun expt a m = if (m = 0) 
	       then 1
	       else a * (expt a (dec m));

fun range i j =
    if i >= j then []
    else i :: (range (inc i) j);

fun sum nil = 0
  | sum (x::nil) = x 
  | sum (x::xs) = x + sum xs;

fun prod nil = 0
  | prod (x::nil) = x | prod (x::xs) = x * prod xs;

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

fun is_prime p =
    if p < 2 then false
    else if (p = 2) then true
    else if is_even p then false
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

fun primes_under x = filter is_prime (range 1 x);



