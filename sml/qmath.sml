open Math
open List

type big = IntInf.int;

val fbig = Int.toLarge;
val inc = fn x => x + 1;
val dec = fn x => x - 1;

fun even' x = (0 = (x mod 2));

fun expt a 0 = 1
  | expt a m = a * (expt a (dec m)) 

fun range i j =
    if i >= j
    then []
    else i :: (range (inc i) j);

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
    else if even' p then false
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
	if even' p
	then if prime' (inc p)
	     then (inc p)
	     else nextPrime (inc p)
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

fun function x =
    let
        val t = Timer.startCPUTimer()
        val result = nth_prime x
    in
        print (Time.toString(#usr(Timer.checkCPUTimer(t))) ^ "\n");
        result
    end;


