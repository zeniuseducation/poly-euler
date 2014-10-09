open Math;
open List;

val inc = fn x => x + 1;
val dec = fn x => x - 1;

fun even' x = (0 = (x mod 2));

fun expt a 0 = 1
  | expt a m = a * (expt a (dec m)) 

fun range i j =
    if i >= j then []
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

fun primes_under x = filter prime' (range 1 x);

fun function x =
    let
        val t = Timer.startCPUTimer()
        val result = primes_under x
    in
        print (Time.toString(#usr(Timer.checkCPUTimer(t))) ^ "\n");
        result
    end;



