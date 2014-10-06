use "smallint.sml";

(* prov 01 *)
val sol1 = (sum (srange 3 1000 3) + sum (srange 5 1000 5)) -
	   sum (srange 15 1000 15);

fun filter' f [] = []
    | filter' f (x::xs) =
      if f x then x :: (filter' f xs)
      else (filter' f xs);

fun sol2 x = sum' (filter' is_even (fibo_under x));

fun sol10 x = suma_prima x;

fun sol7 x = prima_lista x;

fun res x =
    let
        val t = Timer.startCPUTimer()
        val result = sol7 x
    in
	print (Time.toString(#usr(Timer.checkCPUTimer(t))) ^ "\n");
        result
    end;



