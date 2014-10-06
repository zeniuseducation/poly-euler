use "math1.sml";

(* prov 01 *)
val sol1 = (sum (srange 3 1000 3) + sum (srange 5 1000 5)) -
	   sum (srange 15 1000 15);

fun iterate f i lim =
    let val nexti = f i
    in
	if nexti > lim then []
	else nexti :: (iterate f nexti lim)
    end;

fun filter' f [] = []
  | filter' f (x::xs) =
    if f x then x :: (filter' f xs)
    else (filter' f xs);
    



