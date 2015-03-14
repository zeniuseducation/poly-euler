open List;

fun prime' (n : int64) =
    let fun loopi (i : int64) =
	    if (i*i) > n
	    then true
	    else if 0 = (n mod i)
	    then false
	    else loopi (i+2)
    in loopi 3
    end;

fun sum_primes (lim : int64) =
    let fun loopi (i : int64) (res : int64) =
	    if i > lim
	    then res
	    else if prime' i
	    then loopi (i+2) (res+i)
	    else loopi (i+2) res
    in loopi 3 2
    end;
			

fun time f x =
    let val start = Time.toMilliseconds (Time.now())
	val result = f x
	val endi = Time.toMilliseconds (Time.now())
    in (print (Int.toString result) ; endi-start)
    end;
		     

