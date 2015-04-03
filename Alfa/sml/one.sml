open List;
open Array;

fun prime' (n : int) =
    let fun loopi (i : int) =
	    if (i*i) > n
	    then true
	    else if 0 = (n mod i)
	    then false
	    else loopi (i+2)
    in loopi 3
    end;

fun sum_primes (lim : int) =
    let fun loopi (i : int) (res : int) =
	    if i > lim
	    then res
	    else if prime' i
	    then loopi (i+2) (res+i)
	    else loopi (i+2) res
    in loopi 3 2
    end;

fun sum_sieves (lim:int) =
    let val primes = array (lim+1,true)
        fun outer (i:int) (res: int) =
            let fun inner (j:int) =
                    if j < lim
                    then (update (primes,j,false); inner (j + (2 * i)))
                    else 0
            in if i < lim
               then if (sub (primes,i))
                    then if i <= (lim div i)
                         then (inner (i*i) ;
                               outer (i + 2) (res + i))
                         else outer (i+2) (res + i)
                    else outer (i+2) res
               else res
            end
    in outer 3 2
    end;			

fun time f x =
    let val start = Time.toMilliseconds (Time.now())
	val result = f x
	val endi = Time.toMilliseconds (Time.now())
    in (print (Int.toString result) ; endi-start)
    end;
		     

