open List;
open Array;

val toLarge = Int.toLarge;
val fromInt = LargeInt.fromInt;
val fromLarge = LargeInt.fromLarge;
val toInt = LargeInt.toInt;

fun odd_prime (n: int) : bool =
    let fun loopi i =
	    if (i*i) > n
	    then true
	    else if (0 = (n mod i)) 
	    then false
	    else loopi (i+2)
    in loopi 3
    end;

fun sum_primes (lim : int) : LargeInt.int =
    let fun loopi i res =
	    if i > lim
	    then res
	    else if odd_prime i
	    then loopi (i+2) (res + (toLarge i))
	    else loopi (i+2) res
    in loopi 3 2
    end;

fun sum_sieves (lim:int) =
    let val primes = array (lim+1,true)
        fun outer (i:int) (res: LargeInt.int) =
            let fun inner (j:int) =
                    if j < lim
                    then (update (primes,j,false); inner (j + (2 * i)))
                    else 0
            in if i < lim
               then if (sub (primes,i))
                    then if (i*i) <= lim
                         then (inner (i*i) ;
                               outer (i + 2) (res + (toLarge i)))
                         else outer (i+2) (res + (toLarge i))
                    else outer (i+2) res
               else res
            end
    in outer 3 2
    end;

fun sol7 (tar : int) =
    let val lim = 12 * tar
	val primes = array (lim+1,true)
	fun outer (i:int) (idx:int) =
	    let fun inner (j:int)=
		    if j < lim
		    then (update (primes,j,false); inner (j+(2*i)))
		    else 0
	    in if idx >= tar
	       then i-2
	       else if (sub (primes,i))
	       then if i <= (lim div i)
		    then (inner (i*i);
			  outer (i+2) (idx+1))
		    else outer (i+2) (idx+1)
	       else outer (i+2) idx
	    end
    in outer 3 2
    end;
			     
				    

fun fibo_25 (tar : IntInf.int) : int =
    let fun loopi (a : IntInf.int) (b:IntInf.int) (i:int) =
	    if a > tar
	    then i
	    else loopi (a+b) a (i+1)
    in loopi 1 0 1
    end;

fun time f x =
    let val start = Time.toMicroseconds (Time.now())
	val result = f x
	val endi = Time.toMicroseconds (Time.now())
    in (print "Result : ";
	print (Int.toString result);
	print ("\nin ");
	print (LargeInt.toString (endi-start));
	print (" microseconds \n")) 
    end;

time sol7 10000;





