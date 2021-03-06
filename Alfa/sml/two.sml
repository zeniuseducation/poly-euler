open List;
open Array;

val toLarge = Int.toLarge;
val fromInt = LargeInt.fromInt;
val fromLarge = LargeInt.fromLarge;
val toInt = LargeInt.toInt;

fun range (i:int) (j:int) (step:int) =
    let fun loopi a res =
	    if a >= j
	    then rev res
	    else loopi (a+step) (a::res)
    in loopi i []
    end;

fun range2 (i:int)(j:int) = range i j 1;
fun range1 (i:int) = range 0 i 1;

fun sol1 (lim : int) =
    let val plus = (fn (a,b) => a+b)
	val sum3 = List.foldl plus 0 (range 3 1000 3)
	val sum5 = List.foldl plus 0 (range 5 1000 5)
	val sum15 = List.foldl plus 0 (range 15 1000 15)
    in sum3 + sum5 - sum15
    end;

fun sol1b (lim : int) =
    let fun loopi i res =
	    if i >= lim
	    then res
	    else if (0 = i mod 3) orelse (0 = i mod 5)
	    then loopi (i+1) (res+i)
	    else loopi (i+1) res
    in loopi 3 0
    end;

fun sol2 (lim : int) =
    let fun loopi a b res =
        if a > lim then res
        else if 0 = (a mod 2)
        then loopi (a+b) a (res+a)
        else loopi (a+b) a res
    in loopi 2 1 0
    end;

fun odd_prime (n: int) : bool =
    let fun loopi i =
	    if (i*i) > n
	    then true
	    else if (0 = (n mod i))
	    then false
	    else loopi (i+2)
    in loopi 3
    end;

fun sol3 (n : Int64.int) : int =
    let fun loopi (p : Int64.int) (i : int) =
	    if p = 0
	    then i-2
	    else if p = 1
	    then i-2
	    else if odd_prime i
	    then if 0 = (p mod (Int64.fromInt i))
		 then loopi (p div (Int64.fromInt i)) (i+2)
		 else loopi p (i+2)
	    else loopi p (i+2)
    in loopi n 3
    end;

fun numcol (i:int) : int list =
    let fun loopi n res =
	    if n < 10 then n::res
	    else loopi (n div 10) ((n mod 10)::res)
    in loopi i []
    end;

fun ispalin (n : int) : bool =
    let val xs = numcol n
    in xs = rev xs
    end;

fun helper4 (a : int) (b:int) =
    let fun outer i res =
	    let fun inner j =
		    let val mul = i * j
		    in if j < (i-a) then 0
		       else if mul < res then 0
		       else if ispalin mul then mul
		       else inner (j-1)
		    end
		val initmul = i*(i-1)
	    in if initmul < res then res
	       else if i < b then res
	       else let val resj = inner (i-1)
		    in if resj > res then outer (i-1) resj
		       else outer (i-1) res
		    end
	    end
    in outer 999 0
    end;

fun sol4 (i : int) = helper4 100 i;

fun sum lst = List.foldl (fn (x,y) => x + y) 0 lst

fun prod lst = List.foldl (fn (x,y) => x * y) 1 lst

fun sol5 n =
    let fun outer [] res = prod res
	  | outer (x::xs) res =
	    let fun inner [] resi = resi
		  | inner (xx::xxs) resi =
		    if 0 = (resi mod xx)
		    then inner xxs (resi div xx)
		    else inner xxs resi
	    in outer xs ((inner res x)::res)
	    end
    in outer (range 1 (n+1) 1) [1]
    end;

fun lcm lst =
    let fun outer [] res = prod res
          | outer (x::xs) res =
            let fun inner [] resi = resi
                  | inner (xx::xxs) resi =
                    if 0 = (resi mod xx)
                    then inner xxs (resi div xx)
                    else inner xxs resi
            in outer xs ((inner res x)::res)
            end
    in outer lst [1]
    end;

fun sol6 lim =
    let val squares = sum (List.map (fn x => x*x) (range2 1 (lim+1))) 
	val sums = sum (range2 1 (lim+1))
	val squaresums = sums * sums
    in squaresums - squares
    end;

fun sum_primes (lim : int) : Int64.int =
    let fun loopi i res =
	    if i > lim
	    then res
	    else if odd_prime i
	    then loopi (i+2) (res + (Int64.fromInt i))
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
                    then if i <= (lim div i)
                         then (inner (i*i) ;
                               outer (i + 2) (res + (toLarge i)))
                         else outer (i+2) (res + (toLarge i))
                    else outer (i+2) res
               else res
            end
    in outer 3 2
    end;

fun sol10 (lim:int) =
    let val primes = array (lim+1,true)
        fun outer (i:int) (res: Int64.int) =
            let fun inner (j:int) =
                    if j < lim
                    then (update (primes,j,false); inner (j + (2 * i)))
                    else 0
            in if i < lim
               then if (sub (primes,i))
                    then if i <= (lim div i)
                         then (inner (i*i) ;
                               outer (i + 2) (res + (Int64.fromInt i)))
                         else outer (i+2) (res + (Int64.fromInt i))
                    else outer (i+2) res
               else res
            end
    in outer 3 2
    end;

fun sol7b (tar : int) =
    let val lim = 12 * tar
	val primes = array (lim+1,true)
	fun outer (i:int) (idx:int) =
	    if (sub (primes,i))
	    then if idx = (tar-1)
		 then i
		 else if i <= (lim div i)
		 then let fun inner (j:int) =
			      if j <= lim
			      then (update (primes,j,false);
				    inner (j+(2*i)))
			      else 0
		      in (inner(i*i);outer (i+2) (idx+1))
		      end
		 else outer (i+2) (idx+1)
	    else outer (i+2) idx
    in outer 3 1
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

fun sol25 (tar : IntInf.int) : int =
    let fun loopi (a : IntInf.int) (b:IntInf.int) (i:int) =
	    if a > tar
	    then i
	    else loopi (a+b) a (i+1)
    in loopi 1 0 1
    end;

fun time f x st =
    let val start = Time.toMicroseconds (Time.now())
	val result = f x
	val endi = Time.toMicroseconds (Time.now())
    in (print "Result : ";
	print st;
	print " : ";
	print (Int.toString result);
	print ("\nin ");
	print (LargeInt.toString (endi-start));
	print (" microseconds \n"))
    end;

fun timex f x st =
    let val start = Time.toMicroseconds (Time.now())
        val result = f x
        val endi = Time.toMicroseconds (Time.now())
    in (print "Result : ";
	print st;
        print " : ";
        print (LargeInt.toString result);
        print ("\nin ");
        print (LargeInt.toString (endi-start));
        print (" microseconds \n"))
    end;

fun timed f x st =
    let val start = Time.toMicroseconds (Time.now())
        val result = f x
        val endi = Time.toMicroseconds (Time.now())
    in (print "Result : ";
	print st;
        print " : ";
        print (Int64.toString result);
        print ("\nin ");
        print (LargeInt.toString (endi-start));
        print (" microseconds \n"))
    end;

time sol1 1000 "#1";
time sol1b 1000 "#1";
time sol2 4000000 "#2";
time sol3 600851475143 "#3";
time sol4 800 "#4";
time sol5 20 "#5";
time sol6 100 "#6";
time sol7 10000 "#7";
time sol7b 10001 "#7b";
timed sol10 2000000 "#10";
time sol25 (IntInf.pow(10,999)) "#25";

