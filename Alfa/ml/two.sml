open List;
open Array;

val toLarge = Int.toLarge;
val fromInt = LargeInt.fromInt;
val fromLarge = LargeInt.fromLarge;
val toInt = LargeInt.toInt;
val floor = Real.floor;
val ceil = Real.ceil;
val dir = "/users/questmac/Public/lambdas/poly-euler/Alfa/ml/";

fun range (i:int) (j:int) (step:int) =
    let fun loopi a res =
	    if a >= j
	    then rev res
	    else loopi (a+step) (a::res)
    in loopi i []
    end;

fun read file =
let val inStream = TextIO.openIn (dir ^ file)
in
    TextIO.inputAll inStream
end

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

fun sol10b (lim : int) =
	let val primes = array(lim+1,true)
		val llim = Real.ceil (Math.sqrt (Real.fromInt lim))
		val hlim = if 0 = llim mod 2 then llim + 1 else llim + 2
		fun outer (i:int) (res : Int64.int)=
			let fun inner (j:int) =
					if j < lim
					then (update (primes,j,false); inner (j + (2*i)))
					else ()
			in if i <= llim
			   then if (sub (primes,i))
			   		then (inner (i * i);
						  outer (i + 2) (res + (Int64.fromInt i)))
					else outer (i+2) res
			   else res
			end
		fun accum (i:int) (res:Int64.int) =
			if i <= lim
			then if (sub(primes,i))
				 then accum (i+2) (res + (Int64.fromInt i))
				 else accum (i+2) res
			else res
	in accum hlim (outer 3 2)
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

fun cdivs (n : int) : int =
    let val step = if 0 = n mod 2 then 1 else 2
	fun iter (i : int) (res : int) : int =
	    if i > n div i
	    then if i*i = n then res+1 else res
	    else if 0 = n mod i
	    then iter (i+step) (res+2)
	    else iter (i+step) res
    in if 0 = n mod 2
       then iter 2 2
       else iter 3 2
    end;

fun sum_pdivs (n : int) : int =
    let val k = if n mod 2 = 0 then 1 else 2
	val start = if n mod 2 = 0 then 2 else 3
	val lim = ceil (Math.sqrt (Real.fromInt n))
	fun iter i res =
	    if i >= lim then if i*i = n then res+i else res
	    else if n mod i = 0 then iter (i+k) (res + i + (n div i))
	    else iter (i+k) res
    in iter start 1
    end;

fun fact 0 = 1
  | fact 1 = 1
  | fact n = n * (fact (n-1));

fun remove elm lst =
    let fun iter [] res = rev res
	  | iter (x::xs) res = if x = elm then (rev res) @ xs
			       else iter xs (x::res)
    in iter lst []
    end;

fun colnum lst =
    let fun iter [] res = res
	  | iter (x::[]) res = (Int64.fromInt x) + (res * 10)
	  | iter (x::xs) res = iter xs ((Int64.fromInt x)+(res*10))
    in iter lst (Int64.fromInt 0)
    end;

fun sol24 (n : int) =
    let fun iter i [] res = rev res
	  | iter i (x::[]) res = rev (x::res)
	  | iter i raw res =
	    let val faks = fact ((List.length raw)-1)
		val divs = i div faks
		val elm = nth (raw, divs)
	    in iter (i mod faks) (remove elm raw) (elm::res)
	    end
    in colnum (iter n (range2 0 10) [])
    end;


fun sol23 (lim : int) : int =
    let val abun = array (lim+2, false)
	val sums = array (lim+2, false)
	fun init_abun (i : int) : unit =
	    if i > lim then ()
	    else let val isum = sum_pdivs i
		 in if isum > i then (update (abun, i, true);
				      init_abun(i+1))
		    else init_abun (i+1)
		 end
	fun init_sums (i: int) : unit =
	    let fun iterj (j:int) : unit =
		    let val n = i+j
		    in if n > lim then ()
		       else if sub(abun,j)
		       then (update (sums, n, true); iterj(j+1))
		       else iterj (j+1)
		    end
	    in if i > (lim div 2) then ()
	       else if sub(abun, i) then (iterj i ; init_sums (i+1))
	       else init_sums(i+1)
	    end
	fun iter (i : int) (res : int ) : int =
	    if i > lim then res
	    else if sub(sums, i) then iter (i+1) (res + i)
	    else iter (i+1) res
    in (init_abun 2 ;
	init_sums 2 ;
	(sum (range2 1 (lim+1))) - (iter 12 0))
    end;

fun is_amic (n:int) : bool =
    let val next = sum_pdivs n
    in if next = n then false
       else if n = (sum_pdivs next) then true
       else false
    end;

fun sol21 (lim:int) : int =
    let fun plus (x,y) = x+y
    in List.foldl plus 0 (List.filter is_amic (range2 2 (lim+1)))
    end;

fun sol21b (lim : int ) : int =
    let fun iter i res =
	    if i > lim then res
	    else if is_amic i then iter (i+1) (res+i)
	    else iter (i+1) res
    in iter 2 0
    end;

fun sol12 (tar : int) : Int64.int =
    let fun iter (n : int) =
	    if 0 = n mod 2
	    then if (cdivs (n div 2)) * (cdivs (n+1)) > tar
		 then (Int64.fromInt n) * (Int64.fromInt (n+1)) div 2
		 else iter (n+1)
	    else if (cdivs n) * (cdivs ((n+1) div 2)) > tar
            then (Int64.fromInt n) * (Int64.fromInt (n+1)) div 2
            else iter (n+1)
    in iter 12
    end;

fun sol14 (lim : Int64.int) : Int64.int =
    let fun next n = if 0 = n mod 2 then n div 2 else (3*n)+1
	fun collatz 1 = 1
	  | collatz n = 1 + collatz(next n)
	fun iter i res source =
	    if i > lim
	    then source
	    else let val nnext = collatz i
		 in if res > nnext
		    then iter (i+2) res source
		    else iter (i+2) nnext i
		 end
    in iter 500001 0 0
    end;

fun pascal (row : int) =
    let fun fsum (x : Int64.int ,y : Int64.int) : Int64.int = x+y
	fun iter i res =
	    if i = row
	    then res
	    else let val raw = ListPair.zip ((0::res) ,(res @ [0]))
		     val sumi = List.map fsum raw
		 in iter (i+1) sumi
		 end
    in iter 0 [1]
    end;

fun sol15 (lim : int) =
    let fun sqr x = x * x
	fun plus (x,y) = x+y
    in
	List.foldl plus 0 (List.map sqr (pascal lim))
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
timed sol10b 2000000 "#10b";
timed sol12 500 "#12";
timed sol15 20 "#15";
time sol21 10000 "#21";
time sol21b 10000 "#21b";
time sol23 28123 "#23";
timed sol24 999999 "#24";
time sol25 (IntInf.pow(10,999)) "#25";
