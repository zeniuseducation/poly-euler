open List;
open Array;

fun pita (lim : int) =
    let fun outer (a : int) =
	    let fun inner (b : int) =
		    let val c = lim - (a + b)
		    in
			if c * c = a * a + b * b
			then a * b * c
			else if b > c
			then 0
			else inner (b + 1)
		    end
		val res = inner (a + 1)
	    in
		if res = 0
		then outer (a + 1)
		else res
	    end
    in outer 3
    end;

fun find_cycle (n : int) =
    let val refs = array (n+1,false)
	val refs2 = array (n+1, false)
	fun iter (i: int) (res : int) (res2:int) =
	    if sub (refs2,i)
	    then res2
	    else let val rems = (i*10) mod n
		 in if 0 = rems
		    then 0
		    else if sub (refs,i)
		    then (update (refs2,i,true); iter rems res (1 + res2))
		    else (update (refs,i,true); iter rems (1 + res) res2)
		 end
    in iter 1 0 0
    end;

fun max_cycle (lim : int) =
    let fun iter (i:int) (n:int) (res:int) =
	    if res > i
	    then [n,res]
	    else let val tmp = find_cycle i
		 in if tmp > res
		    then iter (i - 1) i tmp
		    else iter (i - 1) n res
		 end
    in iter lim lim 0
    end;

fun is_prime (n:int) =
    let fun inner (i:int) =
	    if i*i > n
	    then true
	    else if 0 = n mod i
	    then false
	    else inner (2 + i)
    in if n < 2
       then false
       else if n = 2
       then true
       else if 0 = n mod 2
       then false
       else inner 3
    end;

fun sieves (lim:int) =
    let val refs = array (lim+1,false)
	fun outer (i:int) (res: int list) =
	    let fun inner (j:int) =
		    if j < lim
		    then (update (refs,j,true); inner (j + (2 * i)))
		    else 0
	    in if i < lim
	       then if not (sub (refs,i))
		    then if (i*i) < lim
			 then (inner (i*i) ;
			       outer (i + 2) (res @ [i]))
			 else outer (i+2) (res @ [i])
		    else outer (i+2) res
	       else res
	    end
    in outer 3 [2]
    end;

fun range (i:int) (j:int) (k:int) =
    let fun inner (a:int) (res : int list) =
	    if a > j
	    then res
	    else inner (a + k) (res @ [a])
    in inner i []
    end;

fun euler27 (lim :int) =
    let val bs = sieves lim
	fun outer [] resb = resb
	  | outer (b::bbs) resb = 
	    let val lsa = filter
			      (fn x => x + b + 1 > 0)
			      (range (~ lim) lim 1)
		fun inner [] (cura : int) (res : int) = [res, cura, b]
		  | inner (a::aas) (cura : int) (res :int) = 
		    let fun sinner (n:int) (i:int) =
			    if is_prime ((n*n) + (a * n) + b)
			    then sinner (1+n) (1+i)
			    else i
			val resn = sinner 1 1
		    in if resn > res
		       then inner aas a resn
		       else inner aas cura res
		    end
	    in outer bbs ((inner lsa 0 0) :: resb)
	    end
    in outer bs []
    end;

fun sol27 [] (r::b::c::[]) = b * c
  | sol27 ((r::a::b::[])::xs) (ir::ia::ib::[]) =
    if r < ir
    then sol27 xs (ir::ia::ib::[])
    else sol27 xs (r::a::b::[])

fun pow (a:int) (b:int) = if b = 0 then 1 else a * (pow a (b-1));

fun sumfif (n:int) =
    let fun looper (i:int) (res:int) =
	    if i < 10
	    then res + (pow i 5)
	    else looper (i div 10) (res + (pow (i mod 10) 5))
    in looper n 0
    end;

    
fun euler30 (lim:int) =
    if lim = 1000
    then 0
    else let val sum5 = sumfif lim
	 in if sum5 = lim
	    then lim + (euler30 (lim-1))
	    else euler30 (lim-1)
	 end;

fun is_sumfif (n:int) =
    let fun looper (i:int) (res:int) =
            if i < 10
            then n = res + (pow i 5)
            else if res > n
	    then false
	    else looper (i div 10) (res + (pow (i mod 10) 5))
    in looper n 0
    end;

fun euler30b (lim:int) =
    let fun looper (i:int) (res:int) =
	    if i > lim
	    then res
	    else if is_sumfif i
	    then looper (1+i) (res + i)
	    else looper (1+i) res
    in looper 10 0
    end;

fun sum_coins (n:int) =
    let val cs = [1,2,5,10,20,50,100,200]
	fun sumas (i:int) (c:int) =
	    let fun inner (x:int) (res:int) =
		    if i < x * (nth (cs,c))
		    then res
		    else inner (1 + x)
			       (res + (sumas (i - (x * (nth (cs,c)))
					     ) (c-1)))
	    in if i = 0
	       then 1
	       else if c = 0
	       then 1
	       else inner 0 0
	    end
    in sumas n 7
    end;

fun sum_ints (n:int) =
    let fun sumas (i:int) (c:int) =
            let fun inner (x:int) (res:int) =
                    if i < x * c
                    then res
                    else inner (1 + x)
                               (res + (sumas (i - (x * c)) (c-1)))
            in if i = 0
               then 1
               else if c = 1
               then 1
               else inner 0 0
            end
    in sumas n (n-1)
    end;




fun function x =
    let
        val t = Timer.startCPUTimer()
        val result = sum_ints x
    in
        print (Time.toString(#usr(Timer.checkCPUTimer(t))) ^ "\n");
        result
    end;
	    







