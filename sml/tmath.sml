
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
	fun iter (i: int) (res : int) =
	    if sub (refs,i)
	    then res
	    else let val rems = (i*10) mod n
		 in if 0 = rems
		    then 0
		    else (update (refs,i,true); iter rems (1 + res))
		 end
    in iter 1 0
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

fun function x =
    let
        val t = Timer.startCPUTimer()
        val result = max_cycle x
    in
        print (Time.toString(#usr(Timer.checkCPUTimer(t))) ^ "\n");
        result
    end;
	    
