open List;

fun euler1 (n:int) =
    let fun loop (i:int) (res:int) =
	    if i < n
	    then if (0 = i mod 3)
		 then loop (i+1) (i+res)
		 else if (0 = i mod 5)
		 then loop (i+1) (i+res)
		 else loop (i+1) res
	    else res
    in loop 3 0
    end;

fun range i j k =
    let fun loopi n res =
	    if n > j
	    then res
	    else loopi (n+k) (res @ [n])
	fun loopj n res =
	    if n < j
	    then res
	    else loopj (n-k) (res @ [n])
    in if i > j then loopj i [] else loopi i []
    end;

fun even m = 0 = (m mod 2);

fun fibolist (lim : int) =
    let val start = [1,1]
	fun loop (m:int list) (res:int) =
	    if (hd m) > lim
	    then res
	    else if even (hd m)
	    then loop [(hd m) + (hd (tl m)), hd m] (res + (hd m))
	    else loop [(hd m) + (hd (tl m)), hd m] res
    in loop start 0
    end;
	
fun function x =
    let
        val t = Timer.startCPUTimer()
        val result = fibolist x;
    in
        print (Time.toString(#usr(Timer.checkCPUTimer(t))) ^ "\n");
        result
    end;
