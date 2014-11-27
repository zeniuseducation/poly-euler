open List;

type int64 = Int64.int;

fun maksud (x:int64) = x mod 123;

fun is_prime (n:int64) =
    let fun looper (i:int64) =
		    if (i*i) > n
		    then true
		    else if (0 = (n mod i))
		    then false
		    else looper (i+2)
    in
	if n < 10
	then exist n [2,3,5,7]
	else if 0 = (n mod 2)
	then false
	else looper 3
    end;

fun next_prime (m:int64) =
    if m = 2 then 3 else if is_prime (m+2) then m+2 else next_prime (m+2);

fun pfactors (n:int64) =
    let fun looper (i:int64) (j:int64) =
	    if is_prime j
	    then j
	    else if 0 = (j mod i)
	    then looper 2 (j div i)
	    else looper (next_prime i) j
    in looper 2 n
    end;



fun function x =
    let
        val t = Timer.startCPUTimer()
        val result = pfactors x
    in
        print (Time.toString(#usr(Timer.checkCPUTimer(t))) ^ "\n");
        result
    end;



