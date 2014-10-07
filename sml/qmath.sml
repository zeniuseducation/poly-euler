open Math
open List



fun res x =
    let
        val t = Timer.startCPUTimer()
        val result = sol7 x
    in
	print (Time.toString(#usr(Timer.checkCPUTimer(t))) ^ "\n");
        result
    end;
