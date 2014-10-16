use "bigint.sml";

fun res x =
    let
        val t = Timer.startCPUTimer()
        val result = suma_prima x
    in
	print (Time.toString(#usr(Timer.checkCPUTimer(t))) ^ "\n");
        result
    end;
