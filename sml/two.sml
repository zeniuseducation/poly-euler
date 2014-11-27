open List;


fun remove e [] = []
  | remove e (x::xs) =
    if e = x then xs else x :: (remove e xs);

fun exist e ls = exists (fn x => x = e) ls;

fun sort [] = []
  | sort ((x::xs):int list) = 
    let val smaller = filter (fn i => x >= i) xs
	val larger = filter (fn i => x < i) xs
    in (sort smaller) @ [x] @ (sort larger)
    end;

fun is_pandig ls = (sort ls) = range 1 9 1;

fun distinct (ls:int list) =
    let fun looper [] res = res
	  | looper ((x::xs):int list) (res:int list) = 
	    if exist x res
	    then looper xs res
	    else looper xs (res @ [x])
    in looper ls []
    end;

fun pandig_products (lim:int) =
    let fun outer (i:int) (res:int list) =
	    let fun inner (j:int) (resj:int list) =
		    if (i*j) > (3*lim)
		    then resj
		    else if is_pandig ((numcol i) @
				       (numcol j) @
				       (numcol (i*j)))
		    then inner (1 + j) ((i*j)::resj)
		    else inner (1+j) resj
	    in if (i*i) > lim
	       then res
	       else outer (1+i) (res @ (inner (1+i) []))
	    end;
    in foldl (fn (x,y) => x+y) 0 (distinct (outer 2 []))
    end;

fun numcol (n:int) =
    let fun looper (i:int) (res:int list) =
	    if i< 10
	    then i::res
	    else looper (i div 10) ((i mod 10)::res)
    in looper n []
    end;

fun colnum ls =
    let fun looper (x::xs) res =
	    if null xs
	    then x + (10*res)
	    else looper xs (x+10*res)
    in looper ls 0
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

fun is_cprime (n:int) =
    let val res = (length (numcol n)) - 1
	fun looper (m:int) (i:int) =
	    if i = ~1
	    then true
	    else if is_prime m
	    then looper ((m div 10) + ((m mod 10) * (pow 10 res))) (i-1)
	    else false
    in if n < 10 then exist n [3,7] else looper n res
    end;

fun all_cprimes (lim:int) =
    let val bahan = [1,3,7,9]
	fun looper (i:int) =
	    if i > lim
	    then 0
	    else if is_cprime i
	    then foldl (fn (x,y) => x+y) 1
		       (map (fn x => looper ((10*i) + x)) bahan)
	    else foldl (fn (x,y) => x+y) 0
                       (map (fn x => looper ((10*i) + x)) bahan)
    in foldl (fn (x,y) => x+y) 2 (map looper bahan)
    end;

fun is_binpalin (n:int) =
    let fun bincol (i:int) (res:int list)=
	    if i < 2
	    then i :: res
	    else bincol (i div 2) ((i mod 2) :: res)
	val bcol= (bincol n) []
    in bcol = rev bcol
    end;

fun sum_bipalins (dig:int) =
    let fun evenpalins (n:int) (res:int) =
	    let val (x::xs) = numcol n
		val tnum = colnum ((x::xs) @ (rev (x::xs)))
	    in
		if n = pow 10 (dig div 2)
		then res
		else if 0 = (x mod 2)
		then evenpalins (colnum ((x+1)::xs)) res
		else if is_binpalin tnum
		then evenpalins (n+1) (tnum+res)
		else evenpalins (n+1) res
	    end;
	fun oddpalins (n:int) (res:int) =
            let val (x::xs) = numcol n
		val nums = map (fn i =>
				   colnum ((x::xs) @ [i] @ (rev (x::xs))))
			       (range 0 9 1)
		val useit = foldl (fn (x,y) => x+y) 0
				  (filter is_binpalin nums)
            in
                if n = pow 10 (dig div 2)
                then res
		else if 0 = (x mod 2)
		then oddpalins (colnum ((x+1)::xs)) res
                else oddpalins (n+1) (res + useit)
            end;
    in if dig <= 1
       then foldl (fn (x,y) => x+y) 0 [1,3,5,7,9]
       else if 0 = (dig mod 2)
       then (evenpalins (pow 10 ( (dig div 2)-1)) 0) + (sum_bipalins (dig - 1))
       else (oddpalins (pow 10 ((dig div 2)-1)) 0) + (sum_bipalins (dig - 1))
    end;

fun is_rtprime (n:int)=
    if n < 10
    then exist n [2,3,5,7]
    else if is_prime n
    then is_rtprime (n div 10)
    else false;

fun is_ltprime (n:int) =
    if n < 10
    then exist n [2,3,5,7]
    else if is_prime n
    then is_ltprime (n mod (pow 10 (length (numcol n) - 1)))
    else false;

fun tprimes (lim:int) =
    let val digs = [1,3,7,9,0]
	val refs = [2,3,5,7]
	fun outer (refsi:int list) (res:int list) = 
	    let fun inner (i:int) (j:int) (refsa : int list) resa =
		    let val num = (nth(digs,j)) + (10*i)
		    in
			if j = 4
			then [refsa,resa]
			else if is_rtprime num
			then if is_ltprime num
			     then inner i (1+j)
					(num::refsa) (num::resa)
			     else inner i (1+j)
					(num::refsa) resa
			else inner i (1+j) refsa resa
		    end
		val results = map (fn m => inner m 0 [] res) refsi
	    in if (length res) >= lim
	       then res
	       else outer (distinct (concat (map hd results)))
			  (distinct (concat (map (fn k => nth (k,1)) results)))
	    end;
    in foldl (fn (x,y) => x+y) 0 (outer refs [])
    end;

fun count_divs (n:int) =
    let fun looper (i:int) (j:int) (res:int) =
	    if i*i >= n
	    then if i*i = n then res+1 else res
	    else if (n mod i)=0
	    then looper (i+j) j (res+2)
	    else looper (i+j) j res
    in if 0 = (n mod 2)
       then looper 2 1 2
       else looper 3 2 2
    end;

fun triangle500 (target:int) =
    let fun looper (i:int) =
	    let val divs = if (0=i mod 2)
			   then (count_divs (i div 2))
				* (count_divs (i+1))
			   else (count_divs ((i+1) div 2))
                                * (count_divs i)
	    in if divs > target then (i* (i+1)) div 2 else looper (i+1)
	    end
    in looper 3
    end;

fun function x =
    let
        val t = Timer.startCPUTimer()
        val result = triangle500 x;
    in
        print (Time.toString(#usr(Timer.checkCPUTimer(t))) ^ "\n");
        result
    end;


			

	    







  
