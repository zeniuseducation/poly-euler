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

fun numcol n = if n < 10 then [n] else (numcol (n div 10))
				       @ [n mod 10];

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
    if n < 10 then [n] else (numcol (n div 10)) @ [n mod 10];

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
            


fun function x =
    let
        val t = Timer.startCPUTimer()
        val result = pandig_products x;
    in
        print (Time.toString(#usr(Timer.checkCPUTimer(t))) ^ "\n");
        result
    end;
	    







  
