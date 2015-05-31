  
let square x = x * x;;
  
let rec evenfibo a b res tar =
  if a > tar
  then res
  else evenfibo (a+b) a(if 0 == a mod 2 then a else 0) tar;;
  

  
let oddPrime (p : int) = 
  let rec looper (i : int) = 
    if i*i > p 
    then true
    else
      if 0 == p mod i
      then false
      else looper (i+2)
  in looper 3;;

let lprime (n : int) =
  let rec looper (p : int) (i : int) =
    if 0 == p mod 2
    then looper (p / 2) i
    else if not (oddPrime i)
    then looper p (i+2)
    else if oddPrime p
    then p
    else if 0 == p mod i
    then looper (p / i) 3
    else looper p (i+2)
  in looper n 3;;
  
let sumPrimes (lim : int) =
  let rec looper (i : int) (res : int) =
    if i > lim
    then res
    else
      if oddPrime i
      then looper (i+2) (res+i)
      else looper (i+2) res
  in looper 3 2;;

let sumSieve lim =
  let is_prime = Array.make (lim+1) true in
  let llim = floor(sqrt (float lim)) in
  let i = ref 3 in
  let res = ref 2 in
  while !i < lim do
    if is_prime.(!i) then
      if !i <= llim then
	let j = ref (i*i) in
	while !j < lim do
	  is_prime.(!j) <- false;
	  j := !j + (2*!i);
	done;
	res := !res + !i;
	i := !i + 2;
      else
	begin
	  res := !res + !i;
	  i := !i + 2;
	end;
    else i := !i + 2
  done;
  !res;;

let time f x =
  let t = Sys.time() in
  let fx = f x in
  Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
  fx;;


