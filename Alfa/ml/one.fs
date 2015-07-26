module One

open System
open System.IO
open System.Diagnostics
open System.Collections.Generic

let square x = x * x;;

let cube x = x*x*x;;

let sumSieve (lim : int) =
  let primes = Array.zeroCreate<bool> (lim + 3)
  Array.fill primes 0 lim true
  let llim = int (sqrt (float lim))
  let initstuff = if 0 = llim % 2 then llim + 1 else llim + 2
  let rec iouter (i : int) (res : int64) =
    let rec inner (j : int) =
      match j with
        | _ when (j <= lim) -> (Array.set primes j false; inner (j+2*i))
        | _ -> ()
    match primes.[i] with
      | _ when (i*i > lim) -> res
      | true -> (inner (i*i) ; iouter (i+2) (res + (int64 i)))
      | false -> iouter (i+2) res
  let rec outer (i : int) (res : int64) =
    match primes.[i] with
      | _ when i > lim -> res
      | true -> outer (i+2) (res + (int64 i))
      | false -> outer (i+2) res
  outer initstuff (iouter 3 2L);;


let rec bexpt (a: bigint) (m : int) =
  match m with
    | 0 -> 1I
    | 1 -> a
    | _ -> let half = bexpt a (m/2)
           if m % 2 = 0 then half*half else a*half*half

let rec expt (a: int64) (m : int) =
 match m with
   | 0 -> 1L
   | 1 -> a
   | _ -> let half = expt a (m/2)
          if m % 2 = 0 then half*half else a*half*half

let sol1 (lim : int) =
  List.sum [for i in [1..lim] do if (0 = i % 3) || (0 = i % 5) then yield i]

let sol2 (lim : int) =
  let rec fibo a b res =
    match a with
      | _ when a > lim -> res
      | _ -> fibo (a+b) a (if 0 = a % 2 then a + res else res)
  fibo 2 1 0;;

let odd_prime (x : int64) =
  let rec loopi i =
    match i with
      | _ when (i*i) > x -> true
      | _ when 0L = x % i -> false
      | _ -> loopi (i + 2L)
  loopi 3L;;

let prime (x:int64) =
  match x with
    | _ when x < 2L -> false
    | 2L|3L|5L|7L -> true
    | _ when 0L = x % 2L -> false
    | _ -> odd_prime x ;;

let sol3 (n : int64) =
  let rec loopi (p : int64) (i : int64) =
      match p with
        | 0L -> i-2L
        | 1L -> i-2L
        | _ when odd_prime i
            -> if 0L = p % i then loopi (p/i) (i+2L)
               else loopi p (i+2L)
        | _ -> loopi p (i+2L)
  loopi n 3L;;

let numcol (i : int) =
  let rec loop n xs =
    match n with
      | _ when n < 10 -> n::xs
      | _ -> loop (n/10) (n % 10 :: xs)
  loop i [];;

let ispalin (n : int) =
  let xs = numcol n
  xs = List.rev xs;;

// 6ms
let sol4 maxi =
  let tabs = new Dictionary<string,int> ()
  let rec loopi a b =
    let mul = a*b
    let muls = (string a) + (string b)
    match a with
      | _ when a = b -> loopi a (b-1)
      | _ when mul < maxi -> 0
      | _ -> if tabs.ContainsKey(muls) then tabs.[muls]
             else let resi = loopi a (b-1)
                  let resj = loopi (a-1) b
                  match mul with
                    | _ when ispalin mul -> let ans = List.max [resi;resj;mul]
                                            (tabs.Add(muls, ans); ans)
                    | _ -> let ans = List.max [resi;resj]
                           (tabs.Add(muls,ans);ans)
  loopi 999 999;;

let collatz (n : int64) =
  match n with
    | _ when 0L = n % 2L -> n / 2L
    | _ -> (3L*n) + 1L

// 471ms
let sol14 (lim : int64) =
  let refs = new Dictionary<int64,int> ()
  let rec iter (n : int64) =
    match n with
      | 1L -> 1
      | _ when refs.ContainsKey(n) -> refs.[n]
      | _ -> let result = 1 + (iter <| collatz n)
             (refs.Add (n, result); result)
  let rec finder (i : int64) (maxi : int) (sumber : int64)=
    let tmp = iter i
    match tmp with
      | _ when i >= lim -> if tmp > maxi then i else sumber
      | _ when tmp > maxi -> finder (i+2L) tmp i
      | _ -> finder (i+2L) maxi sumber
  finder 500001L 1 1L;;

let sum_pdivs (n : int) =
  let k = if 0 = n % 2 then 1 else 2
  let lim = int (sqrt (float n))
  let rec iter i res =
    match i with
      | _ when i > lim -> res
      | _ when i*i = n -> res + i
      | _ when 0 = n % i -> iter (i+k) (res + i + (n/i))
      | _ -> iter (i+k) res
  iter (if 0 = n % 2 then 2 else 3) 1;;

// 1-2ms
let sol4b (a:int) =
  let b = 900
  let rec outer i res =
    let rec inner j =
      let mul = i*j
      match mul with
        | _ when j < (i-a) -> 0
        | _ when mul < res -> 0
        | _ when ispalin mul -> mul
        | _ -> inner (j-1)
    let initmul = i*(i-1)
    match initmul with
      | _ when initmul < res -> res
      | _ when i < b -> res
      | _ -> let resj = inner (i-1)
             if resj > res then outer (i-1) resj else outer (i-1) res
  outer 999 0;;

// 13ms
let sol4c (lower : int) =
  List.max [for i in lower..998 ->
              List.max [for j in (i+1)..999 ->
                let mul = i*j
                if ispalin mul then mul else 0]];;

// less than 1ms
let sol5 lst =
  let rec outer xs res =
    match xs with
      | [] -> List.fold (fun a b -> a * b) 1 res
      | hd::tl -> let rec inner xres resi =
                    match xres with
                      | [] -> resi::res
                      | hdx::tlx -> if 0 = resi % hdx then inner tlx (resi/hdx)
                                    else inner tlx resi
                  outer tl (inner res hd)
  outer lst [1];;

let lcm (lst : int64 list) =
  let rec outer (xs : int64 list) (res : int64 list) =
    match xs with
      | [] -> List.fold (fun a b -> a * b) 1L res
      | hd::tl -> let rec inner xres resi =
                    match xres with
                      | [] -> resi::res
                      | hdx::tlx -> if 0L = resi % hdx then inner tlx (resi/hdx)
                                    else inner tlx resi
                  outer tl (inner res hd)
  outer lst [1L];;

// less than 1 ms
let sol6 lim =
  let bahan = [1..lim]
  let squares = List.sumBy square bahan
  let sums = List.sum bahan
  let sumsquares = sums*sums
  sumsquares - squares;;

let sum_primes (lim) =
  let rec loopi i res =
      if i >= lim then res
      elif odd_prime i then loopi (i+2L) (res + (int64 i))
      else loopi (i+2L) res
  loopi 3L 2L;;


// less than 1ms
let sol7 (tar : int) =
  let lim = tar * 12
  let primes = Array.zeroCreate<bool> (lim+5)
  Array.fill primes 0 lim true
  let rec outer (i:int) (idx : int) =
    match primes.[i] with
      | true -> if idx = (tar-1) then i
                elif i <= lim / i then
                  let rec inner (j : int) =
                    if j > lim then ()
                    else (Array.set primes j false; inner(j+2*i))
                  (inner (i*i) ; outer (i+2) (idx+1))
                else outer (i+2) (idx+1)
      | _ -> outer (i+2) idx
  outer 3 1;;

let prod xs = List.reduce (*) xs

// this runs in 5ms
let sol8 nmax =
  let raw = Array.fold (+) "" <| File.ReadAllLines "p8.txt"
  let sumxs a b = prod [for i in raw.[a..b] -> int64 (string i)]
  let rec iter i (maxi : int64) =
    match i with
      | _ when i = nmax-15 -> maxi
      | _ -> let this_max = sumxs i (i+12)
             iter (i+1) (if this_max > maxi then this_max else maxi)
  iter 0 0L;;

let jumfak (lim : int) =
  let faks = Array.zeroCreate<int> (lim + 1)
  Array.fill faks 0 (lim+1) 1
  let rec outer (i : int) =
    let isqr = i*i
    let rec inner (j : int) =
      match j with
        | _ when j <= lim -> (Array.set faks j (faks.[j] + i + j/i); inner(j+i))
        | _ -> ()
    match i with
      | _ when isqr <= lim -> (Array.set faks isqr (faks.[isqr] + i); inner(isqr+i); outer(i+1))
      | _ -> List.filter (fun x -> faks.[x] > x ) [2..lim]
  outer 2;;

let sol23b (lim : int) =
  let hlim = lim/2
  let abuns = List.toArray <| jumfak lim
  let refs = Array.zeroCreate<bool> (lim+1)
  let maxi = Array.length refs
  Array.fill refs 0 lim false
  let rec outer (i : int) =
    let iref = abuns.[i]
    let rec inner (j : int) =
      let jref = abuns.[j]
      match j with
        | _ when iref+jref <= lim -> (Array.set refs (iref+jref) true; inner (j+1))
        | _ -> ()
    match iref with
      | _ when iref <= hlim -> (inner i; outer (i+1))
      | _ -> ( (lim * (lim+1))/2) - (List.sum (List.filter (fun x -> refs.[x]) [12..lim]))
  outer 0;;

let sol18 fname =
  let rows = Array.map (fun (x: String) -> x.Split [|' '|]) <| File.ReadAllLines fname
  let res = Array.map (fun x -> Array.map int x) rows
  let start = (Array.length res) - 2
  let rec iter i =
    let rec inner j =
      match j with
        | _ when j > i -> ()
        | _ -> let a = res.[i+1].[j]
               let b = res.[i+1].[j+1]
               if a > b then (Array.set res.[i] j (res.[i].[j] + a); inner (j+1))
               else (Array.set res.[i] j (res.[i].[j] + b); inner (j+1))
    match i with
      | 0 -> let a = res.[1].[0]
             let b = res.[1].[1]
             if a > b then res.[0].[0] + a
             else res.[0].[0] + b
      | _ -> (inner 0; iter (i-1))
  iter start;;

let sol22 () =
  let raw = File.ReadAllLines "p22.txt"
  let iraw = raw.[0].Split [|','|]
  let jraw = Array.sort iraw
  let tabs = new Dictionary<char,int> ()
  let abjad = ['A'..'Z']
  let rec init_dict i =
    if i > 26 then () else (tabs.Add (abjad.[i], i) ; init_dict (i+1))
  let cal = Array.length jraw
  let score i st = i * List.sum [for l in st -> tabs.[l]]
  let rec iter i res =
    if i = cal then res else iter (i+1) (res + score (i+1) jraw.[i])
  (init_dict 0; iter 0 0);;


let sol67 fname =
  let rows = Array.map (fun (x: String) -> x.Split [|' '|]) <| File.ReadAllLines fname
  let res = Array.map (fun x -> Array.map int x) rows
  let start = (Array.length res) - 2
  let rec iter i =
    let rec inner j =
      match j with
        | _ when j > i -> ()
        | _ -> let a = res.[i+1].[j]
               let b = res.[i+1].[j+1]
               if a > b then (Array.set res.[i] j (res.[i].[j] + a); inner (j+1))
               else (Array.set res.[i] j (res.[i].[j] + b); inner (j+1))
    match i with
      | 0 -> let a = res.[1].[0]
             let b = res.[1].[1]
             if a > b then res.[0].[0] + a
             else res.[0].[0] + b
      | _ -> (inner 0; iter (i-1))
  iter start;;

let ispsqr x =
  let xsqrt = sqrt (float x)
  let flx = int64 (floor xsqrt)
  let clx = int64 (ceil xsqrt)
  flx = clx;;

let count_divisor (n : int) =
  let step = if n % 2 = 0 then 1 else 2
  let rec iter (i : int) (res : int) =
    match i with
      | _ when i > n / i -> res
      | _ when i*i = n -> res+1
      | _ when n % i = 0 -> iter (i+step) (res+2)
      | _ -> iter (i+step) res
  iter 1 0;;

let sol12 (tar : int) =
  let rec iter (n : int) =
    let cdiv = count_divisor
    match n with
      | _ when n % 2 = 0 -> if (cdiv (n / 2)) * (cdiv (n+1)) > tar then ((int64 n)*(int64 (n+1)))/2L else iter (n+1)
      | _ -> if (cdiv ((n+1)/ 2)) * (cdiv n) > tar then ((int64 n)*(int64 (n+1)))/2L else iter (n+1)
  iter 12;;

let sum_sieve (lim:int) =
  let primes = Array.zeroCreate<bool> (lim+5)
  Array.fill primes 3 lim true
  let rec outer (i:int) (res : int64) =
    let rec inner (j:int) =
      if j > lim then ()
      else (Array.set primes j false; inner (j+2*i))
    if i > lim then res
    elif primes.[i] then
      if i <= lim / i then (inner (i*i); outer (i+2) (res + (int64 i)))
      else outer (i+2) (res + (int64 i))
    else outer (i+2) res
  outer 3 2L;;


let sol23 (lim : int) =
  let refs = Array.zeroCreate<bool> (lim+5)
  Array.fill refs 1 (lim+1) false
  let sum_abuns = Array.zeroCreate<bool> (lim+2)
  Array.fill sum_abuns 1 (lim+1) false
  let rec init_refs i =
    let isum = sum_pdivs i
    match i with
      | _ when i > lim -> ()
      | _ when isum > i -> (Array.set refs i true; init_refs (i+1))
      | _ -> (Array.set refs i false ; init_refs (i+1))
  let rec iter i =
    let rec iterj j =
      let n = i+j
      match n with
        | _ when n > lim -> ()
        | _ when refs.[j] -> (Array.set sum_abuns n true ; iterj (j+1))
        | _ -> iterj (j+1)
    match i with
      | _ when i > (lim / 2) -> ()
      | _ when refs.[i] -> (iterj i; iter (i+1))
      | _ -> iter (i+1)
  let rec outer i res =
    match i with
      | _ when i > lim -> res
      | _ when sum_abuns.[i] -> outer (i+1) (res+i)
      | _ -> outer (i+1) res
  init_refs 2
  iter 1
  (List.sum [1..lim])-(outer 1 0);;

let remove elm lst =
  let rec iter xs hxs =
    match xs with
      | [] -> List.rev hxs
      | (hd::tl) -> if elm = hd then (List.rev hxs) @ tl else iter tl (hd::hxs)
  iter lst [];;

let rec fact (i : int) : int =
  match i with
    | 0 -> 1
    | 1 -> 1
    | _ -> i * fact (i-1)

let colnum lst =
  let rec iter xs res =
    match xs with
      | [] -> res
      | (hd :: []) -> (int64 hd) + (10L*res)
      | (hd :: tl) -> iter tl ((int64 hd) + (10L * res))
  iter lst 0L;;

let sol24 (n : int) =
  let rec iter i raw res =
    match raw with
      | [] -> List.rev res
      | (x :: []) -> List.rev (x :: res)
      | _  -> let faks = fact ((List.length raw) - 1)
              let divs = i / faks
              let elm = raw.[divs]
              iter (i % faks) (remove elm raw) (elm :: res)
  colnum <| iter n [0..9] []


// runs in 1ms-an
let sol25 (tar:bigint) =
  let rec loopi a b (i:int) =
    if a > tar then i else loopi (a+b) a (i+1)
  loopi 1I 0I 1;;

// runs in about 9-11ms
let sol10 (lim:int) =
  let primes = Array.zeroCreate (lim + 5)
  Array.fill primes 0 (lim+1) true
  let rec outer (i : int) (res : int64) =
    let rec inner (j : int) =
      match j with
        | _ when j > lim -> 0L
        | _ -> (Array.set primes j false; inner (j + 2*i))
    match primes.[i] with
      | true -> if i <= lim / i then outer (i+2) (res + (int64 i) + (inner (i*i)))
                else outer (i+2) (res + (int64 i))
      | false -> if i > lim then res else outer (i+2) res
  outer 3 2L;;

// This one simply produces positive primes less than lim
let sieve (lim:int) =
  let primes = Array.zeroCreate<bool> (lim+5)
  Array.fill primes 3 lim true
  let rec outer (i:int) (res : int64) =
    let rec inner (j:int) =
      match j with
        | _ when j > lim -> ()
        | _ -> (Array.set primes j false; inner (j + 2*i))
    match primes.[i] with
      | true -> if i <= lim / i then (inner (i*i) ; outer (i+2) (res + (int64 i)))
                else outer (i+2) (res + (int64 i))
      | false -> if i > lim then List.filter (fun x -> primes.[x]) [3..2..lim]
                 else outer (i+2) res
  2::outer 3 2L;;

let sol47 (lim:int) =
  let primes = Array.zeroCreate<bool> (lim+2)
  let refs = Array.zeroCreate<int> (lim+2)
  Array.fill refs 0 lim 0
  Array.fill primes 0 lim true
  let allPrime i = [true;true;true;true] = List.map (fun x -> refs.[x] = 4) [i..i+3]
  let rec outer (i:int) =
    let rec inner (j:int) =
      match j with
        | _ when j > lim -> ()
        | _ -> (Array.set primes j false; inner (j+i))
    let rec innerRef (j : int) =
      match j with
        | _ when j > lim -> ()
        | _ -> (Array.set refs j (1 + refs.[j]) ; innerRef (j+i))
    match primes.[i] with
      | true -> if i <= lim/i then (inner (i*i); innerRef i; outer (i+1))
                else (innerRef i ; outer (i+1))
      | false -> if i > lim
                 then if allPrime i then i else outer (i+1)
                 else if allPrime i then i else outer (i+1)
  outer 2;;


let pascal (row:int) =
  let rec lastRow i res =
    match i with
      | _ when i = row -> res
      | _ -> lastRow (i+1) (List.map2 (fun x y -> x + y) (0L::res) (List.append res [0L]))
  lastRow 0 [1L];;

let sol15 (row : int) =
  List.sum <| List.map (fun x -> x*x) (pascal row);;

let bnumcol (n : bigint) =
  let rec iter i res =
    match i with
      | _ when i < 10I -> (int i)::res
      | _ -> iter (i/10I) (int(i%10I) :: res)
  iter n [];;

let sol16 (m : int) =
  List.sum <| bnumcol (bexpt 2I m)

let factorial (n : int) =
  let rec iter i (res : bigint) =
    if i = n then (bigint i) * res else iter (i+1) ((bigint i) * res)
  iter 1 1I;;

let sol21 (lim : int) =
  let is_amic (n : int) =
    let next = sum_pdivs n
    match next with
      | _ when next = n -> false
      | _ when n = sum_pdivs next -> true
      | _ -> false
  let rec summing i res =
    match i with
      | _ when i > lim -> res
      | _ when is_amic i -> summing (i+1) (res+i)
      | _ -> summing (i+1) res
  summing 2 0;;

let sol21b (lim : int) =
  let tabs = new Dictionary<int,bool> ()
  let is_amic (n : int) =
    if tabs.ContainsKey(n) then tabs.[n]
    else let next = sum_pdivs n
         match next with
          | _ when next = n -> (tabs.Add(n, false); false)
          | _ when n = sum_pdivs next -> (tabs.Add(n, true); tabs.Add(next, true); true)
          | _ -> (tabs.Add(n, false) ;false)
  List.sum <| List.filter is_amic [2..lim];;

let timed funi data msg =
    let timer = new Stopwatch()
    timer.Start()
    let result = funi data
    timer.Stop()
    printf "This is the answer for %s : %i \n" msg result
    printf "elapsed %d ms \n" timer.ElapsedMilliseconds


let timex funi data msg =
    let timer = new Stopwatch()
    timer.Start()
    let result = funi data
    timer.Stop()
    printf "This is the answer for %s : %i \n" msg result
    printf "elapsed %d ms \n" timer.ElapsedMilliseconds

let timeb funi data msg =
    let timer = new Stopwatch()
    timer.Start()
    let result = funi data
    timer.Stop()
    printf "This is the answer for %s : %i \n" msg result
    printf "elapsed %d ms \n" timer.ElapsedMilliseconds

let sol72 (lim : int) =
  let primes = Array.zeroCreate<bool> (lim+5)
  Array.fill primes 2 lim true
  let tots = Array.init (lim+2) (fun idx -> idx)
  let rec outer (i : int) =
    let rec inner (j:int) =
      match j with
        | _ when j > lim -> ()
        | _ -> (Array.set primes j false; inner (j+i))
    match primes.[i] with
      | true -> if i <= lim/i
                then (inner (i*i); outer (i+1))
                else outer (i+1)
      | false -> if i > lim then () else outer (i+1)
  let rec tots_outer (i : int) (res:int64) =
    let rec tots_inner (j:int) =
      match j with
        | _ when j > lim -> ()
        | _ -> let tmp = (int64 tots.[j]) * (int64 (i-1))
               (Array.set tots j (int (tmp/(int64 i))); tots_inner (j+i))
    match primes.[i] with
      | _ when i > lim -> res
      | true -> (tots_inner (2*i); tots_outer (i+1) (res + (int64 (i-1))))
      | false -> tots_outer (i+1) (res + (int64 tots.[i]))
  (outer 2; tots_outer 2 0L);;


let main () =
    timed sol1 1000 "#1"
    timed sol2 4000000 "#2"
    timex sol3 600851475143L "#3"
    timed sol4 900000 "#4a"
    timed sol4b 900 "#4b"
    timed sol5 [1..20] "#5"
    timed sol6 100 "#6"
    timed sol7 10001 "#7"
    timex sol8 1000 "#8"
    timex sol10 2000000 "#10"
    timex sol12 500 "#12"
    timex sol14 1000000L "#14"
    timex sol15 20 "#15"
    timed sol16 1000 "#16"
    timed sol18 "p18.txt" "#18"
    timed sol21 10000 "#21"
    timed sol23 28123 "#23"
    timex sol24 999999 "#24"
    timeb sol25 (bexpt 10I 999) "#25"
    timed sol47 150000 "#47"
    timed sol67 "p67.txt" "#67"
    timex sol72 1000000 "#72"
