module Two

open System
open System.IO
open System.Diagnostics
open System.Collections.Generic

let numcol (i : int64) =
  let rec loop n xs =
    match n with
      | _ when n < 10L -> n::xs
      | _ -> loop (n/10L) (n % 10L :: xs)
  loop i [];;

let oddPrime (n : int)=
  let rec outer (i : int) =
    match i with
      | _ when i > n/i -> true
      | _ when 0 = n % i -> false
      | _ -> outer (i+2)
  outer 3;;

let sol3(tar : int64) =
  let rec outer (n : int64) (i : int) (lim : int) =
    if i > lim then n else
      match i with
        | _ when 0L = n % (int64 i) && oddPrime(i) ->
          let rec inner (a : int64) =
            if 0L = a % (int64 i) then inner (a/(int64 i)) else a
          let puke = inner n
          outer puke (i+2) (int (sqrt (float puke)))
        | _ -> outer n (i+2) lim
  outer tar 3 1000000;;

let sol5 (lim : int) =
  let faks = Array.init (lim+2) (fun i -> i)
  let rec outer (i : int) (res : int64) =
    let p = faks.[i]
    let rec inner (j : int) =
      match j with
        | _ when j > lim -> ()
        | _ -> (Array.set faks j (faks.[j] / p) ; inner (j+i))
    match i with
      | _ when i > lim -> res
      | _ -> (inner (2*i); outer (i+1) (res * (int64 p)))
  outer 2 1L;;

let sol62 (lim : int) =
  let tabs = new Dictionary<int64 list,int64 list> ()
  let rec iter (i : int64) =
    let tmp = List.sort <| numcol (i*i*i)
    match tabs.ContainsKey tmp with
      | true -> let tmpi = tabs.[tmp]
                if (List.length tmpi) = lim-1 then List.map (fun x -> x*x*x) tmpi
                else (tabs.Remove tmp ; tabs.Add (tmp, i::tmpi) ; iter (i+1L))
      | false -> (tabs.Add (tmp,[i]); iter (i+1L))
  iter 1L;;

// unbelievably fast 1ms or less
let sol21 (n : int) : int =
  let lim = 3*n
  let faks = Array.zeroCreate<int> (3+lim)
  Array.fill faks 0 lim 1
  let rec outer (i:int) : int =
    let rec inner (j:int) =
      match j with
        | _ when j > lim -> ()
        | _ -> (Array.set faks j (faks.[j] + i + j/i) ; inner (j+i))
    let isqr = i*i
    match i with
      | _ when i > lim / i -> 0
      | _ -> (Array.set faks isqr (faks.[isqr] + i) ; inner (isqr+i); outer (i+1))
  let rec acum (i : int) (res : int) : int =
    let itmp = faks.[i]
    match i with
      | _ when i > n -> res
      | _ -> if (i <> itmp) && (i = faks.[itmp])
             then acum (i+1) (res+i)
             else acum (i+1) res
  acum 1 (outer 2);;

let sumSieve (lim : int) =
  let llim = int (sqrt (float lim))
  let t = if 0 = llim % 2 then llim + 1 else llim + 2
  let primes = Array.zeroCreate<bool> (lim+3)
  Array.fill primes 0 lim true
  let rec outer (i : int) (res : int64)=
    let rec inner (j : int) =
      if j > lim then () else (Array.set primes j false; inner (j+i+i))
    match primes.[i] with
      | _ when i > llim -> res
      | true -> (inner (i*i); outer (i+2) (res + (int64 i)))
      | false -> outer (i+2) res
  let rec counter (i : int) (res : int64) =
    match primes.[i] with
      | _ when i > lim -> res
      | true -> counter (i+2) (res + (int64 i))
      | false -> counter (i+2) res
  counter t (outer 3 2L);;

let timed funi data msg =
    let timer = new Stopwatch()
    timer.Start()
    let result = funi data
    timer.Stop()
    printf "This is the answer for %s : %i \n" msg result
    System.Console.WriteLine timer.Elapsed


let timex funi data msg =
    let timer = new Stopwatch()
    timer.Start()
    let result = funi data
    timer.Stop()
    printf "This is the answer for %s : %i \n" msg result
    System.Console.WriteLine timer.Elapsed

let main () =
  timed sumSieve 2000000 "#sumSieve to 2000000"
  timed sol5 20 "#No5"
  timex sol21 10000 "#No 21"
  timed sol3 600851475143L "#no 3"
  timed sumSieve 2000000 "#sumSieve to 2000000"
  timed sol5 20 "#No5"
  timex sol21 10000 "#No 21"
  timed sol3 600851475143L "#no 3"
