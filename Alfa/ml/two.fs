module Two

open System
open System.IO
open System.Diagnostics
open System.Collections.Generic

let sumSieve (lim : int) =
	let primes = Array.zeroCreate<bool> (lim + 1)
	Array.fill primes 0 lim true
	let llim = int <| float lim
	let initstuff = if 0 = llim % 2 then llim + 1 else llim + 2
	let rec initOuter (i : int, res : int64) =
		let isqr = i*i
		let rec inner (j : int) =
			match j with
				| _ when j <= lim -> (Array.set primes j false; inner (j+2*i))
				| _ -> ()
		match primes.[i] with
			| _ when isqr > lim -> res
			| true -> (inner isqr; outer (i+2) (res + (int64 i)))
			| false -> outer (i+2)
	(initOuter 3 2L) + (List.sum (List.filter (fun x -> primes.[x]) [initstuff..2..lim]));;
