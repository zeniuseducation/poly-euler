(* ::Package:: *)

(* ::Input:: *)
(*sol1[lim_] := Return[Sum[If[((Mod[i,3] ==0) || (Mod[i,5]== 0)), Return[i],Return[0]]&, {i,lim}]];*)
(*sol1[1000]*)


(* ::Input:: *)
(*sol1 [lim_] := Fold[Plus,Select[Range[1,lim], (Mod[#,3] == 0) || (Mod[#,5]==0) &]];*)
(*Timing[sol1[1000]]*)


(* ::Input:: *)
(*sol2[lim_] := *)
(*Module[{a=1,b=0,sum=0,tmp=0},*)
(*While[a<=lim, tmp = a;a+=b;b=tmp;If[EvenQ[a],sum+=a]];Return[sum]]*)


(* ::Input:: *)
(*Timing[sol2[4000000]]*)


(* ::Input:: *)
(*oddPrime[x_] := *)
(*Module[{i=3},*)
(*While[i*i <= x, *)
(*If[Mod[x,i]==0, Return[False]];*)
(*i += 2];*)
(*Return[True]]*)


(* ::Input:: *)
(*sol3[tar_] :=*)
(*Module[{i=3, n = tar},*)
(*While [True,*)
(*If[Mod[n,i]==0,*)
(*If[oddPrime[i],*)
(*While[Mod[n,i]==0,n = Quotient[n,i]];*)
(*If[oddPrime[n],Return [n]]]];*)
(*i+=2]]*)


(* ::Input:: *)
(*Timing[sol3[600851475143]]*)
