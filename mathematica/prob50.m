(* ::Package:: *)

(* ::Input:: *)
(*sol1[lim_] := Return[Sum[If[((Mod[i,3] ==0) || (Mod[i,5]== 0)), Return[i],Return[0]]&, {i,lim}]];*)
(*sol1[1000]*)


(* ::Input:: *)
(*sol1 [lim_] := Fold[Plus,Select[Range[1,lim], (Mod[#,3] == 0) || (Mod[#,5]==0) &]];*)
(**)
(*sol1a[lim_] :=*)
(*Module[{i=1,sum=0},*)
(*While[i< lim,*)
(*If[Mod[i,3]==0 || Mod[i,5]==0, sum+= i];*)
(*i++];*)
(*Return[sum]];*)
(*RepeatedTiming[sol1a[1000]]*)


(* ::Input:: *)
(*sol2[lim_] := *)
(*Module[{a=1,b=0,sum=0,tmp=0},*)
(*While[a<=lim, tmp = a;a+=b;b=tmp;If[EvenQ[a],sum+=a]];Return[sum]]*)


(* ::Input:: *)
(*RepeatedTiming[sol2[4000000]]*)


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
(*RepeatedTiming[sol3[600851475143]]*)


(* ::Input:: *)
(*isPalin[xs_] := Reverse[xs]== xs*)


(* ::InheritFromParent:: *)
(*sol4[lim_] := *)
(*Block[{tor =Select[Apply[Join,Table[{i,j}, {i,900,lim},{j,900,lim}]],Block[{t1=#[[1]],t2=#[[2]]}, t1!= t2 && isPalin[IntegerDigits[t1*t2]]]&]},*)
(*Max [Map[#[[1]]*#[[2]]&,tor]]]*)
(*RepeatedTiming[sol4[999]]*)


(* ::Input:: *)
(*sol4a [lim_] :=*)
(*Module[{cur = 0},*)
(*For[i=lim, i >= 900, i--, *)
(*For[j=i-1, j >= 900, j--,*)
(*Block[{tmp = i*j},*)
(*If[isPalin[IntegerDigits[tmp]],*)
(*If[tmp> cur,*)
(*cur = tmp]]]]];*)
(*cur];*)
(*RepeatedTiming[sol4a[999]]*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)
