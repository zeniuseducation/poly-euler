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
(*isPalin[xs_]:= Reverse[xs]== xs;*)


(* ::InheritFromParent:: *)
(*sol4[lim_] := *)
(*Block[{tor =Select[Apply[Join,Table[{i,j}, {i,900,lim},{j,900,lim}]],Block[{t1=#[[1]],t2=#[[2]]}, t1!= t2 && isPalin[IntegerDigits[t1*t2]]]&]},*)
(*Max [Map[#[[1]]*#[[2]]&,tor]]]*)
(*RepeatedTiming[sol4[999]]*)


(* ::Input:: *)
(*sol4a [lim_] :=*)
(*Block[{cur = 0},*)
(*For[i=lim, i >= 900, i--, *)
(*For[j=i-1, j >= 900, j--,*)
(*Block[{tmp = i*j},*)
(*If[isPalin[IntegerDigits[tmp]],*)
(*If[tmp> cur,*)
(*cur = tmp]]]]];*)
(*cur];*)
(*RepeatedTiming[sol4a[999]];*)


(* ::Input:: *)
(*sol5[lim_] := *)
(*Module[{refs = Array[#&,lim]},*)
(*For[i=2,i<= lim, i++,*)
(*Block[{tmp = refs[[i]]},*)
(*For[j = 2*i, j <= lim, j += i,*)
(*refs[[j]] /= tmp]]];*)
(*Fold[Times,refs]]*)


(* ::Input:: *)
(*RepeatedTiming[sol5[20]]*)


(* ::Input:: *)
(*square[x_]:= x*x;*)
(*sol6 = Compile[{lim},*)
(*Module[{tmp=Range[1,lim]},*)
(*square[Fold[Plus,tmp]] - Fold[Plus, square /@ tmp]]];*)
(*RepeatedTiming[sol6[100]]*)


(* ::Input:: *)
(*sol5a = *)
(*Compile[{lim},*)
(*Block[{refs = Range[1,20]},*)
(*For[i = 2, i <= lim, i++,*)
(*Block[{tmp = refs[[i]]},*)
(*For[j = i+i, j <= lim , j += i,*)
(*refs[[j]] /= tmp]]];*)
(*Fold[Times,refs]]];*)
(*RepeatedTiming[sol6[20]];*)
(*Round[Sqrt[10]]*)


(* ::Input:: *)
(*sol7 = Compile[{nth},*)
(*Block[{lim= 12*nth, primes = Array[True,lim], i = 3, cur =2, n=1, llim = Round[Sqrt[lim]]},*)
(*While[n!= nth,*)
(*If[i <= llim,*)
(*If[primes[[i]],*)
(*cur = i; n++;*)
(*For[j = i*i, j <= lim, primes[[j]] = False, j += i + i]],*)
(*If[primes[[i]],*)
(*cur = i; n++]];*)
(*i+= 2];*)
(*Return[cur]]];*)
(*RepeatedTiming[sol7[101]];*)
(**)


(* ::Input:: *)
(*sol7a[nth_] := *)
(*Block[{lim=12*nth, primes=Table[True,{x,1,lim}], cur = 2, i = 3, n= 1, llim=Round[Sqrt[lim]]},*)
(*While[n != nth,*)
(*If[i<= llim,*)
(*If[primes[[i]],*)
(*For[j = i*i, j <= lim, primes[[j]] = False, j += i+i] ;*)
(*cur = i; n++],*)
(*If[primes[[i]],*)
(*cur = i; n++]];*)
(*i+= 2];*)
(*cur];*)
(*RepeatedTiming[sol7a[10001]]*)


(* ::Input:: *)
(*Block[{tmpi=10, x = 10*tmpi}, x]*)
