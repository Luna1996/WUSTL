(* ::Package:: *)

BeginPackage["M2`"];
buildCC2D;
buildCC3D;
thinExhaustive;
sameCC;
thin;
Begin["`P`"];


(* ::Text:: *)
(*Algorithm 1*)


c3x3={
	{1,3,7,9},
	{{2,{1,3}},{4,{1,7}},{6,{3,9}},{8,{7,9}}},
	{2,4,6,8}};


buildCC2D[bimg_]:=
	Module[{C,X,Y,M,T,n,i},
		C={{},{},{}};
		{X,Y}=Dimensions[bimg];
		M=Table[0,(2*X+1)*(2*Y+1)];
		T=Table[0,9];
		Do[If[bimg[[x,y]]==1,
			i=(2*x-1)+(2*y-1)*(2*X+1)+1;n=1;
			Do[T[[n++]]=i+dx+dy,{dx,{-1,0,1}},{dy,{-X*2-1,0,X*2+1}}];
			Do[If[M[[T[[v]]]]==0,
				AppendTo[C[[1]],QuotientRemainder[T[[v]]-1,X*2+1]/2+1];
				M[[T[[v]]]]=Length[C[[1]]]];
			,{v,c3x3[[1]]}];
			Do[If[M[[T[[v[[1]]]]]]==0,
				AppendTo[C[[2]],M[[T[[v[[2]]]]]]];
				M[[T[[v[[1]]]]]]=Length[C[[2]]]];
			,{v,c3x3[[2]]}];
			AppendTo[C[[3]],M[[T[[c3x3[[3]]]]]]];
		],{x,X},{y,Y}];
	C];


(* ::Text:: *)
(*Algorithm 2*)


c3x3x3={
	{1,3,7,9,19,21,25,27},
	{{2,{1,3}},{4,{1,7}},{6,{3,9}},{8,{7,9}},
	{20,{19,21}},{22,{19,25}},{24,{21,27}},{26,{25,27}},
	{10,{1,19}},{12,{3,21}},{16,{7,25}},{18,{9,27}}},
	{{5,{2,4,6,8}},{11,{2,10,12,20}},{13,{4,10,16,22}},
	{15,{6,12,18,24}},{17,{8,16,18,26}},{23,{20,22,24,26}}},
	{5,11,13,15,17,23}
};


xyz=({Mod[#1,#2],Mod[Quotient[#1,#2],#3],Quotient[#1,#2*#3]}/2+1)&;


buildCC3D[bimg_]:=
	Module[{C,X,Y,Z,M,T,n,i},
		C={{},{},{},{}};
		{X,Y,Z}=Dimensions[bimg]*2+1;
		M=Table[0,X*Y*Z];
		T=Table[0,27];
		Do[If[bimg[[x,y,z]]==1,
			i=(2*x-1)+(2*y-1)*X+(2*z-1)*X*Y+1;n=1;
			Do[T[[n++]]=i+dx+dy+dz,
				{dx,{-1,0,1}},
				{dy,{-X,0,X}},
				{dz,{-X*Y,0,X*Y}}];
			Do[If[M[[T[[v]]]]==0,
				AppendTo[C[[1]],xyz[T[[v]]-1,X,Y]];
				M[[T[[v]]]]=Length[C[[1]]]];
			,{v,c3x3x3[[1]]}];
			Do[If[M[[T[[v[[1]]]]]]==0,
				AppendTo[C[[d]],M[[T[[v[[2]]]]]]];
				M[[T[[v[[1]]]]]]=Length[C[[d]]]];
			,{d,2,3},{v,c3x3x3[[d]]}];
			AppendTo[C[[4]],M[[T[[c3x3x3[[4]]]]]]];
		],{x,(X-1)/2},{y,(Y-1)/2},{z,(Z-1)/2}];
	C];


(* ::Text:: *)
(*Algorithm 3 *)


thinExhaustive[cc_,keep_:(True&)]:=Module[
	{C,M1,M2,D,T,t},
	C=cc;
	M1=M2=Map[0&,cc,{2}];
	D=Length[cc];
	T=Table[{},D];
	T[[1]]=Table[i,{i,Length[cc[[1]]]}];
	Do[
		T[[d]]=Table[i,{i,Length[cc[[d]]]}];
		(* calculate for all d-1-cell that how many d-cell use it as boundary, track the last user *)
		Do[Scan[(M1[[d-1,#]]=c;M2[[d-1,#]]+=1)&,cc[[d,c]]],{c,Length[cc[[d]]]}];
		Do[
			t=M1[[d-1,c]];
			(* Find a simple pair *)
			If[M2[[d-1,c]]==1&&keep[d,t,d-1,c],
			(* Mark the pair as removeable *)
			T[[d-1,c]]=0;T[[d,t]]=0;
			(* Mark the other boundary of the simple cell as unremoveable *)
			Scan[If[#!=c,M2[[d-1,#]]=0]&,cc[[d,t]]];
		],{c,Length[cc[[d-1]]]}];
		(* Remove witness and simple cell in d-1-cell*)
		C[[d-1]]=MapThread[If[#1==0,Nothing,#2]&,{T[[d-1]],C[[d-1]]}];
		(* Relabel d-cell*)
		t=0;
		C[[d]]=C[[d]]/.Table[If[T[[d-1,i]]!=0,t++;If[i!=t,i->t,Nothing],Nothing],{i,Length[T[[d-1]]]}];
	,{d,2,D}];
	(* Remove simple cell in D-cell*)
	C[[D]]=MapThread[If[#1==0,Nothing,#2]&,{T[[D]],C[[D]]}];
	If[Length[C[[D]]]==0,Most[C],C]
];


(* ::Text:: *)
(*Algorithm 4*)


sameCC=If[Length[#1]!=Length[#2],False,AllTrue[MapThread[(Length[#1]==Length[#2]&),{#1,#2}],TrueQ]]&;


thin[cc_,{t1_:{Infinity,1},t2_:{Infinity,1}}]:=
	Module[{C1,C2,I,S,m,keep,k},
		C1=cc;
		I=Map[Infinity&,cc,{2}];
		keep=(If[k-I[[#1,#2]]>m[[#1]],AppendTo[S,{#1,#2}];AppendTo[S,{#3,#4}];False,True])&;
		k=0;
		While[!sameCC[C1,C2],
			C2=C1;
			Do[Scan[(I[[d-1,#]]=k)&,C2[[d,c]]],{d,3,Length[C2]},{c,Length[C2[[d]]]}];
			k++;
			m={Infinity,Max[t1[[1]],t1[[2]]*k],Max[t2[[1]],t2[[2]]*k],Infinity};
			S={};
			C1=thinExhaustive[C2,keep];
			Scan[(I[[#[[1]],#[[2]]]]=Nothing)&,S];
		];
	C1];


End[];
EndPackage[];
