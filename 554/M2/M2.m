(* ::Package:: *)

BeginPackage["M2`"];
buildCC2D;
buildCC3D;
buildCC3D2;
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


buildCC3D2[bimg_]:=
	Module[{C,X,Y,Z,M,b,c0,c1,c2},
		C={{},{},{},{}};
		{X,Y,Z}=Dimensions[bimg]*2-1;
		M=Table[0,X,Y,Z];
		c0=c1=c2=0;
		Do[If[bimg[[x,y,z]]==1,
			AppendTo[C[[1]],{x-1,y-1,z-1}];
			M[[x*2-1,y*2-1,z*2-1]]=++c0],
		{x,(X+1)/2},{y,(Y+1)/2},{z,(Z+1)/2}];
		Do[If[M[[x,y,z]]>0,
			b={
				M[[x,y,z]],
				If[x>1,M[[x-2,y,z]],0],
				If[y>1,M[[x,y-2,z]],0],
				If[z>1,M[[x,y,z-2]],0],
				If[x>1&&y>1,M[[x-2,y-2,z]],0],
				If[x>1&&z>1,M[[x-2,y,z-2]],0],
				If[z>1&&y>1,M[[x,y-2,z-2]],0],
				If[x>1&&y>1&&z>1,M[[x-2,y-2,z-2]],0]};
			If[b[[2]]>0,AppendTo[C[[2]],{b[[1]],b[[2]]}];M[[x-1,y,z]]=++c1];
			If[b[[3]]>0,AppendTo[C[[2]],{b[[1]],b[[3]]}];M[[x,y-1,z]]=++c1];
			If[b[[4]]>0,AppendTo[C[[2]],{b[[1]],b[[4]]}];M[[x,y,z-1]]=++c1];
			If[b[[2]]>0&&b[[3]]>0&&b[[5]]>0,AppendTo[C[[3]],{
				M[[x-1,y,z]],M[[x-1,y-2,z]],M[[x,y-1,z]],M[[x-2,y-1,z]]
			}];M[[x-1,y-1,z]]=++c2];
			If[b[[2]]>0&&b[[4]]>0&&b[[6]]>0,AppendTo[C[[3]],{
				M[[x-1,y,z]],M[[x-1,y,z-2]],M[[x,y,z-1]],M[[x-2,y,z-1]]
			}];M[[x-1,y,z-1]]=++c2];
			If[b[[3]]>0&&b[[4]]>0&&b[[7]]>0,AppendTo[C[[3]],{
				M[[x,y-1,z]],M[[x,y-1,z-2]],M[[x,y,z-1]],M[[x,y-2,z-1]]
			}];M[[x,y-1,z-1]]=++c2];
			If[b[[2]]*b[[3]]*b[[4]]*b[[5]]*b[[6]]*b[[7]]*b[[8]]!=0,AppendTo[C[[4]],{
				M[[x-1,y-1,z]],M[[x-1,y,z-1]],M[[x,y-1,z-1]],
				M[[x-1,y-1,z-2]],M[[x-1,y-2,z-1]],M[[x-2,y-1,z-1]]
			}]]],
		{x,1,X,2},{y,1,Y,2},{z,1,Z,2}];
	C];


(* ::Text:: *)
(*Algorithm 3 *)


thinExhaustive[cc_]:=Module[
	{U,E,R,e,k},
	U=Map[(0&),cc,{2}];
	Do[Scan[(U[[d-1,#]]++)&,cc[[d]],{2}],{d,2,Length[cc]}];
	e=1;
	
	While[e!=0,
	e=0;
	E=Table[{},Length[cc]];
	Do[If[U[[d,i]]==0,
		Scan[(If[U[[d-1,#]]==1,
			e++;
			AppendTo[E[[d]],i];
			AppendTo[E[[d-1]],#];
			Return[]])&,
		cc[[d,i]]]
	],{d,2,Length[cc]},{i,Length[cc[[d]]]}];
	Do[Scan[(
		U[[d,#]]=-1;
		If[d!=1,Scan[U[[d-1,#]]--&,cc[[d,#]]]];
	)&,E[[d]]],{d,Length[E]}]];
	
	R=Table[0,Length[cc]];
	Do[
		k=0;
		If[d!=Length[U],U[[d]]=(If[#<0,#,++k]&)/@U[[d]]];
		R[[d]]=MapThread[(If[#1<0,Nothing,If[d==1,#2,U[[d-1,#]]&/@#2]])&,{U[[d]],cc[[d]]}],
	{d,Length[U]}];
	While[Length[R[[-1]]]==0,R=Most[R]];
R];


(* ::Text:: *)
(*Algorithm 4*)


sameCC=If[Length[#1]!=Length[#2],False,AllTrue[MapThread[(Length[#1]==Length[#2]&),{#1,#2}],TrueQ]]&;


thin[cc_,{t1_,t2_}]:=Module[
	{U,I,E,e,k=0,q,R},
	U=Map[(0&),cc,{2}];
	I=Map[(Infinity&),cc,{2}];
	Do[Scan[(U[[d-1,#]]++)&,cc[[d]],{2}],{d,2,Length[cc]}];
	While[k==0||e>0,
		Do[If[U[[d,i]]==0&&I[[d,i]]>k,I[[d,i]]=k],{d,Length[cc]},{i,Length[cc[[d]]]}];
		k++;
		q={-Infinity,k-Max[t1[[1]],t1[[2]]*k],k-Max[t2[[1]],t2[[2]]*k],-Infinity};
		E=Table[{},Length[cc]];e=0;
		Do[If[U[[d,i]]==0&&I[[d,i]]>q[[d]],
			Scan[(If[U[[d-1,#]]==1,
				e++;
				AppendTo[E[[d]],i];
				AppendTo[E[[d-1]],#];
				Return[]])&,
			cc[[d,i]]]
		],{d,2,Length[cc]},{i,Length[cc[[d]]]}];
		Do[Scan[(
			U[[d,#]]=-1;
			If[d!=1,Scan[U[[d-1,#]]--&,cc[[d,#]]]];
		)&,E[[d]]],{d,Length[E]}]
	];
	R=Table[0,Length[cc]];
	Do[
		k=0;
		If[d!=Length[U],U[[d]]=(If[#<0,#,++k]&)/@U[[d]]];
		R[[d]]=MapThread[(If[#1<0,Nothing,If[d==1,#2,U[[d-1,#]]&/@#2]])&,{U[[d]],cc[[d]]}],
	{d,Length[U]}];
	While[Length[R[[-1]]]==0,R=Most[R]];
R];


End[];
EndPackage[];
