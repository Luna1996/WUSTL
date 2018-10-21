(* ::Package:: *)

BeginPackage["A3`"];


buildIntervalTree;
contour;


Begin["`P`"];


P=Global`P;


(* ::Text:: *)
(*Build Centered Interval Tree*)


V2D[x_,y_]:={
	{x,y},{x+1,y},
	{x+1,y+1},{x,y+1}};
V3D[x_,y_,z_]:={
	{x,y,z},{x+1,y,z},{x+1,y+1,z},{x,y+1,z},
	{x,y,z+1},{x+1,y,z+1},{x+1,y+1,z+1},{x,y+1,z+1}};
(* {{min, max}, {indexes}} *)
buildIntervalNode[its_]:=Module[
	{m,L,C,R},
	m=Median[Flatten[P[its,{All,1}]]];
	{L,C,R}=GatherBy[
		Join[{
			{{-Infinity,-Infinity}},
			{{m,m}},
			{{Infinity,Infinity}}},its],
		If[P[#,{1,2}]<m,1,If[P[#,{1,1}]>m,2,3]]&];
	L=Rest[L];C=Rest[C];R=Rest[R];
	{
		m,(* median value *)
		If[Length[L]!=0,buildIntervalNode[L]],(* Smaller than m *)
		If[Length[R]!=0,buildIntervalNode[R]],(* Greater than m *)
		C, (* Cantains m, min-end increase *)
		SortBy[C,-P[#,{1,2}]&] (* Cantains m, max-end decrease *)
	}
];
buildIntervalTree[img_]:=Module[
	{D=Dimensions[img]},
	buildIntervalNode[SortBy[
		P[Reap[If[Length[D]==2,
			Do[Sow[{MinMax[P[img,V2D[x,y]]],{x,y}}],{x,P[D,1]-1},{y,P[D,2]-1}],
			Do[Sow[{MinMax[P[img,V3D[x,y,z]]],{x,y,z}}],{x,P[D,1]-1},{y,P[D,2]-1},{z,P[D,3]-1}]]],
		{2,1}],
	P[#,{1,1}]&]]
];
Q[root_,iso_,q_:Sow]:=
		If[iso<P[root,1],
			Do[If[P[i,{1,1}]>iso,Break[],q[P[i,2]]],{i,P[root,4]}];
			If[Length[P[root,2]]!=0,Q[P[root,2],iso,q]],
		If[iso>P[root,1],
			Do[If[P[i,{1,2}]<iso,Break[],q[P[i,2]]],{i,P[root,5]}];
			If[Length[P[root,3]]!=0,Q[P[root,3],iso,q]],
		Do[q[P[i,2]],{i,P[root,4]}]
		]];


(*MV2D[x_,y_]:={
	{{x,y},{x+1,y},{2x,2y-1}},
	{{x+1,y},{x+1,y+1},{2x+1,2y}},
	{{x+1,y+1},{x,y+1},{2x,2y+1}},
	{{x,y+1},{x,y},{2x-1,2y}}
};
ME2D={
	{},
	{{1,4}},
	{{1,2}},
	{{2,4}},
	{{2,3}},
	{{1,2},{3,4}},
	{{1,3}},
	{{3,4}},
	{{3,4}},
	{{1,3}},
	{{1,2},{3,4}},
	{{2,3}},
	{{2,4}},
	{{1,2}},
	{{1,4}},
	{}
};
contour2DM[img_,iso_]:=Module[
{X,Y,M,T,d,k,t,s},
	{X,Y}=Dimensions[img];
	M=Table[0,2X-1,2Y-1];
	k=0;
	P[Reap[Do[
		T=MV2D[x,y];
		Do[If[(P[img,P[i,1]]>iso\[Xor]P[img,P[i,2]]>iso)&&P[M,P[i,3]]==0,
			M[[P[i,{3,1}],P[i,{3,2}]]]=++k;
			Sow[P[i,1]+((iso-P[img,P[i,1]])/(P[img,P[i,2]]-P[img,P[i,1]]))(P[i,2]-P[i,1])-{0.5,0.5},v];
		],{i,T}];
		d=1;t=1;
		Do[If[P[img,P[i,1]]<=iso,t+=d];d*=2,{i,T}];
		Do[Sow[P[M,T[[i,3]]],e],{i,P[ME2D,t]}],
	{x,X-1},{y,Y-1}]],2]
];*)


cycles=Get["M3/dat/cycles.txt"];
EDGE={
	Function[{x,y},{
		{{x,y},{x+1,y},{2x,2y-1}},
		{{x+1,y},{x+1,y+1},{2x+1,2y}},
		{{x+1,y+1},{x,y+1},{2x,2y+1}},
		{{x,y+1},{x,y},{2x-1,2y}}
	}],
	Function[{x,y,z},{
		{{x+1,y+1,z+1},{x+1,y+1,z},{2x+1,2y+1,2z}},
		{{x+1,y+1,z},{x+1,y,z},{2x+1,2y,2z-1}},
		{{x+1,y,z+1},{x+1,y+1,z+1},{2x+1,2y,2z+1}},
		{{x+1,y,z},{x+1,y,z+1},{2x+1,2y-1,2z}},
		
		{{x,y+1,z+1},{x,y+1,z},{2x-1,2y+1,2z}},
		{{x,y+1,z},{x,y,z},{2x-1,2y,2z-1}},
		{{x,y,z+1},{x,y+1,z+1},{2x-1,2y,2z+1}},
		{{x,y,z},{x,y,z+1},{2x-1,2y-1,2z}},
		
		{{x+1,y+1,z+1},{x,y+1,z+1},{2x,2y+1,2z+1}},
		{{x+1,y+1,z},{x,y+1,z},{2x,2y+1,2z-1}},
		{{x+1,y,z},{x,y,z},{2x,2y-1,2z-1}},
		{{x+1,y,z+1},{x,y,z+1},{2x,2y-1,2z+1}}
	}]
};
TRANS={
	{00,08,06,00,11,00,00,00},
	{08,00,00,07,00,12,00,00},
	{06,00,00,05,00,00,10,00},
	{00,07,05,00,00,00,00,09},
	{11,00,00,00,00,04,02,00},
	{00,12,00,00,04,00,00,03},
	{00,00,10,00,02,00,00,01},
	{00,00,00,09,00,03,01,00}
};
LOOKUP={
	{
		{},
		{{1,4}},
		{{1,2}},
		{{2,4}},
		{{2,3}},
		{{1,2},{3,4}},
		{{1,3}},
		{{3,4}},
		{{3,4}},
		{{1,3}},
		{{1,2},{3,4}},
		{{2,3}},
		{{2,4}},
		{{1,2}},
		{{1,4}},
		{}
	},
	Map[P[TRANS,#]&,cycles,{3}]
};


contour[img_,itt_,iso_,m_:"M"]:=Module[
	{D,R,M,A,q,k=0,pad,set,V},
	D=Dimensions[img];R=Length[D];
	V=If[R==2,4,8];
	pad=Table[.5,R];
	M=Table@@Join[{0},2D-1];
	set=If[R==2,
		Function[{x,y},M[[x,y]]=++k],
		Function[{x,y,z},M[[x,y,z]]=++k]];
	q=Module[
		{T,d=1,t=1},
		T=P[EDGE,R-1]@@#;
		Do[If[(P[img,P[i,1]]>iso\[Xor]P[img,P[i,2]]>iso)&&P[M,P[i,3]]==0,
			set@@P[i,3];
			Sow[P[i,1]+((iso-P[img,P[i,1]])/(P[img,P[i,2]]-P[img,P[i,1]]))(P[i,2]-P[i,1])-pad,v];
		],{i,T}];
		Do[If[P[img,P[T,{i,1}]]<=iso,t+=d];d*=2,{i,V}];
		Do[Sow[P[M,T[[i,3]]],e],{i,P[LOOKUP,{R-1,t}]}]
	]&;
	A=P[Reap[Q[itt,iso,q]],2]
];


End[];
EndPackage[];
