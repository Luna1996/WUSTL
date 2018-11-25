(* ::Package:: *)

BeginPackage["A4`"];


preFair;
fair;
getCoefficients;
getCoefficient;
getMinimizerQEM;
simplify2D;


Begin["`P`"];
P=Global`P;


(* ::Text:: *)
(*Algorithm 1 & 2*)


preFair[E_,l_]:=Flatten[P[Reap[Scan[Function[{e},
		Scan[Function[{i},
			Scan[(If[i!=#,Sow[#,i]])&,e]],e]],E],Range[l]],2],1];


fair[{V_,E_},num_,\[Lambda]_:.5,\[Kappa]_:.1,PT_:Null]:=Module[
	{l,T,V1,V2,k,f},
	l=Length[V];
	T=If[ListQ[PT],PT,preFair[E,l]];
	V1=V;
	V2=V;
	k=1/(\[Kappa]-1/\[Lambda]);
	f[t_]:=(Do[V2[[i]]=(1-t)P[V1,i]+t (Total[V1[[P[T,i]]]])/Length[P[T,i]],{i,l}];V1=V2;);
	Do[f[\[Lambda]];f[k],num];
	{V1,E}
];


(* ::Text:: *)
(*Algorithm 3 & 5*)


getCoefficients[l_]:=Module[
	{L,D,a,b,c},
	If[Length[l]==0,Return[]];
	{L,a,D}=Dimensions[l];
	a=Table[0,D,D];
	Do[a[[m,n]]=Sum[P[l,{i,2,m}]*P[l,{i,2,n}],{i,L}],{m,D},{n,D}];
	b=Table[0,D];
	Do[b[[m]]=Sum[P[l,{i,2,m}]*(P[l,{i,1}].P[l,{i,2}]),{i,L}],{m,D}];
	c=Sum[(P[l,{i,1}].P[l,{i,2}])^2,{i,L}];
	{a,b,c}
];


getCoefficient[Q_,N_]:=Module[
	{D,a,b,c},
	D=Length[Q];
	a=Table[P[N,m]*P[N,n],{m,D},{n,D}];
	c=N.Q;
	b=Table[P[N,m]*c,{m,D}];
	c=c*c;
	{a,b,c}
];


getMinimizerQEM[{a_,b_,c_}]:=Module[
	{p,e},
	p=Inverse[a].b;
	e=p.a.p-2p.b+c;
	{p,e}];


(* ::Text:: *)
(*Algorithm 4*)


norm2D[{x_,y_}]:={-y,x}/\[Sqrt](x*x+y*y);


preSimplify2D[E_,l_]:=Flatten[P[Reap[Do[Scan[Sow[i,#]&,P[E,i]],{i,Length[E]}],Range[l]],2],1];


(* {i,{a,b,c},{p,e}} *)
simplify2D[curve_,num_]:=Module[
	{V,E,MV,ME,l,R,D,tc,ic,ik,id,tl,il,tn,in},
	{V,E}=curve;
	MV=Table[1,Length[V]];
	ME=Table[1,Length[E]];
	l=Length[V]-num;
	R=preSimplify2D[E,Length[V]];
	D=Table[{0,0,0},Length[E]];
	Do[
		D[[i,1]]=i;
		D[[i,2]]=getCoefficient[P[V,P[E,{i,1}]],norm2D[P[V,P[E,{i,1}]]-P[V,P[E,{i,2}]]]],
	{i,Length[E]}];
	Do[
		D[[i,3]]=getMinimizerQEM[Total[D[[Union@@R[[P[E,i]]],2]]]],
	{i,Length[E]}];
	
	Do[
		tc=P[MinimalBy[D,(If[P[ME,P[#,1]]==1,P[#,{3,2}],Infinity])&],1];
		ic=P[tc,1];
		ik=P[E,{ic,1}];
		id=P[E,{ic,2}];
		ME[[ic]]=0;
		MV[[id]]=0;
		V[[ik]]=P[tc,{3,1}];
		{tl,tn}=R[[P[E,ic]]];
		il=If[P[tl,1]==ic,P[tl,2],P[tl,1]];
		in=If[P[tn,1]==ic,P[tn,2],P[tn,1]];
		R[[ik]]={il,in};
		If[P[E,{in,1}]==id,E[[in,1]]=ik,E[[in,2]]=ik];
		D[[il,2]]=getCoefficient[P[V,ik],norm2D[P[V,P[E,{il,1}]]-P[V,P[E,{il,2}]]]];
		D[[in,2]]=getCoefficient[P[V,ik],norm2D[P[V,P[E,{in,1}]]-P[V,P[E,{in,2}]]]];
		D[[il,3]]=getMinimizerQEM[Total[D[[Union@@R[[P[E,il]]],2]]]];
		D[[in,3]]=getMinimizerQEM[Total[D[[Union@@R[[P[E,in]]],2]]]];,
	l];	

	
	V=P[Reap[Do[If[P[MV,i]==1,Sow[P[V,i]]],{i,Length[V]}]],{2,1}];
	l=1;
	Do[If[P[MV,i]==1,MV[[i]]=l++],{i,Length[MV]}];
	E=P[Reap[Do[If[P[ME,i]!=0,Sow[MV[[P[E,i]]]]],{i,Length[E]}]],{2,1}];
	{V,E}
];


End[];
EndPackage[];
