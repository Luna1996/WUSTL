(* ::Package:: *)

BeginPackage["A5`"];


getCentroid;
getPCA;
alignPCA;
alignSVD;
getClosestPoints;
alignICP;
alignCC;
deformLaplace;
deformLaplaceRot;
alignLaplace;


Begin["`P`"];


Needs["A4`","M4/src/A.m"];


P=Global`P;


(* ::Text:: *)
(*PCA*)


getCentroid[pts_]:=Mean[pts];


TransM={
	{{-1,0},{0,-1}},
	{{1,0,0},{0,-1,0},{0,0,-1}},
	{{-1,0,0},{0,1,0},{0,0,-1}},
	{{-1,0,0},{0,-1,0},{0,0,1}}
};
getPCA[pts_,cent_]:=Module[
	{P},
	P=Map[(#-cent)&,pts];
	Eigenvectors[P\[Transpose].P]
	];


alignPCA[ptsSrc_,ptsTgt_]:=Module[
	{C1,C2,A1,A2,R,As},
	C1=getCentroid[ptsSrc];
	A1=getPCA[ptsSrc,C1]\[Transpose];
	C2=getCentroid[ptsTgt];
	A2=getPCA[ptsTgt,C2]\[Transpose];
	If[Length[ptsSrc[[1]]]==2,
		As=Table[0,2];
		As[[1]]=A1;
		As[[2]]=TransM[[1]].A1,
		As=Table[0,4];
		As[[1]]=A1;
		As[[2]]=TransM[[2]].A1;
		As[[3]]=TransM[[3]].A1;
		As[[4]]=TransM[[4]].A1;
	];
	R=Map[(A2.#\[Transpose])&,As];
	R=MaximalBy[R,Tr][[1]];
	Map[(C2+R.(#-C1))&,ptsSrc]]


(* ::Text:: *)
(*SVD*)


alignSVD[ptsSrc_,ptsTgt_]:=Module[
	{C1,C2,R,U,V,W},
	C1=getCentroid[ptsSrc];
	C2=getCentroid[ptsTgt];
	{U,W,V}=SingularValueDecomposition[Map[(#-C1)&,ptsSrc]\[Transpose].Map[(#-C2)&,ptsTgt]];
	R=V.U\[Transpose];
	Map[(C2+R.(#-C1))&,ptsSrc]];


(* ::Text:: *)
(*ICP*)


getClosestPoints[ptsSrc_,ptsTgt_]:=Flatten[Nearest[ptsTgt,ptsSrc],1];


RMSD[p1_,p2_,l_]:=Sqrt[Sum[Norm[p1[[i]]-p2[[i]]],{i,l}]/l];
alignICP[ptsSrc_,ptsTgt_,\[Epsilon]_:0.001]:=Module[
	{p1,p2,l=Length[ptsSrc],c=0},
	p1=alignPCA[ptsSrc,ptsTgt];
	p2=getClosestPoints[p1,ptsTgt];
	While[RMSD[p1,p2,l]>0.00001l && c<100,
		c=c+1;
		p1=alignSVD[p1,p2];
		p2=getClosestPoints[p1,ptsTgt];
	];
	p1
];


alignCC[M_,src_,tgt_]:={M[src[[1]],tgt[[1]]],src[[2]]};


(* ::Text:: *)
(*Basic Laplacian deformation*)


deformLaplace[curve_,vinds_,tgts_,w_]:=Module[
	{l1=Length[curve[[1]]],l2=Length[vinds],A,B,R,x},
	A=Table[0,(l1+l2)*2,l1*2];
	B=Table[0,(l1+l2)*2];
	Do[
		A[[i,vinds[[i]]]]=A[[l2+i,l1+vinds[[i]]]]=1;
		B[[i]]=tgts[[i,1]];
		B[[i+l2]]=tgts[[i,2]],
	{i,l2}];
	R=preFair[curve[[2]],l1];
	Do[
		A[[2*l2+i,i]]=A[[2*l2+i+l1,i+l1]]=w;
		A[[2*l2+i,R[[i]]]]=A[[2*l2+i+l1,R[[i]]+l1]]=-w/Length[R[[i]]];
		{B[[2*l2+i]],B[[2*l2+i+l1]]}=w*(curve[[1,i]]-1/Length[R[[i]]]*Total[curve[[1,R[[i]]]]]),
	{i,l1}];
	x=Inverse[Transpose[A].A].Transpose[A].B;
	{Transpose[{x[[1;;l1]],x[[l1+1;;2l1]]}],curve[[2]]}
];


(* ::Text:: *)
(*Laplacian deformation with transformation compensation*)


deformLaplaceRot[{V_,E_},vinds_,tgts_,w_]:=Module[
	{l1=Length[V],l2=Length[vinds],A,B,R,L,\[Delta],D,C,T,Ni,DNi,x,k},
	A=Table[0,(l1+l2)*2,l1*2];
	B=Table[0,(l1+l2)*2];
	Do[
		A[[i,vinds[[i]]]]=A[[l2+i,l1+vinds[[i]]]]=1;
		B[[i]]=tgts[[i,1]];
		B[[i+l2]]=tgts[[i,2]],
	{i,l2}];
	R=preFair[E,l1];
	Do[
		Ni=R[[i]];
		DNi=Length[Ni];
		C=Table[0,2DNi+2];
		C[[{1,2}]]={{V[[i,1]],V[[i,2]],1,0},{V[[i,2]],-V[[i,1]],0,1}};
		\[Delta]=V[[i]]-Total[V[[Ni]]]/DNi;
		D={{\[Delta][[1]],\[Delta][[2]],0,0},{\[Delta][[2]],-\[Delta][[1]],0,0}};
		L=Table[If[OddQ[m+n],0,-1/DNi],{m,2},{n,2DNi+2}];
		L[[1,1]]=L[[2,2]]=1;
		T=Table[0,2DNi+2,2l1];
		T[[1,i]]=T[[2,l1+i]]=1;
		Do[
			k=Ni[[j]];
			C[[2j+{1,2}]]={{V[[k,1]],V[[k,2]],1,0},{V[[k,2]],-V[[k,1]],0,1}};
			T[[2j+1,k]]=T[[2j+2,l1+k]]=1,
		{j,DNi}];
		A[[2(l2+i)+{-1,0}]]=w (L-D.Inverse[(C\[Transpose].C)].C\[Transpose]).T,
	{i,l1}];
	x=Inverse[Transpose[A].A].Transpose[A].B;
	{Transpose[{x[[1;;l1]],x[[l1+1;;2l1]]}],E}
];


(* ::Text:: *)
(*ICP - Laplacian registration*)


alignLaplace[{Vs_,Es_},{Vt_,Et_},w_,\[Epsilon]_]:=Module[
	{p1,p2,l=Length[Vs],vins,i=0},
	vins=Range[l];
	p1=Vs;
	p2=getClosestPoints[p1,Vt];
	While[RMSD[p1,p2,l]>\[Epsilon]&&i<10,
		i=i+1;
		p1=deformLaplace[{p1,Es},vins,p2,w][[1]];
		p2=getClosestPoints[p1,Vt];
	];
	{p1,Es}
];


End[];
EndPackage[];
