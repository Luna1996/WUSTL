(* ::Package:: *)

BeginPackage["M1`"];
Global`Load["H1","M0"];
open;
morphoDynamic;
showBinaryImageComponent;
maskComponents;
labelComponents;
showLargestComponents;
Begin["`P`"];


(* ::Text:: *)
(*Algorithm 1*)


struct=<|
  "2D4"->{{0,1},{0,-1},{1,0},{-1,0}},
  "2D8"->{{-1,-1},{-1,0},{-1,1},{0,1},{1,1},{1,0},{1,-1},{0,-1}},
  "2D12"->{{-1,-1},{-1,0},{-1,1},{0,1},{1,1},{1,0},{1,-1},{0,-1},{0,-2},{-2,0},{0,2},{2,0}}
|>;


grow[mode_,img_,conn_:"2D8",r_:1]:=
  Module[{X,Y,S,str,i,j},
    {X,Y}=Dimensions[img];
    str=struct[conn];
    S=Length[str];
    Nest[Table[
      If[Do[
        If[#[[x,y]]!=mode,
          i=x+str[[s,1]];
          If[1<=i<=X,j=y+str[[s,2]];If[1<=j<=Y,
            If[#[[i,j]]==mode,Return[True]]
        ]]],
      {s,S}],mode,#[[x,y]],#[[x,y]]],
    {x,X},{y,Y}]&,img,r]];


erode=grow[0,##]&;
dilate=grow[1,##]&;
open=(dilate[erode[##],##2])&;
close=(erode[dilate[##],##2])&;


morphoDynamic[img_]:=Manipulate[showBinaryImage[
Switch[operator,
"Erode",erode,
"Dilate",dilate,
"Open",open,
"Close",close]
[img,Switch[element,"ExSquare","2D12","Square","2D8","Cross","2D4"],t]],{t,0,5,1},
{operator,{"Erode","Dilate","Open","Close"}},{element,{"ExSquare","Square","Cross"}}]


(* ::Text:: *)
(*Algorithm 2*)


flood[img_,{x_,y_},conn_]:=
  Module[{o,rtn,str,l,S,X,Y,i,j,n},
    o=img[[x,y]];
    rtn=Map[(0&),img,{2}];
    str=struct[conn];
    S=Length[str];
    X=Length[img];
    Y=Length[img[[1]]];
    l={{x,y}};
    n=0;
    While[Length[l]>0,
      {i,j}=l[[1]];
      l=Rest[l];
      If[1<=i<=X&&1<=j<=Y&&rtn[[i,j]]==0&&img[[i,j]]==o,
        n=n+1;rtn[[i,j]]=1;
      Do[AppendTo[l,{i+str[[s,1]],j+str[[s,2]]}],{s,S}]]
    ];
  {rtn,n}];


showBinaryImageComponent[img_]:=Manipulate[showBinaryOverlay[img,
flood[img,Ceiling[Reverse[pt]],Switch[object,
"8-connected","2D8",
"4-connected","2D4"]][[1]]],{{pt,Reverse[Dimensions[img]/2]},{1,1},Reverse[Dimensions[img]],Locator},{object,{"8-connected","4-connected"}}]


(* ::Text:: *)
(*Algorithm 3*)


labelComponents[img_,conn_]:=
  Module[{X,Y,S,str,rtn,L,l,f,n,t,T,i,j},
    X=Length[img];
    Y=Length[img[[1]]];
    str=struct[conn];
    S=Length[str];
    rtn=Table[0,{x,X},{y,Y}];
    L=0;
    Do[If[rtn[[x,y]]==0&&img[[x,y]]==1,
      L=L+1;n=0;
      l={{x,y}};
      While[Length[l]>0,
        {i,j}=l[[1]];
        l=Rest[l];
        If[1<=i<=X&&1<=j<=Y&&rtn[[i,j]]==0&&img[[i,j]]==1,
          n=n+1;rtn[[i,j]]=L;
          Do[AppendTo[l,{i+str[[s,1]],j+str[[s,2]]}],{s,S}]]
      ];
      t[L]=n],{x,X},{y,Y}];
    T=Table[{n,t[n]},{n,L}];
    T=Sort[T,(#1[[2]]>#2[[2]]&)];
    rtn=rtn/.Table[T[[n,1]]->n,{n,Length[T]}];
  {rtn,L}];


(* ::Text:: *)
(*Algorithm 4*)


numberComponents[img_,conn_]:=labelComponents[img,conn][[2]];


maskComponents[img_,conn_,ls_]:=
Module[{m1,l},
{m1,l}=labelComponents[img,conn];
m1/.Table[i->If[MemberQ[ls,i],1,0],{i,l}]
];


getLargestComponents[img_,k_,conn_]:=Map[(If[1<=#<=k,1,0]&),labelComponents[img,conn][[1]],{2}];


showLargestComponents[img_]:=
Manipulate[
showBinaryOverlay[threshold[img,v],getLargestComponents[threshold[img,v],k,object]],
{{v,0.5},0,1},
{{k,1},1,numberComponents[threshold[img,v],object],1},
{object,{"2D8","2D4"}}]


End[];
EndPackage[];
