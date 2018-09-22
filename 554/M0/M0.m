(* ::Package:: *)

BeginPackage["M0`"];


threshold;


Begin["`P`"];


threshold:=Function[{img,val},Map[(If[#>val,1,0]&),img,{Depth[img]-1}]];


End[];
EndPackage[];
