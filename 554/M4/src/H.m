(* ::Package:: *)

BeginPackage["H4`"];


showCurveWithPoints;


showSurfaceEdgesOnly;


Begin["`P`"];
P=Global`P;


showCurveWithPoints[{V_,E_},col_:RGBColor[0,0,1]]:=Graphics[{{col,{Map[Line[V[[#]]]&,E]}},Map[Point,V]},Frame->False];


showSurfaceEdgesOnly[{V_,T_},col_:RGBColor[0,0,1]]:=Graphics3D[{col,Map[Line[Append[V[[#]],Last[V[[#]]]]]&,T]},RotationAction->Clip,Lighting->"Neutral",Boxed->False];


End[];
EndPackage[];
