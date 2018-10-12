(* ::Package:: *)

BeginPackage["H3`"];


showCurve;
showSurface;


Begin["`P`"]


showCurve[L_,col_]:=Graphics[{{col,Line[L],Frame->False];
showSurface[F_,col_]:=Graphics3D[{col,Polygon[F]},RotationAction->Clip,Lighting->"Neutral"];


End[];
EndPackage[];
