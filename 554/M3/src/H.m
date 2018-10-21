(* ::Package:: *)

BeginPackage["H3`"];


showCurve;
showSurface;


Begin["`P`"];


showCurve[CC_,col_:RGBColor[0,0,1]]:=Module[{V,E},
	If[CC!={},{V,E}=CC;Graphics[{col,Map[Line[V[[#]]]&,E]},Frame->False]]];


showSurface[CC_,col_:RGBColor[0,0,1]]:=Module[{V,T},
	If[CC!={},{V,T}=CC;Graphics3D[{col,EdgeForm[{}],Map[Polygon[V[[#]]]&,T]},RotationAction->Clip,Lighting->"Neutral"]]];


End[];
EndPackage[];
