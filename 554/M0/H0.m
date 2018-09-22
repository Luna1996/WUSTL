(* ::Package:: *)

BeginPackage["H0`"];


ImportGray;


Begin["`P`"];


ImportGray=ImageData[ColorConvert[Import[#],"Grayscale"]]&;


End[];
EndPackage[];
