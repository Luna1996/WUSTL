(* ::Package:: *)

BeginPackage["H1`"];
showGrayImage;
showBinaryImage;
showBinaryOverlay;
showGrayOverlay;
Begin["`P`"];


showGrayImage[img_]:=Graphics[Raster[img/Max[img]],Frame->False];


showBinaryImage[img_]:=Graphics[Raster[img],Frame->False];


showBinaryOverlay[img_,mask_]:=Graphics[Raster[MapThread[MapThread[If[#2==1,{1,0,0},{#1,#1,#1}]&,{#1,#2}]&,
{img,mask}]],Frame->False];


showGrayOverlay[img_,mask_]:=Graphics[{Raster[img/Max[img]],Raster[Map[Map[{#,0,0,If[#==1,.5,0]}&,#]&,mask]]},Frame->False];


End[];
EndPackage[];
