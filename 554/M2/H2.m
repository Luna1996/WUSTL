(* ::Package:: *)

BeginPackage["H2`"];


showCC2D;
showCC2DImg;
showCC3D;
showCC3DExt;
traceFace;


Begin["`P`"];


traceFace[edgelist_]:=
  Module[{face=edgelist[[1]],p,nlist=Drop[edgelist,1]},
    While[Length[nlist]>1,
      p=Position[nlist,Last[face]];
      AppendTo[face,nlist[[p[[1,1]],3-p[[1,2]]]]];
      nlist=Drop[nlist,{p[[1,1]]}]];
    face];


getScale[cc_]:=Max[Map[(Max[#]-Min[#]+1)&,Transpose[cc[[1]]]]];


getExteriorFaces[cc_]:=Module[{val},
  val=Map[0&,cc[[3]]];
  Scan[Scan[val[[#]]++&,#]&,cc[[4]]];
  Pick[cc[[3]],Map[(#<2)&,val]]];


(* ::Text:: *)
(*2D*)


ptSize2D=0.02;
ptColor2D=GrayLevel[0];
ptColorOri2D=GrayLevel[0.85];
edgeSize2D=0.01;
edgeColor2D=GrayLevel[.2];
edgeColorOri2D=GrayLevel[0.9];
faceColor2D=GrayLevel[0.5];
faceColorOri2D=GrayLevel[0.95];


showCC2D[cc_]:=
	Module[{scl=getScale[cc]/10.0,len=Length[cc]},
		Graphics[{
			If[len>=3,{faceColor2D,EdgeForm[{}],Map[Polygon[cc[[1,traceFace[cc[[2,#]]]]]]&,cc[[3]]]},{}],
			If[len>=2,{edgeColor2D,Thickness[edgeSize2D/scl],Map[Line[cc[[1,#]]]&,cc[[2]]]},{}],
		{ptColor2D,PointSize[ptSize2D/scl],Map[Point,cc[[1]]]}}]];


showCC2D[cc1_,cc2_]:=
	Module[{scl=getScale[cc2]/10.0,len1=Length[cc1],len2=Length[cc2]},
		Graphics[{
			If[len2>=3,{faceColorOri2D,EdgeForm[{}],Map[Polygon[cc2[[1,traceFace[cc2[[2,#]]]]]]&,cc2[[3]]]},{}],
			If[len2>=2,{edgeColorOri2D,Thickness[edgeSize2D/scl],Map[Line[cc2[[1,#]]]&,cc2[[2]]]},{}],
			{ptColorOri2D,PointSize[ptSize2D/scl],Map[Point,cc2[[1]]]},
			If[len1>=3,{faceColor2D,EdgeForm[{}],Map[Polygon[cc1[[1,traceFace[cc1[[2,#]]]]]]&,cc1[[3]]]},{}],
			If[len1>=2,{edgeColor2D,Thickness[edgeSize2D/scl],Map[Line[cc1[[1,#]]]&,cc1[[2]]]},{}],
			{ptColor2D,PointSize[ptSize2D/scl],Map[Point,cc1[[1]]]}}]];


showCC2DImg[cc_,img_]:=
	Module[{scl=getScale[cc]/10.0,len=Length[cc]},
		Show[{Graphics[Raster[img/Max[img]],Frame->False],Graphics[{
			If[len>=3,{RGBColor[1,0,0],EdgeForm[{}],Map[Polygon[cc[[1,traceFace[cc[[2,#]]]]]]&,cc[[3]]]},{}],
			If[len>=2,{RGBColor[1,0,0],Thickness[edgeSize2D/scl],Map[Line[cc[[1,#]]]&,cc[[2]]]},{}],
			{RGBColor[1,0,0],PointSize[ptSize2D/scl],Map[Point,cc[[1]]]}}]}]];


(* ::Text:: *)
(*3D*)


ptSize3D=0.02;
ptColor3D=GrayLevel[0];
edgeSize3D=0.01;
edgeColor3D=GrayLevel[0];
faceColor3D=GrayLevel[0.5];
opaqe3D=0.2;


showCC3D[cc_]:=
	Module[{scl=getScale[cc]/10.0,len=Length[cc]},
		Graphics3D[{
			If[len>=3,{faceColor3D,EdgeForm[{}],Map[Polygon[cc[[1,traceFace[cc[[2,#]]]]]]&,cc[[3]]]},{}],
			If[len>=2,{edgeColor3D,Thickness[edgeSize3D/scl],Map[Line[cc[[1,#]]]&,cc[[2]]]},{}],
			{ptColor3D,PointSize[ptSize3D/scl],Map[Point,cc[[1]]]}},
			Boxed->False,RotationAction->Clip,Lighting->"Neutral"]];


showCC3DExt[cc_]:=
	If[Length[cc]<4||Length[cc[[4]]]==0,showCC3D[cc],
	Module[{scl=getScale[cc]/10.0,len=Length[cc]},
		Graphics3D[{
			If[len>=3,{faceColor3D,EdgeForm[{edgeColor3D,Thickness[edgeSize3D/scl]}],Map[Polygon[cc[[1,traceFace[cc[[2,#]]]]]]&,getExteriorFaces[cc]]},{}]},
			Boxed->False,RotationAction->Clip,Lighting->"Neutral"]]];


showCC3D[cc1_,cc2_]:=
	Module[{scl=getScale[cc2]/10.0,len1=Length[cc1],len2=Length[cc2]},
		Graphics3D[{{Opacity[opaqe3D],
			If[len2>=3,{faceColor3D,EdgeForm[{}],Map[Polygon[cc2[[1,traceFace[cc2[[2,#]]]]]]&,getExteriorFaces[cc2]]},{}]},
			If[len1>=3,{faceColor3D,EdgeForm[{}],Map[Polygon[cc1[[1,traceFace[cc1[[2,#]]]]]]&,cc1[[3]]]},{}],
			If[len1>=2,{edgeColor3D,Thickness[edgeSize3D/scl],Map[Line[cc1[[1,#]]]&,cc1[[2]]]},{}],
			{ptColor3D,PointSize[ptSize3D/scl],Map[Point,cc1[[1]]]}},
			Boxed->False,RotationAction->Clip,Lighting->"Neutral"]];


End[];
EndPackage[];
