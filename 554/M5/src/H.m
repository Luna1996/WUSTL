(* ::Package:: *)

BeginPackage["H5`"];


normalize;
perp;
Rot;
RotX;
RotY;
RotZ;
showPoints2D;
showPoints3D;
showCorr2D;
showCorr3D;
showAxes2D;
showAxes3D;
showHandles;


Begin["`P`"];


normalize[v_]:=v/Sqrt[v.v];


perp[{x_,y_}]:={-y,x};


Rot[a_]:=({
 {Cos[a], -Sin[a]},
 {Sin[a], Cos[a]}
});


RotX[a_]:=({
 {1, 0, 0},
 {0, Cos[a], -Sin[a]},
 {0, Sin[a], Cos[a]}
});
RotY[a_]:=({
 {Cos[a], 0, Sin[a]},
 {0, 1, 0},
 {-Sin[a], 0, Cos[a]}
});
RotZ[a_]:=({
 {Cos[a], -Sin[a], 0},
 {Sin[a], Cos[a], 0},
 {0, 0, 1}
});


showPoints2D[pts_,col_:RGBColor[0,0,0]]:=Graphics[{col,Map[Point,pts]},Frame->False,Axes->False];


showPoints3D[pts_,col_:RGBColor[0,0,0]]:=Graphics3D[{col,Map[Point,pts]},Boxed->True,RotationAction->"Clip",Axes->False];


showCorr2D[pts1_,pts2_]:=Graphics[{MapThread[Line[{#1,#2}]&,{pts1,pts2}]}];


showCorr3D[pts1_,pts2_]:=Graphics3D[{MapThread[Line[{#1,#2}]&,{pts1,pts2}]},Boxed->True,RotationAction->"Clip"];


showAxes2D[cent_,axes_]:=Graphics[{
RGBColor[1,0,0],Line[{cent-axes[[1]],cent+axes[[1]]}],
RGBColor[0,1,0],Line[{cent-axes[[2]],cent+axes[[2]]}],
GrayLevel[0],PointSize[0.04],Point[cent]}];


showAxes3D[cent_,axes_]:=Graphics3D[{Sphere[cent,0.05],
RGBColor[1,0,0],Cylinder[{cent-axes[[1]],cent+axes[[1]]},0.02],
RGBColor[0,1,0],Cylinder[{cent-axes[[2]],cent+axes[[2]]},0.02],
RGBColor[0,0,1],Cylinder[{cent-axes[[3]],cent+axes[[3]]},0.02]}];


closeby[p1_,p2_]:=((p1-p2).(p1-p2)<.00001);
showHandles[pts1_,pts2_]:=Module[{len=Length[pts1]},Graphics[{
	Table[{If[closeby[pts1[[i]],pts2[[i]]],Red,Blue],
	PointSize[0.015],Point[pts1[[i]]],PointSize[0.02],Point[pts2[[i]]]},{i,len}]}]];


End[];
EndPackage[];
