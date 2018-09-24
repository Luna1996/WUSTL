(* ::Package:: *)

GenKey[L_]:=Table[RandomInteger[255],L];


ECB[img_,key_]:=Module[{L=Length[key],i},i=L;Map[(BitXor[#,key[[i=1+Mod[i,L]]]]&),img,{3}]];


CBC[img_,key_]:=Module[{L=Length[key],i,v},
	i=L;v=Table[RandomInteger[255],L];
	Map[(i=1+Mod[i,L];v[[i]]=BitXor[key[[i]],v[[i]],#])&,img,{3}]];


SetDirectory[NotebookDirectory[]];
img1=ImageData[Import["img_original.jpg"],"Byte"];


Image[img1,"Byte"]


Image[ECB[img1,GenKey[128]],"Byte"]


Image[CBC[img1,GenKey[128]],"Byte"]
