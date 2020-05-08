(* ::Package:: *)

BeginPackage["AlgebricUtil`"]


matrixConstructor::usage = "matrixConstructor[Dimension]. Creates a square matrix of given dimension, initialized to zero values"


Begin["`Private`"]


matrixConstructor[dim_] := Module[{i,j,m,d=dim},
	m = List[];
	For[i=1,i<= d,i++,
		AppendTo[m,List[]];
		For[j=1,j<= d,j++,
			AppendTo[m[[i]],0];
		];
	];
	Return[m]
];


End[]


EndPackage[]
