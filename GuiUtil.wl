(* ::Package:: *)

BeginPackage["GuiUtil`"]


createMatrix::usage = "createMatrix[Dimension,Matrix]. Create gui to input a square matrix of given dimension"


Begin["`Private`"]


createMatrix[dim_,matrix_] := Module[{i,j,m,ph,d=dim},
	m = List[];
	For[i=1,i <= d,i++,
		AppendTo[m,List[]];
		For[j=1, j<=d,j++,
			(*placeholder*)
			ph = "M["<>ToString[i]<>","<>ToString[j]<>"]";
			With[{i0 = i,j0 = j},
				AppendTo[m[[i]],InputField[Dynamic[matrix[[i0,j0]]](*Dynamic*),Number,FieldHint->ph,FieldSize->{4,1}](*InputField*)];(*AppendTo*)
			](*With*)
		];(*For2*)
	];(*For1*)
	Return[m];
];(*Module*)


End[]


EndPackage[]
