(* ::Package:: *)

BeginPackage["AlgebricUtil`"]


matrixConstructor::usage = "matrixConstructor[Dimension]. Creates a square matrix of given dimension, initialized to zero values"


swapOperation::usage = "swapOperation[Matrix,First,Second]. Returns a copy of Matrix with rows First and Second swapped"


sumOperation::usage = "sumOperation[Matrix,First,Second,C1,C2]. Returns a copy of Matrix with row First replaced by the sum C1*First+C2*Second"


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
swapOperation[matrice_,lop_,rop_]:=Module[{m = matrice,r1 =lop,r2 = rop,i,len,swap},
	len =Length[m[[1]]];
	For[i=1,i<= len,i++,
		swap = m[[r1,i]];
		With[{val  = swap},
			m[[r1,i]] = m[[r2,i]];
			m[[r2,i]] = val;
		];(*<With*)
	];(*<For*)
	Return[m];
];
sumOperation[matrice_,lop_,rop_,c1_,c2_]:=Module[{m = matrice,r1 =lop,r2 = rop,i,len,C1 =c1,C2=c2},
	len =Length[m[[1]]];
	For[i=1,i<= len,i++,
		m[[r1,i]] = m[[r1,i]]*C1 + m[[r2,i]]*C2;
	];
	Return[m];
];


End[]


EndPackage[]
