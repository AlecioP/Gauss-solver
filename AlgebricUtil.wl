(* ::Package:: *)

BeginPackage["AlgebricUtil`"]


matrixConstructor::usage = "matrixConstructor[Dimension]. Creates a square matrix of given dimension, initialized to zero values"


swapOperation::usage = "swapOperation[Matrix,First,Second]. Returns a copy of Matrix with rows First and Second swapped"


sumOperation::usage = "sumOperation[Matrix,First,Second,C1,C2]. Returns a copy of Matrix with row First replaced by the sum C1*First+C2*Second"


solveMatrix::usage = ""


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
solveMatrix[m_] := Module[{i,j,len,c1,c2,opp,matrice = m,hint,op},
(*{"swap",1,2} "swap,1,2"
{"sum",1,2,5,6}*)
	hint = "";
	len = Length[matrice[[1]]];
	For[i=1,i<= len,i++,
		If[matrice[[i,i]]==0,
			(*SWAP*)
			For[j=i+1,j<=len,j++,
				If[matrice[[j,i]]!=0,
					matrice = swapOperation[matrice,j,i];
					op = "Swap Row "<>ToString[j]<>" and Row "<>ToString[i];
					hint = hint<>op;
					If[$OperatingSystem == "Windows", 
						(*Concatenate LF CR*)
						hint<>FromCharacterCode[10]<>FromCharacterCode[13];
					,(*else*)
						hint = hint <>"\n";
					];(*If*)
					i=i-1;
					Break[];	
				];(*<IF*)
			];(*<FOR*)
			,(*SUM*)
			For[j=i+1,j<=len,j++,
				If[matrice[[j,i]]==0,Continue[];];
				opp = - matrice[[i,i]];
				c1 = opp/matrice[[j,i]];
				c2 = 1;
				matrice = sumOperation[matrice,j,i,c1,c2];
				op = "Sum "<>ToString[c1,InputForm]<>" times Row "<>ToString[j]<>" and "<>ToString[c2,InputForm]<>" times Row "<>ToString[i];
				hint = hint<>op;
				If[$OperatingSystem == "Windows", 
						(*Concatenate LF CR*)
						hint<>FromCharacterCode[10]<>FromCharacterCode[13];
				,(*else*)
						hint = hint <>"\n";
				];(*If*)
			];(*<For*)
		];(*<If*)	
	];(*<For*)
	Return[hint];
];


End[]


EndPackage[]
