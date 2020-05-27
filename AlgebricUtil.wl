(* ::Package:: *)

(* :Title: AlgebricUtil *)
(* :Context: AlgebricUtil` *)
(* :Author: Andrea Ercolessi, Alessio Portaro, Francesco Antimi *)
(* :Summary: Package contenente le funzioni di utilit\[AGrave] richiamate nel Notebook final.nb *)
(* :Copyright: Andrea Ercolessi, Alessio Portaro, Francesco Antimi  2020 *)
(* :Package Version: 23, May 2020 *)
(* :Mathematica Version: 12.1 *)
(* :History: *)
(* :Sources: biblio *)
(* :Limitations:
this is a preliminary version, for educational purposes only. *)
(* :Discussion: *)


BeginPackage["AlgebricUtil`"]


matrixConstructor::usage = "matrixConstructor[Dimension]. Creates a square matrix of given dimension, initialized to zero values"


swapOperation::usage = "swapOperation[Matrix,First,Second]. Returns a copy of Matrix with rows First and Second swapped"


sumOperation::usage = "sumOperation[Matrix,First,Second,C1,C2]. Returns a copy of Matrix with row First replaced by the sum C1*First+C2*Second"


solveMatrix::usage = ""


Begin["`Private`"]


(*Creazione dei una matrice di dimensione dim. La matrice serve per memorizzare i dati inseriti dall'utente  *)
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
(*Funzione resituisce una matrice identica a quella di input ma con le righe R1 e R2 invertite.
  Gli indici delle righe da scambiare sono passati come parametri input*)
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
(*Funzione resituisce una matrice identica a quella di input ma con la riga R1 sostituita dal risultato dell'operazione C1*R1 + C2*R2.
  C1 e C2 sono due scalari passati in input alla funzione. R1 e R2 sono gli indici di riga passati in input*)
sumOperation[matrice_,lop_,rop_,c1_,c2_]:=Module[{m = matrice,r1 =lop,r2 = rop,i,len,C1 =c1,C2=c2},
	len =Length[m[[1]]];
	For[i=1,i<= len,i++,
		m[[r1,i]] = m[[r1,i]]*C1 + m[[r2,i]]*C2;
	];
	Return[m];
];
(*Funzione che restituisce una matrice di stringhe. Ogni stringa rappresenta uno step dell'algoritmo di risoluzione della matrice di input.
  Il testo restituito verr\[AGrave] visualizzato in un pannello dell'interfaccia grafica qualora l'utente richeda dei suggerimenti*)
solveMatrix[m_] := Module[{i,j,len,c1,c2,opp,matrice = m,hint,op},
(*{"swap",1,2} "swap,1,2"
{"sum",1,2,5,6}*)
	hint = List[];
	
	len = Length[matrice[[1]]];
	For[i=1,i<= len,i++,
		If[matrice[[i,i]]==0,
			(*SWAP*)
			For[j=i+1,j<=len,j++,
				If[matrice[[j,i]]!=0,
					matrice = swapOperation[matrice,j,i];
					op = "Swap Row "<>ToString[j]<>" and Row "<>ToString[i];
					hint=AppendTo[hint,List[op]];
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
				hint=AppendTo[hint,List[op]];
			];(*<For*)
		];(*<If*)	
	];(*<For*)
	Return[Grid[hint]];
];
solveMatrixAlg1[m_] := Module[{i,j,len,c1,c2,opp,matrice = m,hint,op,colMax,valMax,iter},
	hint = List[];
	len = Length[matrice[[1]]];
	For[i=1,i<= len,i++,
	
		(*Check if the current row contains pivot element with maximum absolute value. 
		Otherwise swap the row containing the effective maximum with the current row*)
		colMax = i;
		valMax = Abs[matrice[[i,i]]];
		(*Search for the maximum value*)
		For[iter=i,iter<=len,iter++,
			If[Abs[matrice[[iter,i]](*<PART*)](*<abs*)>valMax,
				colMax = iter;
				valMax = Abs[matrice[[iter,i]](*<PART*)](*<abs*);
			];(*If*)
		];(*<For*)
		
		(*Swap maximum value row*)
		If[i!=colMax,
			matrice = swapOperation[matrice,i,colMax];
			op = "Swap Row "<>ToString[i]<>" and Row "<>ToString[colMax];
			hint=AppendTo[hint,List[op]];
		];(*<If*)
		
		If[matrice[[i,i]]==0,
			(*SWAP*)
			For[j=i+1,j<=len,j++,
				If[matrice[[j,i]]!=0,
					matrice = swapOperation[matrice,j,i];
					op = "Swap Row "<>ToString[j]<>" and Row "<>ToString[i];
					hint=AppendTo[hint,List[op]];
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
				hint=AppendTo[hint,List[op]];
			];(*<For*)
		];(*<If*)	
	];(*<FOR*)
	Return[Grid[hint]];
];


End[]


EndPackage[]
