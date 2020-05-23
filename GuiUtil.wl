(* ::Package:: *)

(* :Title: GuiUtil *)
(* :Context: GuiUtil` *)
(* :Author: Andrea Ercolessi, Alessio Portaro, Francesco Antimi *)
(* :Summary: Package contenente le componenti grafiche presenti nel Notebook final.nb *)
(* :Copyright: Andrea Ercolessi, Alessio Portaro, Francesco Antimi  2020 *)
(* :Package Version: 23, May 2020 *)
(* :Mathematica Version: 12.1 *)
(* :History: *)
(* :Sources: biblio *)
(* :Limitations:
this is a preliminary version, for educational purposes only. *)
(* :Discussion: *)


BeginPackage["GuiUtil`"]


createMatrix::usage = "createMatrix[Dimension,Matrix]. Create gui to input a square matrix of given dimension"


dimensionPanel::usage = "Graphic component to select matrix dimension"


goButton::usage = "Graphic component to start evaluation of expression"


fieldCoef1::usage = "Graphic component to get first equation coefficient from input"


fieldCoef2::usage = "Graphic component to get second equation coefficient from input"


gph::usage = "Graphic Place Holder"


equationbox::usage = "Graphic compenent to visualiza the equation beeing composed"


operB::usage = "Button to select the row to insert into the equation"


rowSelector::usage = "Function to create rows buttons"


rowsB::usage = "Buttons to select the row to insert into the equation"


composedGUI::usage = "Final GUI composed of all the parts into the package"


hintPanel::usage = "Panel containing user help messages to solve matrix"


errorPanel::usage = "Panel containing error messages for the user"


checkInputMatrix::usage = "Check if the user insert valid values for the matrix"


successPanel::usage = "Panel to show success message"


Begin["`Private`"]


(*Componente grafico per implementare una spaziatura tra i componenti di un intefaccia grafica*)
gph := Panel[" ",Background->Gray];


(*Bottoni per selezionare l'operazione corrente da effetuare sulla matrice.*)
operB := Row[{
				Button["Swap",Global`operation="SWAP";,Enabled->Dynamic[If[Global`operation=="SWAP",False,True]]],
				gph,
				Button["Sum",Global`operation = "SUM";,Enabled->Dynamic[If[Global`operation=="SWAP",True,False]]]
}];


(*Pannello nel quale scegliere la dimensione della matrice da creare*)
dimensionPanel := Panel[Grid[{{"Dimension : ",RadioButtonBar[Dynamic[Global`dimension],Range[2,3]]}}],Background->Gray];


(*Funzione creata per verificare lo stato corrente di risoluzione della matrice*)
checkSolved[matrix_] := Module[{i,j,len},
	len = Length[matrix];
	For[i=1,i<=len,i++,
		For[j=1,j<=len,j++,
			(*Lower Triangular's element non zero*)
			If[i>j && matrix[[i,j]]!=0,Return[False];];
		];(*For*)
	];(*For*)
	Return[True];
];


(*Bottone per avviare il calcolo dell'equazione importata*)
goButton := Button["=",Dynamic[
(*CAso in cui l'operazione selezionata risulta essere la somma delle righe*)
	If[Global`operation=="SUM",
	(*Controllo  per verificare se i dati inseriti siano numeri interi o razionali*)
		If[MatchQ[Global`coef1, _Rational | _Integer] && MatchQ[Global`coef2, _Rational | _Integer],
		(*Sostituisco la matrice corrente con il risultato dell'operazione sumOperation *)
			Global`matrice = AlgebricUtil`sumOperation[Global`matrice,
				ToExpression[StringDelete[Global`eqList[[1]],"R"]],
				ToExpression[StringDelete[Global`eqList[[2]],"R"]],
				Global`coef1,Global`coef2](*<sumOp*);
				(*Una volta effetuata l'operazione verr\[AGrave] resettata l'iterfaccia grafica *)
			Global`eqPointer=1;
			Global`eqList[[1]]=" ";
			Global`eqList[[2]]=" ";
			Global`coef1 = 1;
			Global`coef2 = 1;
			Global`showError = False;
		,(*<Else*)
		(*Viene visualizzato un pannello contenente un messaggio di errore*)
			Global`showError = True;
			Global`errorMsg = "Equation coefficients are nor Integers or Rationals";
		](*<If*)
	,(*else*)
	(*Caso in cui l'operazione selezionata risulta essere lo scambio delle righe *)
		Global`matrice = AlgebricUtil`swapOperation[Global`matrice,
			ToExpression[StringDelete[Global`eqList[[1]],"R"]],
			ToExpression[StringDelete[Global`eqList[[2]],"R"]]];(*<swapOp*)
			(*Sostituisco la matrice corrente con il risultato dell'operazione swapOperation *)
		Global`eqPointer = 1;
		Global`eqList[[1]]=" ";
		Global`eqList[[2]]=" ";
	](*<If*)
	(*In questa sezione viene contrallato se la matrice risulta essere risolta ed in tal caso viene visualizzato un messaggio di successo*)
	If[checkSolved[Global`matrice]==True,Global`showSuccess=True;]
](*<Dynamic*),Enabled->Dynamic[If[
									(*Condizioni per abilitare il bottone*)
									Global`editMatrix==True || 
									Global`showSuccess==True ||
									StringMatchQ[Global`eqList[[1]],"R*"]==False ||
									StringMatchQ[Global`eqList[[2]],"R*"]==False,False,True]]];(*<Button*)


(*Componente grafico per l'input dei coefficenti moltiplicativi *)
fieldCoef1 := InputField[Dynamic[Global`coef1],Expression,FieldSize->{5,1}];


(*Componente grafico per l'input dei coefficenti moltiplicativi *)
fieldCoef2 := InputField[Dynamic[Global`coef2],Expression,FieldSize->{5,1}];


(*Componente grafica che visulizza l'operazione che si sta per applicare alla matrice*)
equationbox :=
	Row[
		{Global`displayF1,
		gph,
		Panel[
			Dynamic[Global`eqList[[1]]],Background->Dynamic[
					If[Global`eqPointer==1,LightBlue,Automatic]]],
		gph,
		"+",
		gph,
		Global`displayF2,
		gph,
		Panel[
			Dynamic[Global`eqList[[2]]],Background->Dynamic[
					If[Global`eqPointer==2,LightBlue,Automatic]]],
		gph,
		goButton,
		gph,
		Dynamic[Global`eqList[[1]](*<Part*)]
	}];(*<Row*)


(*Componete grafica che permette l'input della matrice*)
createMatrix[dim_,matrix_] := Module[{i,j,m,ph,d=dim},
	m = List[];
	For[i=1,i <= d,i++,
		AppendTo[m,List[]];
		For[j=1, j<=d,j++,
			(*placeholder*)
			ph = "M["<>ToString[i]<>","<>ToString[j]<>"]";
			With[{i0 = i,j0 = j},
				AppendTo[m[[i]],InputField[Dynamic[matrix[[i0,j0]]](*Dynamic*),Expression,FieldHint->ph,FieldSize->{4,1}](*InputField*)];(*AppendTo*)
			](*With*)
		];(*For2*)
	];(*For1*)
	Return[m];
];(*Module*)


(*Bottoni per selezionare le righe alle quale applicare le operazioni*)
rowSelector[dim_,container_] := Module[{i,l},
	l = List[];
	For[i=1,i<=dim,i++,
		With[{i0=i},
			AppendTo[l,Button["R"<>ToString[i0],container = "R"<>ToString[i0],FrameMargins->0]]
		];
	];
	Return[Column[l]];
];


Clear[Global`container];


rowsB := rowSelector[Global`dimension,Global`container];


Clear[Global`matrice];


inputmatrix := createMatrix[Global`dimension,Global`matrice];


Global`hint := AlgebricUtil`solveMatrix[Global`matrice];


(*Bottone per richiedere l'aiuto *)
hintButton := Button["?" ,Global`showHint=True,Enabled->Dynamic[If[Global`editMatrix==True,False,True]]];


(*Pannello nel quale viene visualizzato il messaggio si aiuto*)
hintPanel  := Panel[Global`hint,Background->LightBlue];


(*Funzione per verificare che tutti i numeri inseriti nella matrice siano interi o razionali*)
checkInputMatrix[m_] := Module[{i,j,len,ret},
	len = Length[m];
	ret = List[];
	For[i=1,i<=len,i++,
		For[j=1,j<=len,j++,
		
			If[MatchQ[m[[i,j]](*<Part*), _Rational | _Integer](*<MatchQ*)==False,
				
				With[{i0=i,j0=j},ret = {i0,j0};Break[];];(*<With*)
			];(*<If*)
			
		];(*<For*)
	];(*<For*)
	
	Return[ret];
];(*<Module*)


(*Bottone per rendere la matrice non pi\[UGrave] editabile *)
setMatrixButton := Button["Set",
Dynamic[
	If[
	Length[checkInputMatrix[Global`matrice]]==0,
		Global`showError=False;
		Global`editMatrix=False;
		Global`hint = AlgebricUtil`solveMatrix[Global`matrice];
		If[checkSolved[Global`matrice]==True,Global`showSuccess=True;]
	,(*<else*)
		Global`showError = True;
		Global`errorMsg = "Element at position "<>
							ToString[Part[checkInputMatrix[Global`matrice],1]]<>","<>
							ToString[Part[checkInputMatrix[Global`matrice],2]]<>
							" is nor a Rational or an Integer";
	];(*<If*)
](*<Dynamic*)
,Enabled->Dynamic[If[Global`editMatrix==True,True,False]]](*<Button*)


(*Resetta la matrice corente e permette di inserirne una nuova*)
resetMatrixButton := Button["Reset",
								Global`editMatrix=True;
								Global`matrice = AlgebricUtil`matrixConstructor[Global`dimension]; 
								Global`showError = False; 
								Global`showHint=False;
								Global`showSuccess=False;
								Global`eqList={" "," "};
								Global`eqPointer=1;
					,Enabled->Dynamic[If[Global`editMatrix==True,False,True]]];


(*Pannello che contiene i bottoni di set e reset*)
lastRow1 := Panel[Row[{setMatrixButton,gph,resetMatrixButton}],Background->Gray];


(*Pannello che contiene il bottone di aiuto*)
lastRow2 := Panel[Row[{hintButton}],Background->Gray];


(*Funzione  utilizzata per graficare le equazioni in input sotto forma di rette o piani *)
plotConstructor := Module[{x,y,z},
	If[Global`dimension==2,
		Return[
			ContourPlot[{
				Global`matrice[[1,1]]x+Global`matrice[[1,2]]y == 0,
				Global`matrice[[2,1]]x+Global`matrice[[2,2]]y == 0
			},{x,-20,20},{y,-20,20},Background->LightBlue](*<ContPlt*)
		];(*<Return*)
	,(*<else*)
		Return[ContourPlot3D[{
				Global`matrice[[1,1]]x+Global`matrice[[1,2]]y+Global`matrice[[1,3]]z == 0,
				Global`matrice[[2,1]]x+Global`matrice[[2,2]]y+Global`matrice[[2,3]]z == 0,
				Global`matrice[[3,1]]x+Global`matrice[[3,2]]y+Global`matrice[[3,3]]z == 0
			},{x,-20,20},{y,-20,20},{z,-20,20},Background->LightBlue](*<ContPlt3*)
		];(*<Return*)
	];(*If*)
	
];


plotArea := Dynamic[plotConstructor]


(*Pannello che compone tutti gli elementi dell'intefaccia grafica*)
composedGUI := Panel[Grid[{
		{
			Grid[{
				{lastRow1},
				{gph},
				{If[Global`editMatrix == True,
					MatrixForm[Global`inputmatrix](*<MatrixForm*)
				,(*<else*)
					Style[MatrixForm[Global`matrice],Large]
				](*<If*),rowsB}(*<G_row*)
			}(*<G_matrix*)](*<Grid*),
			gph,
			Column[{operB,gph,Dynamic[equationbox]}](*<Column*),
			Column[{plotArea,lastRow2},Alignment->{Right,Top}]
		},
		{Row[{gph}]}}(*<Gird arg list*)](*<Grid*),Background->Gray
	](*<Panel*);
	


(*Pannello che contiene eventuali messaggi di errore*)
errorPanel := Panel[Dynamic[Global`errorMsg],Background->LightRed];


(*Pannello che contiene il messaggio di successo*)
successPanel := Panel["The matrix is reduced",Background->LightGreen];


End[]


EndPackage[]
