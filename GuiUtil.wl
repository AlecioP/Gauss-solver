(* ::Package:: *)

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


Begin["`Private`"]


gph := Panel[" ",Background->Gray];


operB := Row[{Button["Swap",Global`operation="SWAP";],gph,Button["Sum",Global`operation = "SUM";]}];


dimensionPanel := Panel[Grid[{{"Dimension : ",RadioButtonBar[Dynamic[Global`dimension],Range[2,3]]}}],Background->Gray];


goButton := Button["=",Dynamic[
	If[Global`operation=="SUM",
		If[MatchQ[Global`coef1, _Rational | _Integer] && MatchQ[Global`coef2, _Rational | _Integer],
			Global`matrice = AlgebricUtil`sumOperation[Global`matrice,
				ToExpression[StringDelete[Global`eqList[[1]],"R"]],
				ToExpression[StringDelete[Global`eqList[[2]],"R"]],
				Global`coef1,Global`coef2](*<sumOp*);
			Global`eqPointer=1;
			Global`eqList[[1]]=" ";
			Global`eqList[[2]]=" ";
			Global`coef1 = 1;
			Global`coef2 = 1;
			Global`showError = False;
		,(*<Else*)
			Global`showError = True;
			Global`errorMsg = "Equation coefficients are nor Integers or Rationals";
		](*<If*)
	,(*else*)
		Global`matrice = AlgebricUtil`swapOperation[Global`matrice,
			ToExpression[StringDelete[Global`eqList[[1]],"R"]],
			ToExpression[StringDelete[Global`eqList[[2]],"R"]]];(*<swapOp*)
		Global`eqPointer = 1;
		Global`eqList[[1]]=" ";
		Global`eqList[[2]]=" ";
	](*<If*)
](*<Dynamic*),Enabled->Dynamic[If[Global`editMatrix==True,False,True]]];(*<Button*)


fieldCoef1 := InputField[Dynamic[Global`coef1],Expression,FieldSize->{5,1}];


fieldCoef2 := InputField[Dynamic[Global`coef2],Expression,FieldSize->{5,1}];


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


hintButton := Button["?" ,Global`showHint=True];


hintPanel  := Panel[Global`hint,Background->LightBlue];


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


setMatrixButton := Button["Set",
Dynamic[
	If[
	Length[checkInputMatrix[Global`matrice]]==0,
		Global`showError=False;
		Global`editMatrix=False;
		Global`hint = AlgebricUtil`solveMatrix[Global`matrice];
	,(*<else*)
		Global`showError = True;
		Global`errorMsg = "Element at position "<>
							ToString[Part[checkInputMatrix[Global`matrice],1]]<>","<>
							ToString[Part[checkInputMatrix[Global`matrice],2]]<>
							" is nor a Rational or an Integer";
	];(*<If*)
](*<Dynamic*)
](*<Button*)


resetMatrixButton := Button["Reset",Global`editMatrix=True;Global`matrice = AlgebricUtil`matrixConstructor[Global`dimension]];


lastRow1 := Panel[Row[{setMatrixButton,gph,resetMatrixButton}],Background->Gray];


lastRow2 := Panel[Row[{hintButton}],Background->Gray];


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


composedGUI := Panel[Grid[{
		{
			Grid[{
				{lastRow1},
				{gph},
				{If[Global`editMatrix == True,
					MatrixForm[Global`inputmatrix](*<MatrixForm*)
				,(*<else*)
					MatrixForm[Global`matrice]
				](*<If*),rowsB}(*<G_row*)
			}(*<G_matrix*)](*<Grid*),
			gph,
			Column[{operB,gph,Dynamic[equationbox]}](*<Column*),
			Column[{plotArea,lastRow2},Alignment->{Right,Top}]
		},
		{Row[{gph}]}}(*<Gird arg list*)](*<Grid*),Background->Gray
	](*<Panel*);
	


errorPanel := Panel[Dynamic[Global`errorMsg],Background->LightRed];


End[]


EndPackage[]
