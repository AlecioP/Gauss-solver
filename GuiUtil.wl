(* ::Package:: *)

BeginPackage["GuiUtil`"]


createMatrix::usage = "createMatrix[Dimension,Matrix]. Create gui to input a square matrix of given dimension"


dimensionPanel::usage = "Graphic component to select matrix dimension"


goButton::usage = "Graphic component to start evaluation of expression"


fieldCoef1::usage = "Graphic component to get first equation coefficient from input"


fieldCoef2::usage = "Graphic component to get second equation coefficient from input"


gph::usage = "Graphic Place holder"


equationbox::usage = "Graphic compenent to visualiza the equation beeing composed"


operB::usage = "Buttons to select the kind of operation to perform on the matrix"


rowSelector::usage = "Function to create rows buttons"


rowsB::usage = "Buttons to select the row to insert into the equation"


composedGUI::usage = "Final GUI composed of all the parts into the package"


Begin["`Private`"]


gph := Panel[" ",Background->Gray];
operB := Row[{Button["Swap",Global`operation="SWAP";],gph,Button["Sum",Global`operation = "SUM";]}];
dimensionPanel := Panel[Grid[{{"Dimension : ",RadioButtonBar[Dynamic[Global`dimension],Range[2,3]]}}],Background->Gray];
goButton := Button["=",Dynamic[
	If[Global`operation=="SUM",
		Global`matrice = AlgebricUtil`sumOperation[Global`matrice,
			ToExpression[StringDelete[Global`eqList[[1]],"R"]],
			ToExpression[StringDelete[Global`eqList[[2]],"R"]],
			Global`coef1,Global`coef2](*<sumOp*);
		Global`eqPointer=1;
		Global`eqList[[1]]=" ";
		Global`eqList[[2]]=" ";
		Global`coef1 = 1;
		Global`coef2 = 1;
	,(*else*)
		Global`matrice = AlgebricUtil`swapOperation[Global`matrice,
			ToExpression[StringDelete[Global`eqList[[1]],"R"]],
			ToExpression[StringDelete[Global`eqList[[2]],"R"]]];(*<swapOp*)
		Global`eqPointer = 1;
		Global`eqList[[1]]=" ";
		Global`eqList[[2]]=" ";
	](*<If*)
](*<Dynamic*)];(*<Button*)
fieldCoef1 := InputField[Dynamic[Global`coef1],Number,FieldSize->{5,1}];
fieldCoef2 := InputField[Dynamic[Global`coef2],Number,FieldSize->{5,1}];
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
				AppendTo[m[[i]],InputField[Dynamic[matrix[[i0,j0]]](*Dynamic*),Number,FieldHint->ph,FieldSize->{4,1}](*InputField*)];(*AppendTo*)
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
composedGUI := Panel[
		Row[{
			Grid[{{MatrixForm[Global`inputmatrix](*<MatrixForm*),rowsB}(*<G_row*)}(*<G_matrix*)](*<Grid*),
			gph,
			Column[{operB,gph,Dynamic[equationbox]}](*<Column*)
		}](*<Row*),Background->Gray
	](*<Panel*);


End[]


EndPackage[]
