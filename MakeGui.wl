(* ::Package:: *)

BeginPackage["MakeGui`"]


makeGUI::usage = "makeGUI[] Hides all the cells containing code. Formats all output cells"


editNB::usage = "editNB[]. Shows the input cells of the notebook"


Begin["`Private`"]


makeGUI := Module[{nb},
	nb = EvaluationNotebook[];
	NotebookFind[EvaluationNotebook[],"Input",All,CellStyle];
	SetOptions[NotebookSelection[nb],CellOpen->False,ShowCellBracket->False,ShowCellTags->False];
	NotebookFind[EvaluationNotebook[],"Output",All,CellStyle];
	SetOptions[NotebookSelection[nb],ShowCellBracket->False,ShowCellTags->False];
];
editNB := Module[{nb},
	nb = EvaluationNotebook[];
	NotebookFind[EvaluationNotebook[],"Input",All,CellStyle];
	SetOptions[NotebookSelection[nb],CellOpen->True,ShowCellBracket->True];
	NotebookFind[EvaluationNotebook[],"Output",All,CellStyle];
	SetOptions[NotebookSelection[nb],ShowCellBracket->True];
];


End[]


EndPackage[]
