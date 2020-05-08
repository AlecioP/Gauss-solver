(* ::Package:: *)

BeginPackage["MakeGui`"]


makeGUI::usage = "makeGUI[] Hides all the cells containing code. Formats all output cells"


Begin["`Private`"]


makeGUI := Module[{nb},
	nb = EvaluationNotebook[];
	NotebookFind[EvaluationNotebook[],"Input",All,CellStyle];
	SetOptions[NotebookSelection[nb],CellOpen->False,ShowCellBracket->False,ShowCellTags->False];
	NotebookFind[EvaluationNotebook[],"Output",All,CellStyle];
	SetOptions[NotebookSelection[nb],ShowCellBracket->False,ShowCellTags->False];
]


End[]


EndPackage[]
