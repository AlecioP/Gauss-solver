(* ::Package:: *)

(* :Title: MakeGui *)
(* :Context: MakeGui *)
(* :Author: Andrea Ercolessi, Alessio Portaro, Francesco Antimi *)
(* :Summary: Package contenente le funzioni di utilit\[AGrave] per l'intefaccia grafica richiamata nel Notebook final.nb *)
(* :Copyright: Andrea Ercolessi, Alessio Portaro, Francesco Antimi  2020 *)
(* :Package Version: 23, May 2020 *)
(* :Mathematica Version: 12.1 *)
(* :History: *)
(* :Sources: biblio *)
(* :Limitations:
this is a preliminary version, for educational purposes only. *)
(* :Discussion: *)


BeginPackage["MakeGui`"]


makeGUI::usage = "makeGUI[] Hides all the cells containing code. Formats all output cells"


editNB::usage = "editNB[]. Shows the input cells of the notebook"


Begin["`Private`"]


(*La funzione nasconde le celle di input e formatta il notebook in un layout pi\[UGrave] chiaro all'utente *)
makeGUI := Module[{nb},
	nb = EvaluationNotebook[];
	NotebookFind[EvaluationNotebook[],"Input",All,CellStyle];
	SetOptions[NotebookSelection[nb],CellOpen->False,ShowCellBracket->False,ShowCellTags->False];
	NotebookFind[EvaluationNotebook[],"Output",All,CellStyle];
	SetOptions[NotebookSelection[nb],ShowCellBracket->False,ShowCellTags->False];
];
(*La funzione rende visibili le celle di input per rendere possibile un eventuale modifica del comportamento del notebook*)
editNB := Module[{nb},
	nb = EvaluationNotebook[];
	NotebookFind[EvaluationNotebook[],"Input",All,CellStyle];
	SetOptions[NotebookSelection[nb],CellOpen->True,ShowCellBracket->True];
	NotebookFind[EvaluationNotebook[],"Output",All,CellStyle];
	SetOptions[NotebookSelection[nb],ShowCellBracket->True];
];


End[]


EndPackage[]
