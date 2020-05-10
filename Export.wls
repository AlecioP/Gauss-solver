#!/Applications/Mathematica.app/Contents/MacOS/wolframscript
(* ::Package:: *)

UsingFrontEnd[
	Clear[nb];
	currentDir = DirectoryName[$InputFileName];
	fn = currentDir <> "Gauss.txt";
	nb = NotebookOpen[currentDir<>"Gauss.nb"];
	FrontEndExecute[FrontEndToken[nb,"Save",{fn,"Text"}]];
]
