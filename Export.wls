#!/Applications/Mathematica.app/Contents/MacOS/wolframscript
(* ::Package:: *)

UsingFrontEnd[
	Clear[nb];
	currentDir = DirectoryName[$InputFileName];
	fn = currentDir <> "Final.txt";
	nb = NotebookOpen[currentDir<>"Final.nb"];
	FrontEndExecute[FrontEndToken[nb,"Save",{fn,"Text"}]];
]
