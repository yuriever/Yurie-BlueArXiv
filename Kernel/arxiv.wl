(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["lily`arxiv`"];

Unprotect@@Names["`*"];
ClearAll@@Names["`*"];
(*ClearAll@@Names["`*`*"];*)

Get["lily`arxiv`common`"];


(* ::Section:: *)
(*Usage*)


arXivIDQ::usage =
    "check whether the string is a syntactically valid arXiv ID.";

arXivPDFNameFormat::usage = 
    "set the format of arXiv PDF names.";

arXivInterface::usage = 
    "show the interface.";

extractID::usage =
    "extract arXiv IDs from string or PDF file/folder path.";
searchByID::usage = 
    "search by arXiv IDs extracted from string or PDF file/folder path.";
downloadByID::usage = 
    "download by arXiv IDs extracted from string or PDF file/folder path.";
generateBibTeXByID::usage = 
    "generate BibTeX entries on INSPIRE by arXiv IDs extracted from string or PDF file/folder path.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*arXivIDQ*)


arXivIDQ[string_String] :=
    StringMatchQ[string,$arXivIDPattern];
arXivIDQ[_] = False;


(* ::Subsection:: *)
(*functions*)


Get["lily`arxiv`arXivPDFNameFormat`"];
Get["lily`arxiv`extractID`"];
Get["lily`arxiv`searchByID`"];
Get["lily`arxiv`downloadByID`"];
Get["lily`arxiv`generateBibTeXByID`"];
Get["lily`arxiv`arXivInterface`"];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


Protect@@Names["`*"];


EndPackage[];
