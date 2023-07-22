(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["lily`arxiv`"];

Unprotect@@Names["`*"];
ClearAll@@Names["`*"];
ClearAll@@Names["`*`*"];

Get["lily`arxiv`common`"];


(* ::Section:: *)
(*Usage*)


arXivIDQ::usage =
    "check whether a string is a valid arXiv ID.";

arXivPDFNameFormat::usage = 
    "set the format of arXiv PDF names.";

arXivInterface::usage = 
    "show the interface.";

extractID::usage =
    "extract arXiv IDs from string or PDF file/folder path.";
searchByID::usage = 
    "search by IDs extracted from string or PDF file/folder path, "<>
    "and return the found items on arXiv with formatted names.";
downloadByID::usage = 
    "download by IDs extracted from string or PDF file/folder path to the target folder, "<>
    "and return the file objects with formatted names."
generateBibTeXByID::usage = 
    "export the found BibTeX entries on inspirehep by IDs extracted string or PDF file/folder path, "<>
    "and return the BibTeX keys.";


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
