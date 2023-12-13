(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`BlueArXiv`"];


(*ClearAll["`*"];*)
(*ClearAll["`*`*"];*)


(* ::Section:: *)
(*Public*)


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
(*Subpackages *)


Needs["Yurie`BlueArXiv`Common`"];


Get["Yurie`BlueArXiv`Default`"];
Get["Yurie`BlueArXiv`arXivPDFNameFormat`"];
Get["Yurie`BlueArXiv`extractID`"];
Get["Yurie`BlueArXiv`searchByID`"];
Get["Yurie`BlueArXiv`downloadByID`"];
Get["Yurie`BlueArXiv`generateBibTeXByID`"];
Get["Yurie`BlueArXiv`arXivInterface`"];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
