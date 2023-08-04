(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`PaperTool`"];


Unprotect["`*"];
ClearAll["`*"];
(*ClearAll["`*`*"];*)


Get["Yurie`BlueArXiv`common`"];


(* ::Section:: *)
(*Usage*)


extractTitleFromPDF::usage = 
    "extract title from PDF file/folder path.";

extractCiteKey::usage = 
    "extract cite keys from string or TeX file/folder path.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*extractTitleFromPDF|extractCiteKeyFromTeX*)


Get["Yurie`PaperTool`extractTitleFromPDF`"];
Get["Yurie`PaperTool`extractCiteKey`"];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


Protect["`*"];


EndPackage[];
