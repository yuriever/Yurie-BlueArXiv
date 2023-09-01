(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`PaperTool`"];


(*ClearAll["`*"];*)
(*ClearAll["`*`*"];*)


Get["Yurie`BlueArXiv`Common`"];


(* ::Section:: *)
(*Public*)


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
(*Subpackages*)


Get["Yurie`PaperTool`extractTitleFromPDF`"];
Get["Yurie`PaperTool`extractCiteKey`"];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
