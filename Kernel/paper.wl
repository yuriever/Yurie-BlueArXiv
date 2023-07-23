(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`paper`"];

Unprotect@@Names["`*"];
ClearAll@@Names["`*"];
(*ClearAll@@Names["`*`*"];*)

Get["Yurie`arxiv`common`"];


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


Get["Yurie`paper`extractTitleFromPDF`"];
Get["Yurie`paper`extractCiteKey`"];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


Protect@@Names["`*"];


EndPackage[];
