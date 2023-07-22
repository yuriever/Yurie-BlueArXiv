(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["lily`paper`"];

Unprotect@@Names["`*"];
ClearAll@@Names["`*"];
ClearAll@@Names["`*`*"];

Get["lily`arxiv`common`"];


(* ::Section:: *)
(*Usage*)


extractTitleFromPDF::usage = 
    "extract title from first page of PDF by searching grouped texts with larger Y coordinate and fontsize.";

extractCiteKey::usage = 
    "extract cite keys from string or TeX file/folder path.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*extractTitleFromPDF|extractCiteKeyFromTeX*)


Get["lily`paper`extractTitleFromPDF`"];
Get["lily`paper`extractCiteKey`"];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


Protect@@Names["`*"];


EndPackage[];
