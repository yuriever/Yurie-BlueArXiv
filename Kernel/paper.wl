(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["lily`paper`"];

Unprotect@@Names["`*"];
ClearAll@@Names["`*"];
ClearAll@@Names["`*`*"];

Needs["lily`paper`common`"];


(* ::Section:: *)
(*Usage*)


extractTitleFromPDF::usage = 
    "extract title from first page of PDF by searching grouped texts with larger Y coordinates and fontsize.";

extractCiteKeyFromTeX::usage = 
    "extract cite keys from TeX files.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*extractTitleFromPDF|extractCiteKeyFromTeX*)


Get["lily`paper`extractTitleFromPDF`"];
Get["lily`paper`extractCiteKeyFromTeX`"];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


Protect@@Names["`*"];


EndPackage[];
