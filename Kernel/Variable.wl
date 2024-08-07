(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`BlueArXiv`Variable`"];


(*clear the states when loading.*)

ClearAll["`*"];


Needs["Yurie`BlueArXiv`Common`"];


(* ::Section:: *)
(*Public*)


$arXivPDFNameFormatter::usage =
    "formatter of file names, set by arXivPDFNameFormat.";

$arXivPDFNameRegulator::usage =
    "regulator of file names, set by arXivPDFNameFormat.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Main*)


(*the default format of PDF names.*)
(*arXivPDFNameFormat["ID"<>" "<>"title"<>", "<>"firstAuthor"]*)

$arXivPDFNameFormatter =
    (
        Lookup[#,"ID"]<>" "<>
        Lookup[#,"Title",""]<>", "<>
        Lookup[#,"Author","",Part[#,1,"Name"]&]
    )&;


(*the default regulator of PDF names.*)

$arXivPDFNameRegulator =
    regulateFileName;


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
