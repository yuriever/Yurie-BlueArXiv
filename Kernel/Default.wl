(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`BlueArXiv`Default`"];


(*clear the states when loading.*)

ClearAll["`*"];


Needs["Yurie`BlueArXiv`Common`"];


(* ::Section:: *)
(*Public*)


$arXivPDFNameFormatter::usage =
    "formatter of file names, set by arXivPDFNameFormat.";

$arXivPDFNameRegulator::usage =
    "regulator of file names, set by arXivPDFNameFormat.";

$defaultDownloadDir::usage =
    "default download directory";

$defaultBibName::usage =
    "default BibTeX file name.";


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


$defaultDownloadDir :=
    FileNameJoin@{$HomeDirectory,"Downloads"};


$defaultBibName :=
    "refs-"<>CreateUUID[]<>".bib";


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
