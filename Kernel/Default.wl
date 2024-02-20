(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`BlueArXiv`Default`"];


ClearAll["`*"];


(* ::Section:: *)
(*Public*)


(* ::Subsection:: *)
(*Default values*)


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


Needs["Yurie`BlueArXiv`Common`"];


(* ::Subsection:: *)
(*Default values*)


$arXivPDFNameFormatter =
    (*set the default format of PDF names.*)
    (*arXivPDFNameFormat["ID"<>" "<>"title"<>", "<>"firstAuthor"]*)
    (
        Lookup[#,"ID"]<>" "<>
        Lookup[#,"Title",""]<>", "<>
        Lookup[#,"Author","",Part[#,1,"Name"]&]
    )&;


(*set the default regulator of PDF names.*)
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
