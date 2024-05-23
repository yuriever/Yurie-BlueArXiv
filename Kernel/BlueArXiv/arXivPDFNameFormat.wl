(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`BlueArXiv`arXivPDFNameFormat`"];


Needs["Yurie`BlueArXiv`"];

Needs["Yurie`BlueArXiv`Common`"];

Needs["Yurie`BlueArXiv`Default`"];


(* ::Section:: *)
(*Public*)


arXivPDFNameFormat::usage =
    "set the format of arXiv PDF names.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Main*)


arXivPDFNameFormat//Attributes =
    {HoldAll};

arXivPDFNameFormat[
    HoldPattern[format_:"ID"<>" "<>"Title"<>", "<>"FirstAuthor"],
    HoldPattern[regulator_Symbol:regulateFileName]
] :=
    (
        $arXivPDFNameFormatter =
            formatter[format];
        $arXivPDFNameRegulator =
            regulator;
    );


(* ::Subsection:: *)
(*Helper*)


formatter//Attributes =
    {HoldAll};

formatter[format_] :=
    Hold[format]//ReplaceAll[keywordToFunction]//ReplaceAll[{Hold[expr_]:>Hold[(expr)&]}]//ReleaseHold;


(* ::Code::Initialization::"Tags"-><|"UnscopedObjectError" -> <|Enabled -> False|>|>:: *)
keywordToFunction = {
    (*no default value for "ID", so as to return a failure if ID does not exist.*)
    "ID":>Lookup[#,"ID"],
    "Date":>Lookup[#,"Published","",DateString[#,"ISODate"]&],
    "Title":>Lookup[#,"Title",""],
    "Abs":>Lookup[#,"Summary",""],
    "Authors"|"AllAuthors":>Lookup[#,"Author","",Function[assoc,StringRiffle[Catenate@assoc,", "]]],
    "Author"|"FirstAuthor":>Lookup[#,"Author","",Part[#,1,"Name"]&],
    "FirstTwoAuthors":>Lookup[#,"Author","",Function[assoc,StringRiffle[Take[Catenate@assoc,UpTo[2]],", "]]],
    "FirstThreeAuthors":>Lookup[#,"Author","",Function[assoc,StringRiffle[Take[Catenate@assoc,UpTo[3]],", "]]],
    "Journal":>Lookup[#,"JournalReference",""]
};


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
