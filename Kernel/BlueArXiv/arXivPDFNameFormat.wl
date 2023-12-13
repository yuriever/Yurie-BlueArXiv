(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`BlueArXiv`arXivPDFNameFormat`"];


Needs["Yurie`BlueArXiv`"];


(* ::Section:: *)
(*Public*)


arXivPDFNameFormat;


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


Needs["Yurie`BlueArXiv`Common`"];
Needs["Yurie`BlueArXiv`Default`"];


(* ::Subsection:: *)
(*arXivPDFNameFormat*)


arXivPDFNameFormat//Attributes = 
    {HoldAll};

arXivPDFNameFormat[
    HoldPattern[format_:"ID"<>" "<>"title"<>", "<>"firstAuthor"],
    HoldPattern[regulator_Symbol:regulateFileName]
] :=
    (
        $arXivPDFNameFormatter = 
            formatter[format];
        $arXivPDFNameRegulator = 
            regulator;
    );


formatter//Attributes = 
    {HoldAll};

formatter[format_] :=
    Hold[format]/.keywordToFunction/.{Hold[expr_]:>Hold[(expr)&]}//ReleaseHold;


(* ::Code::Initialization::"Tags"-><|"UnscopedObjectError" -> <|Enabled -> False|>|>:: *)
keywordToFunction = {
    (*no default value for "ID", so as to return a failure if ID does not exist.*)
    "ID":>Lookup[#,"ID"],
    "date":>Lookup[#,"Published","",DateString[#,"ISODate"]&],
    "title":>Lookup[#,"Title",""],
    "abs":>Lookup[#,"Summary",""],
    "authors"|"allAuthors":>Lookup[#,"Author","",Function[assoc,StringRiffle[Catenate@assoc,", "]]],
    "author"|"firstAuthor":>Lookup[#,"Author","",Part[#,1,"Name"]&],
    "firstTwoAuthors":>Lookup[#,"Author","",Function[assoc,StringRiffle[Take[Catenate@assoc,UpTo[2]],", "]]],
    "firstThreeAuthors":>Lookup[#,"Author","",Function[assoc,StringRiffle[Take[Catenate@assoc,UpTo[3]],", "]]],
    "journal":>Lookup[#,"JournalReference",""]
};


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
