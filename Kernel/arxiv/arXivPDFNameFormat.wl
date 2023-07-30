(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`arxiv`arXivPDFNameFormat`"];


Needs["Yurie`arxiv`common`"];
Needs["Yurie`arxiv`"];


arXivPDFNameFormat;


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*arXivPDFNameFormat*)


arXivPDFNameFormat//Attributes = 
    {HoldAll};
arXivPDFNameFormat[format_] :=
    (
        $arXivPDFNameFormat = Hold[format]/.keywordToFunction/.{Hold[expr_]:>Hold[(expr)&]}//ReleaseHold;
    );


keywordToFunction = {
    "ID":>Query["ID"][#],
    "date":>DateString[Query["Published"][#],"ISODate"],
    "title":>Query["Title"][#],
    "abs":>Query["Summary"][#],
    "author":>RemoveDiacritics@StringRiffle[Query["Author",All,"Name"][#],", "],
    "firstAuthor":>RemoveDiacritics@Query["Author",1,"Name"][#],
    "firstThreeAuthor":>RemoveDiacritics@StringRiffle[DeleteMissing@Query["Author",1;;3,"Name"][#],", "],
    "journal":>DeleteMissing@Query["JournalReference"][#]
};


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
