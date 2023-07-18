(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["lily`arxiv`"];

Unprotect@@Names["`*"];
ClearAll@@Names["`*"];
ClearAll@@Names["`*`*"];

Needs["lily`paper`common`"];


(* ::Section:: *)
(*Usage*)


arXivIDQ::usage =
    "check whether a string is a valid arXiv ID.";

arXivPDFNameFormat::usage = 
    "set the format of arXiv PDF names.";

arXivInterface::usage = 
    "show the interface.";

extractID::usage =
    "extract arXiv IDs from string, file name or path.";
searchByID::usage = 
    "search by IDs extracted from string, file or path, "<>
    "and return the found items on arXiv with formatted names by fileNameFormatter.";
downloadByID::usage = 
    "download by IDs extracted from string, file or path to the target path, "<>
    "and return the file objects with formatted names by fileNameFormatter."
generateBibTeXByID::usage = 
    "export the found BibTeX entries on inspirehep by IDs extracted from string, file or path, "<>
    "and return the BibTeX keys.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*arXivIDQ*)


arXivIDQ[string_String] :=
    StringMatchQ[
        string,
        RegularExpression["(\\d{4}\\.\\d{4,5})|((astro-ph|cond-mat|gr-qc|hep-ex|hep-lat|hep-ph|hep-th|math-ph|nlin|nucl-ex|nucl-th|physics|quant-ph|math|cs)/\\d{7})"]
    ];
arXivIDQ[_] = False;


(* ::Subsection:: *)
(*arXivPDFNameFormat*)


arXivPDFNameFormat//Attributes = 
    {HoldAll};
arXivPDFNameFormat[format_] :=
    (
        $arXivPDFNameFormat = Hold[format]/.lily`arxiv`arXivPDFNameFormat`Private`keywordToFunction/.{Hold[expr_]:>Hold[(expr)&]}//ReleaseHold;
    );


lily`arxiv`arXivPDFNameFormat`Private`keywordToFunction = {
    "ID":>Query["ID"][#],
    "date":>DateString[Query["Published"][#],"ISODate"],
    "title":>Query["Title"][#],
    "abs":>Query["Summary"][#],
    "author":>RemoveDiacritics@StringRiffle[Query["Author",All,"Name"][#],", "],
    "firstAuthor":>RemoveDiacritics@Query["Author",1,"Name"][#],
    "firstThreeAuthor":>RemoveDiacritics@StringRiffle[Query["Author",1;;3,"Name"][#],", "],
    "journal":>DeleteMissing@Query["JournalReference"][#]
};


(* ::Subsection:: *)
(*arXivInterface*)


arXivInterface[] :=
    CellPrint@ExpressionCell[
        Interpretation[
            {    
                fun = "download",
                tag = "string",
                string = "",
                target = FileNameJoin@{$HomeDirectory,"Downloads"}
            },
            Panel@Column@{
                "Function:",
                Row@{
                    PopupMenu[Dynamic[fun],{"extract","search","download","generate BibTeX"},Appearance->"DialogBox",ImageSize->Small],
                    " from ",
                    PopupMenu[Dynamic[tag],{"string","path"},Appearance->"DialogBox",ImageSize->Small]
                },
                "",
                Sequence@@lily`arxiv`arXivInterface`Private`targetUnit,
                "",
                Sequence@@lily`arxiv`arXivInterface`Private`inputUnit
            },
            Switch[fun,
                "extract",
                    extractID[tag][string],
                "search",
                    searchByID[tag][string],
                "download",
                    downloadByID[tag,target][string],
                "generate BibTeX",
                    generateBibTeXByID[tag,target,"refs-"<>ToString@RandomInteger[{100,1000}]<>".bib"][string]
            ]
        ],
        "Input"
    ];


lily`arxiv`arXivInterface`Private`targetUnit =
    Hold["Downloads location:",InputField[Dynamic[target],String,FieldHint->"Enter the downloads location.",FieldSize->{First@CurrentValue[WindowSize]/20,1}]];
lily`arxiv`arXivInterface`Private`inputUnit =
    Hold["Input string/path:",InputField[Dynamic[string],String,FieldHint->"Enter a string/file/path containing arXiv IDs.",FieldSize->{First@CurrentValue[WindowSize]/20,Last@CurrentValue[WindowSize]/80}]];


(* ::Subsection:: *)
(*extractID|searchByID|downloadByID|generateBibTeXByID*)


Get["lily`arxiv`extractID`"];
Get["lily`arxiv`searchByID`"];
Get["lily`arxiv`downloadByID`"];
Get["lily`arxiv`generateBibTeXByID`"];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


Protect@@Names["`*"];

(*set the default format of PDF names.*)
arXivPDFNameFormat["ID"<>" "<>"title"<>", "<>"firstAuthor"];

EndPackage[];
