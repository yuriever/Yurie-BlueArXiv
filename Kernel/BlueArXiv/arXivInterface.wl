(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`BlueArXiv`arXivInterface`"];


Needs["Yurie`BlueArXiv`common`"];
Needs["Yurie`BlueArXiv`"];
Needs["Yurie`BlueArXiv`extractID`"];
Needs["Yurie`BlueArXiv`searchByID`"];
Needs["Yurie`BlueArXiv`downloadByID`"];
Needs["Yurie`BlueArXiv`generateBibTeXByID`"];


arXivInterface;


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*downloadByID*)


arXivInterface[HoldPattern[targetFolder:(_?DirectoryQ):$defaultDownloadDir]] :=
    kernel[targetFolder];


kernel[targetFolder_] :=
    Interpretation[
        {    
            fun = "download",
            tag = "string",
            string = "",
            target = targetFolder,
            width = First@CurrentValue[EvaluationNotebook[],"WindowSize"],
            height = Last@CurrentValue[EvaluationNotebook[],"WindowSize"]
        },
        Deploy@Panel@Column@{
            "Function:",
            Row@{
                PopupMenu[Dynamic[fun],{"extract","search","download","generate BibTeX"},ImageSize->Small],
                " from ",
                PopupMenu[Dynamic[tag],{"string","path"},ImageSize->Small]
            },
            "",
            "Downloads path:",
            Sequence@@targetUnit,
            "",
            "Input string/path:",
            Sequence@@inputUnit
        },
        Switch[fun,
            "extract",
                extractID[tag][string],
            "search",
                searchByID[tag][string],
            "download",
                downloadByID[tag,target][string],
            "generate BibTeX",
                generateBibTeXByID[tag,target][string]
        ]
    ];


targetUnit =
    Hold@InputField[
        Dynamic[target],
        String,
        FieldHint->"Enter the downloads path.",
        FieldSize->{Dynamic[width]/17.,1}
    ];


inputUnit =
    Hold@InputField[
        Dynamic[string],
        String,
        FieldHint->"Enter a string or a PDF file/folder path.",
        FieldSize->{Dynamic[width]/17.,{Dynamic[height]/100.,Infinity}}
    ];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
