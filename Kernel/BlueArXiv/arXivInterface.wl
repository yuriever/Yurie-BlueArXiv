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
                Sequence@@targetUnit,
                "",
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
                    generateBibTeXByID[tag,target,"refs-"<>ToString@RandomInteger[{100,1000}]<>".bib"][string]
            ]
        ],
        "Input"
    ];


targetUnit =
    Hold[
        "Downloads path:",
        InputField[
            Dynamic[target],
            String,
            FieldHint->"Enter the downloads path.",
            FieldSize->{First@CurrentValue[WindowSize]/16.885,1}
        ]
    ];

inputUnit =
    Hold[
        "Input string/path:",
        InputField[
            Dynamic[string],
            String,
            FieldHint->"Enter a string or a PDF file/folder path.",
            FieldSize->{First@CurrentValue[WindowSize]/16.885,Last@CurrentValue[WindowSize]/80}
        ]
    ];



(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
