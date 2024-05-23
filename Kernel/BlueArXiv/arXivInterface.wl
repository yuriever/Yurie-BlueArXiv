(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`BlueArXiv`arXivInterface`"];


Needs["Yurie`BlueArXiv`"];

Needs["Yurie`BlueArXiv`Common`"];

Needs["Yurie`BlueArXiv`Default`"];


(* ::Section:: *)
(*Public*)


arXivInterface::usage =
    "show the interface.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Constant*)


tagList =
    {"string","image","path"};


(* ::Subsection:: *)
(*Main*)


arXivInterface[HoldPattern[targetDir:(_?DirectoryQ):$defaultDownloadDir]] :=
    arXivInterfaceKernel[targetDir];


(* ::Subsection:: *)
(*Helper*)


arXivInterfaceKernel[targetDir_] :=
    Interpretation[
        {
            fun = "download",
            tag = "string",
            string = "",
            image = Null,
            target = targetDir,
            width = First@CurrentValue[EvaluationNotebook[],"WindowSize"],
            height = Last@CurrentValue[EvaluationNotebook[],"WindowSize"]
        },
        Deploy@Panel@Column@{
            "Function:",
            Row@{
                PopupMenu[Dynamic[fun],{"extract","search","download","generate BibTeX"},ImageSize->Small],
                " from ",
                PopupMenu[Dynamic[tag],tagList,ImageSize->Small]
            },
            "",
            "Downloads directory:",
            InputField[
                Dynamic[target],
                String,
                FieldHint->"Enter the downloads directory.",
                FieldSize->{Dynamic[width]/20.,1}
            ],
            "",
            "Input string/path:",
            InputField[
                Dynamic[string],
                String,
                FieldHint->"Enter a string or a PDF file/directory path.",
                FieldSize->{Dynamic[width]/20.,{Dynamic[height]/200.,Infinity}}
            ],
            "",
            "Input image:",
            InputField[
                Dynamic[image],
                Expression,
                FieldHint->"Copy an image here.",
                FieldSize->{Dynamic[width]/20.,{Dynamic[height]/200.,Infinity}}
            ]
        },
        Switch[fun,
            "extract",
                extractID[tag]@selectInputByTag[string,image],
            "search",
                searchByID[tag]@selectInputByTag[string,image],
            "download",
                downloadByID[tag,target]@selectInputByTag[string,image],
            "generate BibTeX",
                generateBibTeXByID[tag,target]@selectInputByTag[string,image]
        ]
    ];


selectInputByTag["string"|"path"][string_,expr_] :=
    string;

selectInputByTag["image"][string_,expr_] =
    expr;


(* ::Subsection::Closed:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
