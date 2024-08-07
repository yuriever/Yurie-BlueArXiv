(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`BlueArXiv`arXivInterface`"];


Needs["Yurie`BlueArXiv`"];

Needs["Yurie`BlueArXiv`Constant`"];


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


arXivInterfaceKernel[targetDir:$pathPattern] :=
    Interpretation[
        {
            fun = "download",
            tag = "string",
            str = "",
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
                Dynamic[str],
                String,
                FieldHint->"Enter a string or a PDF file/directory path.",
                FieldSize->{Dynamic[width]/20.,{Dynamic[height]/200.,Infinity}}
            ],
            "",
            "Input image:",
            InputField[
                Dynamic[image],
                Expression,
                FieldHint->"Copy and paste an image here.",
                FieldSize->{Dynamic[width]/20.,{Dynamic[height]/200.,Infinity}}
            ]
        },
        Switch[fun,
            "extract",
                extractID[tag]@selectInputByTag[tag][str,image],
            "search",
                searchByID[tag]@selectInputByTag[tag][str,image],
            "download",
                downloadByID[tag,target]@selectInputByTag[tag][str,image],
            "generate BibTeX",
                generateBibTeXByID[tag,target]@selectInputByTag[tag][str,image]
        ]
    ];


selectInputByTag["string"|"path"][str_String,image:$imagePattern] :=
    str;

selectInputByTag["image"][str_String,image:$imagePattern] :=
    image;


(* ::Subsection::Closed:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
