(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`BlueArXiv`Sample`"];


Needs["Yurie`BlueArXiv`Info`"];

Needs["Yurie`BlueArXiv`Common`"];


(* ::Section:: *)
(*Public*)


sampleString::usage =
    "sample strings.";

sampleFileDirectory::usage =
    "directory of the sample files.";

sampleFilePrepare::usage =
    "create and download the sample files.";

sampleFileClear::usage =
    "clear the sample files.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Constant*)


samplePaperData = {
    <|
        "name"->"oldID-9802150.pdf",
        "ID"->"hep-th/9802150",
        "URL"->"https://arxiv.org/pdf/hep-th/9802150.pdf"
    |>,
    <|
        "name"->"newID-1207.7214.pdf",
        "ID"->"1207.7214",
        "URL"->"https://arxiv.org/pdf/1207.7214.pdf"
    |>,
    <|
        "name"->"csID-1706.03762.pdf",
        "ID"->"1706.03762",
        "URL"->"https://arxiv.org/pdf/1706.03762.pdf"
    |>,
    <|
        "name"->"wrongID-0000.00001.pdf",
        "ID"->"0000.00001",
        "URL"->Missing["Failed"]
    |>
};


sampleTeXData = {
    <|
        "name"->"citeKey1.tex",
        "string"->"\\begin{itemize}\n\t\\item single key \\cite{vaswani2017attention};\n\t\\item multiple keys \\cite{ATLAS:2012yve,Witten:1998qj};\n\\end{itemize}"
    |>,
    <|
        "name"->"citeKey2.tex",
        "string"->"\\begin{itemize}\n\t\\item unicode key \\cite{dieudonn\[EAcute]1969treatise};\n\t\\item empty key \\cite{};\n\\end{itemize}"
    |>
};


(* ::Subsection:: *)
(*Main*)


(* ::Subsubsection:: *)
(*sampleString*)


sampleString =
    <|
        "ID"->StringRiffle[samplePaperData[[All,"ID"]],","],
        "citeKey"->StringRiffle[sampleTeXData[[All,"string"]],"\n"]
    |>;


(* ::Subsubsection:: *)
(*sampleFileDirectory*)


sampleFileDirectory =
    <|
        "self"->$thisSourceDir,
        "pdf"->FileNameJoin@{$thisSourceDir,"pdf"},
        "tex"->FileNameJoin@{$thisSourceDir,"tex"}
    |>;


(* ::Subsubsection:: *)
(*sampleFilePrepare*)


sampleFilePrepare[] :=
    {sampleFilePrepare["pdf"],sampleFilePrepare["tex"]};

sampleFilePrepare["pdf"] :=
    Module[ {dir},
        dir = sampleFileDirectory["pdf"];
        If[ !DirectoryQ[dir],
            CreateDirectory[dir]
        ];
        Which[
            !$NetworkConnected,
                Failure["ConnectionFailed",<|
                    "MessageTemplate"->"The network connection fails."
                |>],
            !sampleFileCheck["pdf"],
                samplePaperData//Query[1;;3,URLDownload[#URL,FileNameJoin@{dir,#name}]&];
                Export[
                    FileNameJoin@{dir,samplePaperData[[4,"name"]]},
                    "This is an example file."
                ];
                Success["SamplePDFPrepared",<|
                    "MessageTemplate"->"The sample PDF files have been prepared."
                |>],
            True,
                Success["SamplePDFPrepared",<|
                    "MessageTemplate"->"The sample PDF files exist."
                |>]
        ]
    ];

sampleFilePrepare["tex"] :=
    Module[ {dir},
        dir = sampleFileDirectory["tex"];
        If[ !DirectoryQ[dir],
            CreateDirectory[dir]
        ];
        If[ !sampleFileCheck["tex"],
            sampleTeXData//Query[All,Export[FileNameJoin@{dir,#name},#string,"Text"]&];
            Success["SamplePDFPrepared",<|
                "MessageTemplate"->"The sample TeX files have been prepared."
            |>],
            (*Else*)
            Success["SamplePDFPrepared",<|
                "MessageTemplate"->"The sample TeX files exist."
            |>]
        ]
    ];


sampleFileCheck["pdf"] :=
    SameQ[
        samplePaperData[[All,"name"]]//Map[FileBaseName]//Sort,
        sampleFileDirectory["pdf"]//getFileNameByExtension["pdf"]//Sort
    ];

sampleFileCheck["tex"] :=
    SameQ[
        sampleTeXData[[All,"name"]]//Map[FileBaseName]//Sort,
        sampleFileDirectory["tex"]//getFileNameByExtension["tex"]//Sort
    ];


(* ::Subsubsection:: *)
(*sampleFileClear*)


sampleFileClear[] :=
    (
        DeleteDirectory[sampleFileDirectory["self"],DeleteContents->True];
        CreateDirectory[sampleFileDirectory["self"]];
        File@sampleFileDirectory["self"]
    );


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
