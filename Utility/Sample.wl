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


formatList = {"pdf","png","tex"};


sampleData = <|
    "pdf"->{
        <|
            "Name"->"oldID-9802150.pdf",
            "ID"->"hep-th/9802150",
            "URL"->"https://arxiv.org/pdf/hep-th/9802150.pdf"
        |>,
        <|
            "Name"->"newID-1207.7214.pdf",
            "ID"->"1207.7214",
            "URL"->"https://arxiv.org/pdf/1207.7214.pdf"
        |>,
        <|
            "Name"->"csID-1706.03762.pdf",
            "ID"->"1706.03762",
            "URL"->"https://arxiv.org/pdf/1706.03762.pdf"
        |>,
        <|
            "Name"->"wrongID-0000.00001.pdf",
            "ID"->"0000.00001",
            "URL"->Missing["Failed"]
        |>,
        <|
            "Name"->"noID.pdf",
            "ID"->"",
            "URL"->Missing["Failed"]
        |>
    },
    "png"->{
        <|
            "Name"->"IDs.png",
            "Content"->"hep-th/9802150  1207.7214\n\n1706.03762  0000.00001"
        |>
    },
    "tex"->{
        <|
            "Name"->"citeKey1.tex",
            "Content"->"\\begin{itemize}\n\t\\item single key \\cite{vaswani2017attention};\n\t\\item multiple keys \\cite{ATLAS:2012yve,Witten:1998qj};\n\\end{itemize}"
        |>,
        <|
            "Name"->"citeKey2.tex",
            "Content"->"\\begin{itemize}\n\t\\item unicode key \\cite{dieudonn\[EAcute]1969treatise};\n\t\\item empty key \\cite{};\n\\end{itemize}"
        |>
    }
|>;


(* ::Subsection:: *)
(*Main*)


(* ::Subsubsection:: *)
(*sampleString*)


sampleString =
    <|
        "ID"->StringRiffle[sampleData[["pdf",All,"ID"]],","],
        "CiteKey"->StringRiffle[sampleData[["tex",All,"Content"]],"\n"]
    |>;


(* ::Subsubsection:: *)
(*sampleFileDirectory*)


sampleFileDirectory =
    <|
        "self"->$thisSourceDir,
        AssociationMap[FileNameJoin@{$thisSourceDir,#}&,formatList]
    |>;


(* ::Subsubsection:: *)
(*sampleFilePrepare*)


sampleFilePrepare[] :=
    Map[sampleFilePrepare,formatList];

sampleFilePrepare["pdf"] :=
    Module[ {dir = sampleFileDirectory["pdf"]},
        sampleDirectoryCreate[dir];
        Which[
            !$NetworkConnected,
                Failure["ConnectionFailed",<|
                    "MessageTemplate"->"The network connection fails."
                |>],
            !sampleFileMatchQ["pdf"],
                sampleData[["pdf"]]//Query[1;;3,URLDownload[#URL,FileNameJoin@{dir,#Name}]&];
                Export[
                    FileNameJoin@{dir,sampleData[["pdf",4,"Name"]]},
                    "This is a sample PDF file with wrong ID 0000.00001."
                ];
                Export[
                    FileNameJoin@{dir,sampleData[["pdf",5,"Name"]]},
                    "This is a sample PDF file without ID."
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

sampleFilePrepare["png"] :=
    Module[ {dir = sampleFileDirectory["png"]},
        sampleDirectoryCreate[dir];
        Which[
            !sampleFileMatchQ["png"],
                sampleData[["png"]]//Query[All,Export[FileNameJoin@{dir,#Name},Rasterize[#Content],"PNG"]&];
                Success["SamplePNGPrepared",<|
                    "MessageTemplate"->"The sample PNG files have been prepared."
                |>],
            True,
                Success["SamplePNGPrepared",<|
                    "MessageTemplate"->"The sample PNG files exist."
                |>]
        ]
    ];

sampleFilePrepare["tex"] :=
    Module[ {dir = sampleFileDirectory["tex"]},
        sampleDirectoryCreate[dir];
        If[ !sampleFileMatchQ["tex"],
            sampleData[["tex"]]//Query[All,Export[FileNameJoin@{dir,#Name},#Content,"Text"]&];
            Success["SampleTeXPrepared",<|
                "MessageTemplate"->"The sample TeX files have been prepared."
            |>],
            (*Else*)
            Success["SampleTeXPrepared",<|
                "MessageTemplate"->"The sample TeX files exist."
            |>]
        ]
    ];


sampleFileMatchQ[format_] :=
    SameQ[
        sampleData[[format,All,"Name"]]//Map[FileNameTake]//Sort,
        sampleFileDirectory[format]//getFileNameByExtension[format]//Sort
    ];

sampleDirectoryCreate[dir_] :=
    If[ !DirectoryQ[dir],
        CreateDirectory[dir]
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
