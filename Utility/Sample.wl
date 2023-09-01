(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`BlueArXiv`Sample`"];


Needs["Yurie`BlueArXiv`Info`"];


(* ::Section:: *)
(*Public*)


sampleFileDirectory::usage = 
    "directory of the sample files.";

sampleFilePrepare::usage = 
    "create and download the sample files.";

sampleFileClear::usage = 
    "clear the sample files.";

sampleString::usage = 
    "sample strings.";


(* ::Section:: *)
(*Private*)


(* ::Subsection::Closed:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Messages*)


sampleFilePrepare::connectionfailed = 
    "The network connection fails.";


(* ::Subsection:: *)
(*Functions*)


samplePaperData = {
    <|
        "name"->"oldID-9802150.pdf",
        "ID"->"hep-th/9802150",
        "URL"->"https://arxiv.org/pdf/hep-th/9802150.pdf",
        "citeKey"->"Witten:1998qj"
    |>,
    <|
        "name"->"newID-1207.7214.pdf",
        "ID"->"1207.7214",
        "URL"->"https://arxiv.org/pdf/1207.7214.pdf",
        "citeKey"->"ATLAS:2012yve"
    |>,
    <|
        "name"->"csID-1706.03762.pdf",
        "ID"->"1706.03762",
        "URL"->"https://arxiv.org/pdf/1706.03762.pdf",
        "citeKey"->"vaswani2017attention"
    |>,
    <|
        "name"->"wrongID-0000.00001.pdf",
        "ID"->"0000.00001",
        "URL"->Missing["Failed"],
        "citeKey"->""
    |>
};


sampleFileDirectory =
    <|
        "self"->$thisSampleDir,
        "pdf"->FileNameJoin@{$thisSampleDir,"pdf"},
        "tex"->FileNameJoin@{$thisSampleDir,"tex"}
    |>;


sampleString =
    <|
        "ID"->StringRiffle[Query[All,#ID&][samplePaperData],","],
        "citeKey"->"\\cite{"<>StringRiffle[Query[All,#citeKey&][samplePaperData],","]<>"}"
    |>;


(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

sampleFilePrepare[] :=
    Module[ {dir},
        dir = sampleFileDirectory["pdf"];
        Which[ 
            !$NetworkConnected,
                sampleFilePrepare::connectionfailed,
            samplePaperData//Query[All,FileExistsQ@FileNameJoin@{dir,#name}&]//AllTrue[TrueQ]//Not,
                Export[
                    FileNameJoin@{dir,samplePaperData//Query[4,#name&]},
                    "This is an example file."
                ];
                samplePaperData//Query[1;;3,URLDownload[#URL,FileNameJoin@{dir,#name}]&];
                Print["The sample files have been created."]
        ];
    ];

(* :!CodeAnalysis::EndBlock:: *)


(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

sampleFileClear[] :=
    Module[ {dir},
        dir = FileNameJoin@{sampleFileDirectory["tex"],"aux"};
        If[ DirectoryQ@dir,
            DeleteDirectory[dir,DeleteContents->True]
        ];
        DeleteFile@FileNames[All,sampleFileDirectory["pdf"]];
        Print["The sample files have been removed."]
    ];

(* :!CodeAnalysis::EndBlock:: *)


(* ::Subsection::Closed:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


sampleFilePrepare[];


EndPackage[];
