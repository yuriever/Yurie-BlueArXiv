(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["lily`arxiv`sample`"];


sampleFileDirectory::usage = 
    "directory of the sample files.";

sampleFilePrepare::usage = 
    "create and download the sample files.";

sampleString::usage = 
    "sample strings.";
    
(*samplePaperData;*)


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*sampleData*)


$thisPaclet :=
    $thisPaclet = 
        PacletObject["lily/arxiv"];


sampleFileDirectory :=
    sampleFileDirectory =
        Module[ {sampleDataDir},
            sampleDataDir = $thisPaclet["AssetLocation","SampleData"];
            <|
                "self"->sampleDataDir,
                "pdf"->FileNameJoin@{sampleDataDir,"pdf"},
                "tex"->FileNameJoin@{sampleDataDir,"tex"}
            |>
        ];


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


sampleString :=
    sampleString =
        <|
            "ID"->StringRiffle[Query[All,#ID&][samplePaperData],","],
            "citeKey"->"\\cite{"<>StringRiffle[Query[All,#citeKey&][samplePaperData],","]<>"}"
        |>;


sampleFilePrepare::connectionfailed = 
    "The network connection fails.";
sampleFilePrepare[] :=
    Module[ {folder},
        folder = sampleFileDirectory["pdf"];
        Which[ 
            !$NetworkConnected,
                sampleFilePrepare::connectionfailed,
            samplePaperData//Query[All,FileExistsQ@FileNameJoin@{folder,#name}&]//AllTrue[TrueQ]//Not,
                Export[
                    FileNameJoin@{folder,samplePaperData//Query[4,#name&]},
                    "This is an example PDF file."
                ];
                samplePaperData//Query[1;;3,URLDownload[#URL,FileNameJoin@{folder,#name}]&]
        ];
    ];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
