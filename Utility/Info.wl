(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`BlueArXiv`Info`"];


(* ::Section:: *)
(*Public*)


$thisPacletDir::usage =
    "directory of paclet.";

$thisKernelDir::usage =
    "directory of kernel.";

$thisSourceDir::usage =
    "directory of source.";

$thisTestDir::usage =
    "directory of unit test.";

$thisTestSourceDir::usage =
    "directory of source notebook for unit test.";

$thisCompletionDir::usage =
    "directory of auto completion data.";

$thisSandboxDir::usage =
    "directory of AI sandbox.";

$thisWorkbenchDir::usage =
    "directory of workbench.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Main*)


$thisPaclet =
    PacletObject["Yurie/BlueArXiv"];

$thisPacletDir =
    $thisPaclet["Location"];

$thisKernelDir =
    FileNameJoin@{$thisPacletDir,"Kernel"};

$thisSourceDir =
    $thisPaclet["AssetLocation","Source"];

$thisTestDir =
    $thisPaclet["AssetLocation","Test"];

$thisTestSourceDir =
    $thisPaclet["AssetLocation","TestSource"];

$thisCompletionDir =
    FileNameJoin@{$thisPaclet["Location"],"AutoCompletionData"};

$thisSandboxDir =
    FileNameJoin@{$thisPaclet["Location"],"Sandbox"};

$thisWorkbenchDir =
    FileNameJoin@{$thisPaclet["Location"],"Workbench"};


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
