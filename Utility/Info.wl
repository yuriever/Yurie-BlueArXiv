(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`BlueArXiv`Info`"];


(* ::Section:: *)
(*Public*)


$thisPacletDir;

$thisKernelDir;

$thisSourceDir;

$thisTestDir;

$thisTestSourceDir;


(* ::Section:: *)
(*Private*)


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
