(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`BlueArXiv`Info`"];


(* ::Section:: *)
(*Public*)


(* ::Subsection:: *)
(*Symbols*)


$thisPacletDir;
$thisSampleDir;
$thisTestDir;


(* ::Section:: *)
(*Private*)


(* ::Subsection::Closed:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Symbols*)


$thisPaclet = 
    PacletObject["Yurie/BlueArXiv"];
    
$thisPacletDir = 
    $thisPaclet["Location"];

$thisSampleDir = 
    $thisPaclet["AssetLocation","Sample"];

$thisTestDir = 
    $thisPaclet["AssetLocation","Test"];


(* ::Subsection::Closed:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
