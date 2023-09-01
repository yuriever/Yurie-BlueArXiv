(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`BlueArXiv`Test`"];


Needs["Yurie`BlueArXiv`Info`"];


(* ::Section:: *)
(*Public*)


(* ::Section:: *)
(*Private*)


(* ::Subsection::Closed:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Functions*)


(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

Module[ {report},
    report = TestReport@FileNames["*.wlt",$thisTestDir];
    report["ResultsByOutcome"]//Map[Column]//TabView//Print;
];

(* :!CodeAnalysis::EndBlock:: *)


(* ::Subsection::Closed:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
