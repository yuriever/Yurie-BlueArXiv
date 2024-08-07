(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`BlueArXiv`Constant`"];


(* ::Section:: *)
(*Public*)


$arXivIDPattern::usage =
    "string pattern of valid arXiv ID.";

$citeKeyPattern::usage =
    "string pattern of cite key.";

$tagPattern::usage =
    "pattern of supported tags.";

$imagePattern::usage =
    "pattern of images.";

$pathPattern::usage =
    "pattern of paths.";

$defaultDownloadDir::usage =
    "default download directory";

$defaultBibName::usage =
    "default BibTeX file name.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Main*)


$arXivIDPattern =
    RegularExpression["(\\d{4}\\.\\d{4,5})|((astro-ph|cond-mat|gr-qc|hep-ex|hep-lat|hep-ph|hep-th|math-ph|nlin|nucl-ex|nucl-th|physics|quant-ph|math|cs)/\\d{7})"];


$citeKeyPattern =
    (*no whitespace tolerance.*)
    RegularExpression["(\\\\cite{)(\\S*?)(})"];


$tagPattern =
    "string"|"image"|"path";


$imagePattern =
    _Image|Null;


$pathPattern =
    _String|_File;


$defaultDownloadDir :=
    FileNameJoin@{$HomeDirectory,"Downloads"};


$defaultBibName :=
    "refs-"<>CreateUUID[]<>".bib";


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
