(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["lily`paper`extractCiteKeyFromTeX`"];

Needs["lily`paper`common`"];
Needs["lily`paper`"];


extractCiteKeyFromTeX;


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*extractCiteKeyFromTeX*)


extractCiteKeyFromTeX::texfailimport = 
    "the TeX file fails to import: \n``";
extractCiteKeyFromTeX//Options = {
};
extractCiteKeyFromTeX[tag:"path"|"file",opts:OptionsPattern[]][arg_] :=
    {};


(* ::Subsubsection:: *)
(*Kernel*)


kernel//Options = {
};
kernel[tag:"path"|"file",opts:OptionsPattern[]][fileOrPath_String] :=
    Module[ {},
        getCiteKeyFromTeX[fileOrPath,tag,fopts]//Query[SortBy[#file&]]
    ];
kernel[tag:"path"|"file",opts:OptionsPattern[]][list_List] :=
    Module[ {fopts},
        fopts = FilterRules[{opts},Options[getTitleFromFileOrPath]];
        getTitleFromFileOrPath[#,tag,fopts]&/@list//Flatten//DeleteDuplicates//Query[SortBy[#file&]]
    ];


getCiteKeyFromTeX[file_,"file"] :=
    file//importStringFromTeX//StringSplit//StringCases[RegularExpression["(\\\\cite{)(\\S*?)(})"]:>"$2"]//
    	Flatten//StringSplit[#,","]&//Flatten//DeleteDuplicates//DeleteCases[""];

getCiteKeyFromTeX[path_,"path"] :=
    getCiteKeyFromTeX[#,"file"]&/@FileNames[__~~".tex"~~EndOfString,path];


importStringFromTeX[file_] :=
    Quiet[
        Check[
            Import[file,"Text"],
            Message[extractCiteKeyFromTeX::texfailimport,file];
            ""
        ],
        All,
        {extractCiteKeyFromTeX::texfailimport}
    ];    


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
