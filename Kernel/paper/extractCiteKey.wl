(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`paper`extractCiteKey`"];

Needs["Yurie`arxiv`common`"];
Needs["Yurie`paper`"];


extractCiteKey;
extractCiteKeyFromStringAsItemList;
extractCiteKeyFromTeXAsItemList;


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*extractCiteKey*)


extractCiteKey//Options = {
    "clickToCopy"->True,
    "hideDirectory"->True,
    "rawCiteKey"->False
};
extractCiteKey::texfailimport = 
    "the TeX file fails to import: \n``";
extractCiteKey["string",OptionsPattern[]][stringOrStringList_] :=
    stringOrStringList//extractCiteKeyFromStringAsItemList//ifAddButtonTo[OptionValue["clickToCopy"]];
extractCiteKey["path",opts:OptionsPattern[]][pathOrPathList_] :=
    Module[ {fopts,itemList},
        fopts = FilterRules[{opts},Options[extractCiteKeyFromTeXAsItemList]];
        itemList = pathOrPathList//extractCiteKeyFromTeXAsItemList[fopts];
        If[ OptionValue["rawCiteKey"],
            itemList//Query[All,#citeKey&]//DeleteDuplicates//ifAddButtonTo[OptionValue["clickToCopy"]],
            itemList//ifAddButtonTo[OptionValue["clickToCopy"],"citeKey"]//Dataset
        ]
    ];

extractCiteKey[opts:OptionsPattern[]][stringOrStringList_] :=
    extractCiteKey["string",opts][stringOrStringList];


extractCiteKeyFromStringAsItemList[stringOrStringList_] :=
    stringOrStringList//getCiteKeyFromStringAsList//Sort;


extractCiteKeyFromTeXAsItemList//Options = {
    "hideDirectory"->True
};
extractCiteKeyFromTeXAsItemList[opts:OptionsPattern[]][pathOrPathList_] :=
    Module[ {fopts},
        fopts = FilterRules[{opts},Options[getCiteKeyFromTeXAsList]];
        pathOrPathList//getTeXFromPathAsList//getCiteKeyFromTeXAsList[fopts]//Query[SortBy[#citeKey&]]
    ];


getCiteKeyFromTeXAsList//Options = {
    "hideDirectory"->True
};
getCiteKeyFromTeXAsList[OptionsPattern[]][file_] :=
    Module[ {citeKeyList,itemList},
        citeKeyList = file//importStringFromTeX//getCiteKeyFromStringAsList;
        itemList = <|"citeKey"->#,"file"->file|>&/@citeKeyList;
        If[ OptionValue["hideDirectory"],
            itemList//Query[All,<|#,"file"->hideDirectory[#file]|>&],
            itemList
        ]
    ];
getCiteKeyFromTeXAsList[opts:OptionsPattern[]][fileList_List] :=
    fileList//Map[getCiteKeyFromTeXAsList[opts]]//Flatten;


getCiteKeyFromStringAsList[stringOrStringList_] :=
    (*extract "\cite{...}"*)
    stringOrStringList//StringCases[$citeKeyPattern:>"$2"]//Flatten//
		(*extract multiple citekeys, then delete duplicated and empty keys.*)
		StringSplit[#,","]&//Flatten//DeleteDuplicates//DeleteCases[""];


hideDirectory[file_] :=
    First@getFileNameByExtension["tex"][file];


getTeXFromPathAsList[pathOrPathList_] :=
    getFileByExtension["tex"][pathOrPathList];


importStringFromTeX[file_] :=
    Quiet[
        Check[
            Import[file,"Text"],
            Message[extractCiteKey::texfailimport,file];
            ""
        ],
        All,
        {extractCiteKey::texfailimport}
    ];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
