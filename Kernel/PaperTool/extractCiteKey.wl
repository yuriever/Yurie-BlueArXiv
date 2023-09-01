(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`PaperTool`extractCiteKey`"];


Needs["Yurie`BlueArXiv`Common`"];
Needs["Yurie`PaperTool`"];


(* ::Section:: *)
(*Public*)


extractCiteKey;
extractCiteKeyFromStringAsItemList;
extractCiteKeyFromTeXAsItemList;


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Options*)


getCiteKeyFromTeXAsList//Options = {
    "hideDirectory"->True
};

extractCiteKeyFromTeXAsItemList//Options = 
    Options@getCiteKeyFromTeXAsList;

extractCiteKey//Options = {
    "clickToCopy"->True,
    "rawCiteKey"->False,
    Splice@Options@extractCiteKeyFromTeXAsItemList
};


(* ::Subsection:: *)
(*Messages*)


extractCiteKey::texfailimport = 
    "the TeX file fails to import: \n``";


(* ::Subsection:: *)
(*extractCiteKey*)


extractCiteKey["string",opts:OptionsPattern[]][stringOrStringList_] :=
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


extractCiteKeyFromTeXAsItemList[opts:OptionsPattern[]][pathOrPathList_] :=
    Module[ {fopts},
        fopts = FilterRules[{opts},Options[getCiteKeyFromTeXAsList]];
        pathOrPathList//getTeXFromPathAsList//getCiteKeyFromTeXAsList[fopts]//Query[SortBy[#citeKey&]]
    ];


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
