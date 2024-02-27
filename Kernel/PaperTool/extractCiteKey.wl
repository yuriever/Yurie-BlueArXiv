(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`PaperTool`extractCiteKey`"];


Needs["Yurie`PaperTool`"];

Needs["Yurie`BlueArXiv`Common`"];


(* ::Section:: *)
(*Public*)


extractCiteKey::usage =
    "extract cite keys from string or TeX file/folder path.";


extractCiteKeyFromStringAsItemList;

extractCiteKeyFromTeXAsItemList;


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Option*)


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
(*Message*)


extractCiteKey::texfailimport =
    "the TeX file fails to import: \n``";


(* ::Subsection:: *)
(*Main*)


extractCiteKey["string",opts:OptionsPattern[]][stringOrStringList_] :=
    stringOrStringList//extractCiteKeyFromStringAsItemList//ifAddButton[OptionValue["clickToCopy"]];

extractCiteKey["path",opts:OptionsPattern[]][pathOrPathList_] :=
    Module[ {fopts,itemList},
        fopts = FilterRules[{opts,Options[extractCiteKey]},Options[extractCiteKeyFromTeXAsItemList]];
        itemList = pathOrPathList//extractCiteKeyFromTeXAsItemList[fopts];
        If[ OptionValue["rawCiteKey"],
            itemList//Query[All,#citeKey&]//DeleteDuplicates//ifAddButton[OptionValue["clickToCopy"]],
            (*Else*)
            itemList//ifAddButton[OptionValue["clickToCopy"],"citeKey"]//Dataset
        ]
    ];

extractCiteKey[opts:OptionsPattern[]][stringOrStringList_] :=
    extractCiteKey["string",opts][stringOrStringList];


(* ::Subsection:: *)
(*Helper*)


extractCiteKeyFromStringAsItemList[stringOrStringList_] :=
    stringOrStringList//getCiteKeyFromStringAsList//Sort;


extractCiteKeyFromTeXAsItemList[opts:OptionsPattern[]][pathOrPathList_] :=
    Module[ {fopts},
        fopts = FilterRules[{opts,Options[extractCiteKeyFromTeXAsItemList]},Options[getCiteKeyFromTeXAsList]];
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
    getFilePathByExtension["tex"][pathOrPathList];


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
