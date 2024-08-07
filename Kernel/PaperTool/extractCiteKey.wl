(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`PaperTool`extractCiteKey`"];


Needs["Yurie`PaperTool`"];

Needs["Yurie`BlueArXiv`Constant`"];

Needs["Yurie`BlueArXiv`Common`"];


(* ::Section:: *)
(*Public*)


extractCiteKey::usage =
    "extract cite keys from string or TeX file/directory path.";


extractCiteKeyData;


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Option*)


getCiteKeyDataFromTeX//Options = {
    "HideDirectory"->True
};

getCiteKeyDataFromPath//Options =
    Options@getCiteKeyDataFromTeX;

extractCiteKeyData//Options =
    Options@getCiteKeyDataFromPath;

extractCiteKey//Options = {
    "ClickToCopy"->True,
    "RawCiteKey"->False,
    Splice@Options@extractCiteKeyData
};


(* ::Subsection:: *)
(*Main*)


extractCiteKey[tag:$tagPattern:"string",opts:OptionsPattern[]][input_] :=
    Module[ {idData},
        idData =
            extractCiteKeyData[tag,FilterRules[{opts,Options[extractCiteKey]},Options[extractCiteKeyData]]][input];
        If[ OptionValue["RawCiteKey"],
            idData//ifAddButton[OptionValue["ClickToCopy"],"CiteKey"]//Query[All,#CiteKey&],
            (*Else*)
            idData//ifAddButton[OptionValue["ClickToCopy"],"CiteKey"]//Dataset
        ]
    ];


(* ::Subsection:: *)
(*Helper*)


extractCiteKeyData[tag:$tagPattern,opts:OptionsPattern[]][input_] :=
    Module[ {idData},
        idData =
            Switch[tag,
                "string",
                    input//getCiteKeyDataFromString,
                "image",
                    Abort[],
                "path",
                    input//getCiteKeyDataFromPath[FilterRules[{opts,Options@extractCiteKeyData},Options@getCiteKeyDataFromPath]]
            ];
        idData//Query[SortBy[#CiteKey&]]
    ];


(* ::Subsubsection:: *)
(*String*)


getCiteKeyDataFromString[str_String] :=
    str//getCiteKeyListFromString//Map[<|"CiteKey"->#|>&];


getCiteKeyListFromString[str_String] :=
    (*extract "\cite{...}"*)
    str//StringCases[$citeKeyPattern:>"$2"]//Flatten//
		(*extract multiple citekeys, then delete duplicated and empty keys.*)
		StringSplit[#,","]&//Flatten//DeleteDuplicates//DeleteCases[""];


(* ::Subsubsection:: *)
(*Path*)


getCiteKeyDataFromPath[opts:OptionsPattern[]][path:$pathPattern] :=
    path//getTeXListFromPath//Map[getCiteKeyDataFromTeX[FilterRules[{opts,Options@getCiteKeyDataFromPath},Options@getCiteKeyDataFromTeX]]]//Flatten;


getTeXListFromPath[path:$pathPattern] :=
    getFilePathByExtension["tex"][path];


getCiteKeyDataFromTeX[OptionsPattern[]][filePath_String] :=
    Module[ {citeKeyList,citeKeyData},
        citeKeyList =
            filePath//tryImport["","Text"]//getCiteKeyListFromString;
        citeKeyData =
            citeKeyList//Map[<|"CiteKey"->#,"FileName"->{filePath}|>&];
        If[ OptionValue["HideDirectory"],
            citeKeyData//Query[All,<|#,"FileName"->hideDirectory[#FileName]|>&],
            citeKeyData
        ]
    ];


hideDirectory[filePathList_List] :=
    filePathList//getFileNameByExtension["tex"];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
