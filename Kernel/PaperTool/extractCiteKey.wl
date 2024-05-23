(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`PaperTool`extractCiteKey`"];


Needs["Yurie`PaperTool`"];

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
    Module[ {fopts,idData},
        fopts =
            FilterRules[{opts,Options[extractCiteKey]},Options[extractCiteKeyData]];
        idData =
            extractCiteKeyData[tag,fopts][input];
        If[ OptionValue["RawCiteKey"],
            idData//ifAddButton[OptionValue["ClickToCopy"],"CiteKey"]//Query[All,#CiteKey&],
            (*Else*)
            idData//ifAddButton[OptionValue["ClickToCopy"],"CiteKey"]//Dataset
        ]
    ];


(* ::Subsection:: *)
(*Helper*)


extractCiteKeyData[tag_,opts:OptionsPattern[]][input_] :=
    Module[ {idData},
        idData =
            Switch[tag,
                "string",
                    input//getCiteKeyDataFromString,
                "image",
                    Abort[],
                "path",
                    input//getCiteKeyDataFromPath[opts]
            ];
        idData//Query[SortBy[#CiteKey&]]
    ];


(* ::Subsubsection:: *)
(*String*)


getCiteKeyDataFromString[string_] :=
    string//getCiteKeyListFromString//Map[<|"CiteKey"->#|>&];


getCiteKeyListFromString[string_] :=
    (*extract "\cite{...}"*)
    string//StringCases[$citeKeyPattern:>"$2"]//Flatten//
		(*extract multiple citekeys, then delete duplicated and empty keys.*)
		StringSplit[#,","]&//Flatten//DeleteDuplicates//DeleteCases[""];


(* ::Subsubsection:: *)
(*Path*)


getCiteKeyDataFromPath[opts:OptionsPattern[]][path_] :=
    path//getTeXListFromPath//Map[getCiteKeyDataFromTeX[opts]]//Flatten;


getTeXListFromPath[pathOrPathList_] :=
    getFilePathByExtension["tex"][pathOrPathList];


getCiteKeyDataFromTeX[OptionsPattern[]][file_] :=
    Module[ {citeKeyList,citeKeyData},
        citeKeyList =
            file//tryImport["","Text"]//getCiteKeyListFromString;
        citeKeyData =
            citeKeyList//Map[<|"CiteKey"->#,"FileName"->{file}|>&];
        If[ OptionValue["HideDirectory"],
            citeKeyData//Query[All,<|#,"FileName"->hideDirectory[#FileName]|>&],
            citeKeyData
        ]
    ];


hideDirectory[file_] :=
    file//getFileNameByExtension["tex"];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
