(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`BlueArXiv`generateBibTeXByID`"];


Needs["Yurie`BlueArXiv`"];


(* ::Section:: *)
(*Public*)


generateBibTeXByID;


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


Needs["Yurie`BlueArXiv`Common`"];
Needs["Yurie`BlueArXiv`Default`"];
Needs["Yurie`BlueArXiv`extractID`"];


(* ::Subsection:: *)
(*Options*)


generateBibTeXByID//Options = {
    "clickToCopy"->True,
    Splice@Options@generateBibTeXByIDFromPathAsItemList
};

generateBibTeXByIDFromPathAsItemList//Options = 
    Options@extractIDFromPathAsItemList;


(* ::Subsection:: *)
(*generateBibTeXByID*)


generateBibTeXByID[
    "string",
    HoldPattern[targetFolder:(_?DirectoryQ):$defaultDownloadDir],
    HoldPattern[bibName_String:$defaultBibName],
    opts:OptionsPattern[]
][arg_] :=
    generateBibTeXByIDFromStringAsItemList[targetFolder,bibName][arg]//ifAddButtonTo[OptionValue["clickToCopy"],"key","ID","BibTeX"]//
    	Dataset[#,HiddenItems->{"BibTeX"->True}]&

generateBibTeXByID[
    "path",
    HoldPattern[targetFolder:(_?DirectoryQ):$defaultDownloadDir],
    HoldPattern[bibName_String:$defaultBibName],
    opts:OptionsPattern[]
][arg_] :=
    Module[ {fopts},
        fopts = FilterRules[{opts},Options[generateBibTeXByIDFromPathAsItemList]];
        generateBibTeXByIDFromPathAsItemList[targetFolder,bibName,fopts][arg]//ifAddButtonTo[OptionValue["clickToCopy"],"key","ID","BibTeX"]//
	    	Dataset[#,HiddenItems->{"BibTeX"->True}]&
    ];

generateBibTeXByID[
    HoldPattern[targetFolder:(_?DirectoryQ):$defaultDownloadDir],
    HoldPattern[bibName_String:$defaultBibName],
    opts:OptionsPattern[]
][arg_] :=
    generateBibTeXByID["string",targetFolder,bibName,opts][arg];


generateBibTeXByIDFromStringAsItemList[targetFolder_,bibName_][stringOrStringList_] :=
    Module[ {itemList},
        itemList = stringOrStringList//extractIDFromStringAsItemList//getBibTeXItemFromIDListAsList;
        exportBibTeXFile[targetFolder,bibName,itemList];
        itemList
    ];


generateBibTeXByIDFromPathAsItemList[targetFolder_,bibName_String,opts:OptionsPattern[]][pathOrPathList_] :=
    Module[ {idDataList,idList,itemList},
        idDataList = pathOrPathList//extractIDFromPathAsItemList[opts];
        idList = idDataList//Query[All,#ID&];
        itemList = JoinAcross[
            getBibTeXItemFromIDListAsList[idList],
            idDataList,
            "ID"
        ];
        exportBibTeXFile[targetFolder,bibName,itemList];
        itemList
    ];


getBibTeXItemFromIDListAsList[idList_] :=
    Map[
        <|"ID"->#,"BibTeX"->URLExecute@HTTPRequest["https://inspirehep.net/api/arxiv/"<>#<>"?format=bibtex"]|>&,
        idList
    ]//Query[All,<|"key"->getBibTeXKeyFromItem[#BibTeX],#|>&];


getBibTeXKeyFromItem[bibtex_String] :=
    First@StringCases[bibtex,StartOfString~~Shortest[__]~~"{"~~Shortest[key__]~~",\n"~~__:>key]; 
       
getBibTeXKeyFromItem[_] :=
    Missing["Failed"];


exportBibTeXFile[targetFolder_,bibName_,itemList_] :=
    Export[
        FileNameJoin@{targetFolder,bibName},
        itemList//Query[Select[Head[#BibTeX]===String&],#BibTeX&]//Riffle[#,""]&,
        "List"
    ];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
