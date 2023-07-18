(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["lily`arxiv`downloadByID`"];

Needs["lily`paper`common`"];
Needs["lily`arxiv`"];
Needs["lily`arxiv`searchByID`"];


downloadByID;
downloadByIDAsItemList;


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*downloadByID*)


downloadByID//Options = {
    "tryFileName"->True,
    "hideDirectory"->True,
    "mergeDuplicateID"->True,
    "fileNameRegulate"->True,
    "clickToCopy"->True
};
downloadByID[tag:"string"|"path",targetFolder_?DirectoryQ,opts:OptionsPattern[]][arg_] :=
    Module[ {fopts},
        fopts = FilterRules[{opts},Options[downloadByIDAsItemList]];
        downloadByIDAsItemList[tag,targetFolder,fopts][arg]//ifAddButtonTo[OptionValue["clickToCopy"],"ID","item","URL"]//Dataset
    ];

downloadByID[targetFolder_?DirectoryQ,opts:OptionsPattern[]][arg_] :=
    downloadByID["string",targetFolder,opts][arg];


downloadByIDAsItemList//Options = {
    "tryFileName"->True,
    "hideDirectory"->True,
    "mergeDuplicateID"->True,
    "fileNameRegulate"->True
};
downloadByIDAsItemList[tag_,targetFolder_,opts:OptionsPattern[]][arg_] :=
    Module[ {idDataList,fopts},
        fopts = FilterRules[{opts},Options[searchByIDAsItemList]];
        idDataList = searchByIDAsItemList[tag,fopts][arg];
        (*download to the target path and return file objects*)
        idDataList//Query[All,<|#,"fileObject"->downloadPDFFromURLAsFileObject[targetFolder,#URL,#item]|>&]
    ];


downloadPDFFromURLAsFileObject[targetFolder_,url_,Missing["Failed"]] :=
    Missing["Failed"];
downloadPDFFromURLAsFileObject[targetFolder_,url_,item_String] :=
    URLDownload[url,FileNameJoin@{targetFolder,item<>".pdf"}];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
