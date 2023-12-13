(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`BlueArXiv`downloadByID`"];


Needs["Yurie`BlueArXiv`"];


(* ::Section:: *)
(*Public*)


downloadByID;
downloadByIDAsItemList;


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


Needs["Yurie`BlueArXiv`Common`"];
Needs["Yurie`BlueArXiv`Default`"];
Needs["Yurie`BlueArXiv`searchByID`"];


(* ::Subsection:: *)
(*Options*)


downloadByIDAsItemList//Options = {
    "hideFileObject"->False,
    Splice@Options@searchByIDAsItemList
};

downloadByID//Options = {
    "clickToCopy"->True,
    Splice@Options@downloadByIDAsItemList
};


(* ::Subsection:: *)
(*downloadByID*)


downloadByID[
    tag:"string"|"path":"string",
    HoldPattern[targetFolder:(_?DirectoryQ):$defaultDownloadDir],
    opts:OptionsPattern[]
][arg_] :=
    Module[ {fopts},
        fopts = FilterRules[{opts},Options[downloadByIDAsItemList]];
        downloadByIDAsItemList[tag,targetFolder,fopts][arg]//ifAddButtonTo[OptionValue["clickToCopy"],"ID","item","URL"]//Dataset
    ];


downloadByIDAsItemList[tag_,targetFolder_,opts:OptionsPattern[]][arg_] :=
    Module[ {idDataList,fopts},
        fopts = FilterRules[{opts},Options[searchByIDAsItemList]];
        idDataList = searchByIDAsItemList[tag,fopts][arg];
        (*download to the target path and return file objects*)
        idDataList//Query[All,<|#,"fileObject"->downloadPDFFromURLAsFileObject[targetFolder,#URL,#item]|>&]//
        	ifHideFileObject[OptionValue["hideFileObject"]]
    ];


downloadPDFFromURLAsFileObject[_,_,Missing[_]] :=
    Missing["Failed"];

downloadPDFFromURLAsFileObject[targetFolder_,url_,item_String] :=
    URLDownload[url,FileNameJoin@{targetFolder,item<>".pdf"}];

downloadPDFFromURLAsFileObject[targetFolder_,url_,item_] :=
    URLDownload[url,FileNameJoin@{targetFolder,ToString[item]<>".pdf"}];


ifHideFileObject[True][idDataList_] :=
    idDataList//KeyDrop["fileObject"];

ifHideFileObject[False][idDataList_] :=
    idDataList;


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
