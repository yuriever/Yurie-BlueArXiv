(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`BlueArXiv`downloadByID`"];


Needs["Yurie`BlueArXiv`"];

Needs["Yurie`BlueArXiv`Common`"];

Needs["Yurie`BlueArXiv`Default`"];

Needs["Yurie`BlueArXiv`searchByID`"];


(* ::Section:: *)
(*Public*)


downloadByID::usage =
    "download by arXiv IDs extracted from string, image or PDF file/directory path.";


downloadByIDAsPaperData;


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Option*)


downloadByIDAsPaperData//Options = {
    "HideFile"->False,
    Splice@Options@searchByIDAsPaperData
};

downloadByID//Options = {
    "ClickToCopy"->True,
    Splice@Options@downloadByIDAsPaperData
};


(* ::Subsection:: *)
(*Main*)


downloadByID[
    tag:$tagPattern:"string",
    HoldPattern[targetDir:(_?DirectoryQ):$defaultDownloadDir],
    opts:OptionsPattern[]
][input_] :=
    Module[ {fopts,paperData},
        fopts =
            FilterRules[{opts,Options[downloadByID]},Options[downloadByIDAsPaperData]];
        paperData =
            downloadByIDAsPaperData[tag,targetDir,fopts][input];
        paperData//ifAddButton[OptionValue["ClickToCopy"],"ID","Paper","URL"]//Dataset
    ];


(* ::Subsection:: *)
(*Helper*)


downloadByIDAsPaperData[tag_,targetDir_,opts:OptionsPattern[]][input_] :=
    Module[ {paperData,fopts},
        fopts =
            FilterRules[{opts,Options[downloadByIDAsPaperData]},Options[searchByIDAsPaperData]];
        paperData =
            searchByIDAsPaperData[tag,fopts][input];
        (*download to the target path and return file objects*)
        paperData//downloadFromPaperDataAsFileObject[targetDir]//ifHideFile[OptionValue["HideFile"]]
    ];


downloadFromPaperDataAsFileObject[targetDir_][paperData_] :=
    paperData//Query[All,<|#,"File"->downloadByURL[targetDir,#URL,#Paper]|>&];


ifHideFile[True][paperData_] :=
    paperData//KeyDrop["File"];

ifHideFile[False][paperData_] :=
    paperData;


downloadByURL[_,_,Missing[_]] :=
    Missing["Failed"];

downloadByURL[targetDir_,url_,paperName_String] :=
    URLDownload[url,FileNameJoin@{targetDir,paperName<>".pdf"}];

downloadByURL[targetDir_,url_,paperName_] :=
    URLDownload[url,FileNameJoin@{targetDir,ToString[paperName]<>".pdf"}];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
