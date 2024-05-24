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
            input//throwWrongTypeInput[tag]//downloadByIDAsPaperData[tag,targetDir,fopts];
        paperData//ifAddButton[OptionValue["ClickToCopy"],"ID","Paper","URL"]//Dataset
    ]//Catch;


(* ::Subsection:: *)
(*Helper*)


downloadByIDAsPaperData[tag:$tagPattern,targetDir:$pathPattern,opts:OptionsPattern[]][input_] :=
    Module[ {paperData,fopts},
        fopts =
            FilterRules[{opts,Options[downloadByIDAsPaperData]},Options[searchByIDAsPaperData]];
        paperData =
            searchByIDAsPaperData[tag,fopts][input];
        (*download to the target path and return file objects*)
        paperData//downloadFromPaperDataAsFileObject[targetDir]//ifHideFile[OptionValue["HideFile"]]
    ];


downloadFromPaperDataAsFileObject[targetDir:$pathPattern][paperData_List] :=
    paperData//Query[All,<|#,"File"->downloadByURL[targetDir,#URL,#Paper]|>&];


ifHideFile[True][paperData_List] :=
    paperData//KeyDrop["File"];

ifHideFile[False][paperData_List] :=
    paperData;


downloadByURL[_,_,Missing[_]] :=
    Missing["PaperNotExist"];

downloadByURL[targetDir:$pathPattern,url_String,paperName_String] :=
    URLDownload[url,FileNameJoin@{targetDir,paperName<>".pdf"}];

downloadByURL[targetDir:$pathPattern,url_String,paperName_String] :=
    URLDownload[url,FileNameJoin@{targetDir,ToString[paperName]<>".pdf"}];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
