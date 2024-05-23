(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`BlueArXiv`generateBibTeXByID`"];


Needs["Yurie`BlueArXiv`"];

Needs["Yurie`BlueArXiv`Common`"];

Needs["Yurie`BlueArXiv`Default`"];

Needs["Yurie`BlueArXiv`extractID`"];


(* ::Section:: *)
(*Public*)


generateBibTeXByID::usage =
    "generate BibTeX entries on INSPIRE by arXiv IDs extracted from string, image or PDF file/directory path.";

generateBibTeXByIDAsBibData;


(* ::Subsection:: *)
(*Message*)


generateBibTeXByID::connectionFailed =
    "connection failed.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Option*)


generateBibTeXByIDAsBibData//Options =
    Options@extractIDData;

generateBibTeXByID//Options = {
    "ClickToCopy"->True,
    Splice@Options@generateBibTeXByIDAsBibData
};


(* ::Subsection:: *)
(*Main*)


generateBibTeXByID[
    tag:$tagPattern:"string",
    HoldPattern[targetDir:(_?DirectoryQ):$defaultDownloadDir],
    HoldPattern[bibName_String:$defaultBibName],
    opts:OptionsPattern[]
][input_] :=
    Module[ {fopts,bibData},
        fopts =
        	FilterRules[{opts,Options[generateBibTeXByID]},Options[generateBibTeXByIDAsBibData]];
        bibData=
        	generateBibTeXByIDAsBibData[tag,targetDir,bibName,fopts][input];
        bibData//ifAddButton[OptionValue["ClickToCopy"],"ID","BibKey","BibTeX"]//
	    	Dataset[#,HiddenItems->{"BibTeX"->True}]&
    ];


(* ::Subsection:: *)
(*Helper*)


generateBibTeXByIDAsBibData[tag_,targetDir_,bibName_String,opts:OptionsPattern[]][input_] :=
    input//extractIDData[tag,opts]//getBibDataFromIDData[targetDir,bibName];


getBibDataFromIDData[targetDir_,bibName_][idData_] :=
    Module[ {idList,idValidList,bibData},
        idList =
            idData//Query[All,#ID&];
        idValidList =
            DeleteDuplicates@DeleteCases[idList,"NotFound"];
        bibData =
            idValidList//getRawBibDataFromIDList//addBibKeyToBibData;
        If[ MemberQ[idList,"NotFound"],
            bibData =
                Join[
                    bibData,
                    {<|"ID"->"NotFound","BibTeX"->Missing["Failed"],"BibKey"->Missing["Failed"]|>}
                ]
        ];
        bibData =
            JoinAcross[
                bibData//addBibKeyToBibData,
                idData,
                "ID"
            ];
        exportBib[targetDir,bibName,bibData];
        bibData
    ];


getRawBibDataFromIDList[idList_] :=
    If[ idList==={},
        (*if there is no valid ID, return empty list.*)
        {},
        (*Else*)
        (*improve robustness against failure of HTTPRequest.*)
        Enclose[
            ConfirmMatch[
                (*return the raw data.*)
                idList//Map[<|"ID"->#,"BibTeX"->requestBib[#]|>&],
                _List,
                Message[generateBibTeXByID::connectionFailed]
            ],
            (*if the connection fails, return list of empty associations.*)
            Table[<||>,Length@idList]&
        ]
    ];


requestBib[id_]:=
    Enclose[
        ConfirmMatch[
            URLExecute@HTTPRequest["https://inspirehep.net/api/arxiv/"<>id<>"?format=bibtex"],
            _String
        ],
        (*if the returned value is not a string, return a missing.*)
        Missing["NotFound"]&
    ];


addBibKeyToBibData[bibData_] :=
    bibData//Query[All,<|"BibKey"->getBibKey[#BibTeX],#|>&];


getBibKey[bibtex_String] :=
    First@StringCases[bibtex,StartOfString~~Shortest[__]~~"{"~~Shortest[key__]~~",\n"~~__:>key]; 

getBibKey[_] :=
    Missing["Failed"];


exportBib[targetDir_,bibName_,bibData_] :=
    Export[
        FileNameJoin@{targetDir,bibName},
        bibData//Query[Select[Head[#BibTeX]===String&],#BibTeX&]//Riffle[#,""]&,
        "List"
    ];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
