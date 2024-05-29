(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`BlueArXiv`searchByID`"];


Needs["Yurie`BlueArXiv`"];

Needs["Yurie`BlueArXiv`Common`"];

Needs["Yurie`BlueArXiv`Default`"];

Needs["Yurie`BlueArXiv`extractID`"];


(* ::Section:: *)
(*Public*)


searchByID::usage =
    "search by arXiv IDs extracted from string, image or PDF file/directory path.";


searchByIDAsPaperData;


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Option*)


searchByIDAsPaperData//Options =
    Options@extractIDData;

searchByID//Options = {
    "ClickToCopy"->True,
    Splice@Options@searchByIDAsPaperData
};


(* ::Subsection:: *)
(*Message*)


searchByID::connectionFailed =
    "connection failed.";


(* ::Subsection:: *)
(*Main*)


searchByID[tag:$tagPattern:"string",opts:OptionsPattern[]][input_] :=
    Module[ {paperData},
        paperData =
            input//throwWrongTypeInput[tag]//searchByIDAsPaperData[tag,FilterRules[{opts,Options[searchByID]},Options[searchByIDAsPaperData]]];
        paperData//ifAddButton[OptionValue["ClickToCopy"],"ID","Paper","URL"]//Dataset
    ]//Catch;


(* ::Subsection:: *)
(*Helper*)


searchByIDAsPaperData[tag:$tagPattern,opts:OptionsPattern[]][input_] :=
    input//extractIDData[tag,FilterRules[{opts,Options@searchByIDAsPaperData},Options@extractIDData]]//getPaperDataFromIDData;


getPaperDataFromIDData[idData_List] :=
    Module[ {idList,idValidList,rawPaperData,newPaperData,paperNameList,urlList},
        idList =
            idData//Query[All,#ID&];
        idValidList =
            DeleteDuplicates@DeleteCases[idList,"NotFound"];
        rawPaperData =
            idValidList//getRawPaperDataFromIDList;
        paperNameList =
            rawPaperData//getPaperNameListFromPaperData;
        urlList =
            rawPaperData//getURLListFromPaperData;
        newPaperData =
            MapThread[
                <|"ID"->#1,"Paper"->#2,"URL"->#3|>&,
                {idValidList,paperNameList,urlList}
            ];
        If[ MemberQ[idList,"NotFound"],
            newPaperData =
                Join[
                    newPaperData,
                    {<|"ID"->"NotFound","Paper"->Missing["IDNotExist"],"URL"->Missing["IDNotExist"]|>}
                ]
        ];
        JoinAcross[
            newPaperData,
            idData,
            "ID"
        ]
    ];


getRawPaperDataFromIDList[idList_List] :=
    If[ idList==={},
        (*if there is no valid ID, return empty list.*)
        {},
        (*Else*)
        (*improve robustness against failure of ServiceExecute.*)
        Enclose[
            ConfirmMatch[
                (*return the raw data.*)
                Quiet@Normal@ServiceExecute["ArXiv","Search",{"ID"->idList,MaxItems->Length@idList}],
                _List,
                Message[searchByID::connectionFailed]
            ],
            (*if the connection fails, return list of empty associations.*)
            Table[<||>,Length@idList]&
        ]
    ];


getPaperNameListFromPaperData[paperData_List] :=
    paperData//Query[All,$arXivPDFNameFormatter,FailureAction->"Replace"]//
    	Map[Switch[#,_Missing,#,_,$arXivPDFNameRegulator[#]]&];


getURLListFromPaperData[paperData_List] :=
    paperData//Map[getURL];


getURL[assoc_Association]/;MissingQ[assoc["ID"]] :=
    Missing["IDNotExist"];

getURL[assoc_Association] :=
    assoc["Link"]//KeyUnion//
    	Query[Select[#Type==="application/pdf"&],FailureAction->"Replace"]//
			Query[All,"Href",FailureAction->"Replace"]//First//StringJoin[#,".pdf"]&;


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
