(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["lily`arxiv`searchByID`"];

Needs["lily`arxiv`common`"];
Needs["lily`arxiv`"];
Needs["lily`arxiv`extractID`"];


searchByID;
searchByIDAsItemList;


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*searchByID*)


searchByID//Options = {
    "tryFileName"->True,
    "hideDirectory"->True,
    "mergeDuplicateID"->True,
    "fileNameRegulate"->True,
    "clickToCopy"->True
};
searchByID::connectionfailed =
    "connection failed.";
searchByID[tag:"string"|"path",opts:OptionsPattern[]][arg_] :=
    Module[ {fopts},
        fopts = FilterRules[{opts},Options[searchByIDAsItemList]];
        searchByIDAsItemList[tag,fopts][arg]//ifAddButtonTo[OptionValue["clickToCopy"],"ID","item","URL"]//Dataset
    ];

searchByID[opts:OptionsPattern[]][arg_] :=
    searchByID["string",opts][arg];


searchByIDAsItemList//Options = {
    "tryFileName"->True,
    "hideDirectory"->True,
    "mergeDuplicateID"->True,
    "fileNameRegulate"->True
};
searchByIDAsItemList["string",opts:OptionsPattern[]][stringOrStringList_] :=
    Module[ {fopts},
        fopts = FilterRules[{opts},Options[searchByIDFromStringAsItemList]];
        searchByIDFromStringAsItemList[fopts][stringOrStringList]
    ];
searchByIDAsItemList["path",opts:OptionsPattern[]][pathOrPathList_] :=
    Module[ {fopts},
        fopts = FilterRules[{opts},Options[searchByIDFromPathAsItemList]];
        searchByIDFromPathAsItemList[fopts][pathOrPathList]
    ];


searchByIDFromStringAsItemList//Options = {
    "fileNameRegulate"->True
};
searchByIDFromStringAsItemList[opts:OptionsPattern[]][stringOrStringList_] :=
    stringOrStringList//extractIDFromStringAsItemList//getItemDataFromIDAsList[opts];


searchByIDFromPathAsItemList//Options = {
    "tryFileName"->True,
    "hideDirectory"->True,
    "mergeDuplicateID"->True,
    "fileNameRegulate"->True
};
searchByIDFromPathAsItemList[opts:OptionsPattern[]][pathOrPathList_] :=
    Module[ {idDataList,idList,fopts},
        fopts[1] = FilterRules[{opts},Options[extractIDFromPathAsItemList]];
        fopts[2] = FilterRules[{opts},Options[getItemDataFromIDAsList]];
        idDataList = pathOrPathList//extractIDFromPathAsItemList[fopts[1]];
        idList = idDataList//Query[All,#ID&];
        JoinAcross[
            getItemDataFromIDAsList[fopts[2]][idList],
            idDataList,
            "ID"
        ]
    ];


getItemDataFromIDAsList//Options = {
    "fileNameRegulate"->True
};
getItemDataFromIDAsList[OptionsPattern[]][idList_] :=
    Module[ {itemList,idValidList,itemNameList,urlList,itemData},
        idValidList = 
            DeleteDuplicates@DeleteCases[idList,"notFound"];
        itemList = 
            idValidList//getItemFromValidIDListAsList;
        itemNameList = 
            itemList//Query[All,$arXivPDFNameFormat,FailureAction->"Replace"]//ifFileNameRegulate[OptionValue["fileNameRegulate"]];
        urlList = 
            getURLFromItem/@itemList;
        itemData = 
            MapThread[
                <|"ID"->#1,"item"->#2,"URL"->#3|>&,
                {idValidList,itemNameList,urlList}
            ];
        If[ MemberQ[idList,"notFound"],
            (*True*)
            Join[
                itemData,
                {<|"ID"->"notFound","item"->Missing["Failed"],"URL"->Missing["Failed"]|>}
            ],
            (*False*)
            itemData
        ]
    ];


getItemFromValidIDListAsList[idValidList_] :=
    If[ idValidList==={},
        (*if there is no valid ID, return empty list.*)
        {},
        (*improve robustness against failure of ServiceExecute.*)
        Enclose[
            ConfirmMatch[
                (*return the searched items.*)
                Quiet@Normal@ServiceExecute["ArXiv","Search",{"ID"->idValidList,MaxItems->Length@idValidList}],
                _List,
                Message[searchByID::connectionfailed]
            ],
            (*if the connection fails, return list of empty associations.*)
            Table[<||>,Length@idValidList]&
        ]
    ];    


getURLFromItem::usage = 
    "get the download URL from \"Link\".";
getURLFromItem[item_Association]/;MissingQ[item["ID"]] :=
    Missing["Failed"];
getURLFromItem[item_Association] :=
    item["Link"]//KeyUnion//
    	Query[Select[#Type==="application/pdf"&],FailureAction->"Replace"]//
			Query[All,"Href",FailureAction->"Replace"]//First//StringJoin[#,".pdf"]&;


ifFileNameRegulate[True] :=
    fileNameRegulate;
ifFileNameRegulate[False] :=
    Identity;


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
