(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`BlueArXiv`searchByID`"];


Needs["Yurie`BlueArXiv`common`"];
Needs["Yurie`BlueArXiv`"];
Needs["Yurie`BlueArXiv`extractID`"];


searchByID;
searchByIDAsItemList;


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Options and messages*)


(*getItemDataFromIDAsList//Options = {
    "fileNameRegulate"->True
};

searchByIDFromStringAsItemList//Options = 
    Options@getItemDataFromIDAsList;

searchByIDFromPathAsItemList//Options = {
    Splice@Options@extractIDFromPathAsItemList,
    Splice@Options@getItemDataFromIDAsList
};*)

searchByIDFromPathAsItemList//Options = 
    Options@extractIDFromPathAsItemList;

searchByIDAsItemList//Options = 
    Options@searchByIDFromPathAsItemList;

searchByID//Options = {
    "clickToCopy"->True,
    Splice@Options@searchByIDAsItemList
};


searchByID::connectionfailed =
    "connection failed.";


(* ::Subsection:: *)
(*searchByID*)


searchByID[tag:"string"|"path":"string",opts:OptionsPattern[]][arg_] :=
    Module[ {fopts},
        fopts = FilterRules[{opts},Options[searchByIDAsItemList]];
        searchByIDAsItemList[tag,fopts][arg]//ifAddButtonTo[OptionValue["clickToCopy"],"ID","item","URL"]//Dataset
    ];


searchByIDAsItemList["string",opts:OptionsPattern[]][stringOrStringList_] :=
    searchByIDFromStringAsItemList[opts][stringOrStringList];

searchByIDAsItemList["path",opts:OptionsPattern[]][pathOrPathList_] :=
    searchByIDFromPathAsItemList[opts][pathOrPathList];


searchByIDFromStringAsItemList[opts:OptionsPattern[]][stringOrStringList_] :=
    stringOrStringList//extractIDFromStringAsItemList//getItemDataFromIDAsList[opts];


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


getItemDataFromIDAsList[opts:OptionsPattern[]][idList_] :=
    Module[ {itemList,idValidList,itemNameList,urlList,itemData},
        idValidList = 
            DeleteDuplicates@DeleteCases[idList,"notFound"];
        itemList = 
            idValidList//getItemFromValidIDListAsList;
        itemNameList = 
            itemList//Query[All,$arXivPDFNameFormatter,FailureAction->"Replace"]//
            	Map[Switch[#,_Missing,#,_,$arXivPDFNameRegulator]&];
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


(*ifFileNameRegulate[True] :=
    fileNameRegulate;

ifFileNameRegulate[False] :=
    Identity;*)


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
