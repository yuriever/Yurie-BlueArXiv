(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["lily`arxiv`extractID`"];

Needs["lily`arxiv`common`"];
Needs["lily`arxiv`"];


extractID;
extractIDFromStringAsItemList;
extractIDFromPathAsItemList;


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*extractID*)


extractID//Options = {
    "tryFileName"->True,
    "hideDirectory"->True,
    "mergeDuplicateID"->True,
    "clickToCopy"->True
};
extractID::pdffailimport = 
    "the PDF file fails to import: \n``";
extractID["string",opts:OptionsPattern[]][stringOrStringList_] :=
    stringOrStringList//extractIDFromStringAsItemList//ifAddButtonTo[OptionValue["clickToCopy"]];
extractID["path",opts:OptionsPattern[]][pathOrPathList_] :=
    Module[ {fopts},
        fopts = FilterRules[{opts},Options[extractIDFromPathAsItemList]];
        pathOrPathList//extractIDFromPathAsItemList[fopts]//ifAddButtonTo[OptionValue["clickToCopy"],"ID"]//Dataset
    ];

extractID[opts:OptionsPattern[]][stringOrStringList_] :=
    extractID["string",opts][stringOrStringList];


(*act on string*)
extractIDFromStringAsItemList[stringOrStringList_] :=
    stringOrStringList//getIDFromStringAsList//Sort;


(*act on path*)
extractIDFromPathAsItemList//Options = {
    "tryFileName"->True,
    "hideDirectory"->True,
    "mergeDuplicateID"->True
};
extractIDFromPathAsItemList[opts:OptionsPattern[]][pathOrPathList_] :=
    Module[ {fopts},
        fopts = FilterRules[{opts},Options[getIDDataFromPDFAsList]];
        pathOrPathList//getPDFFromPathAsList//getIDDataFromPDFAsList[fopts]//ifGatherAndSortByID[OptionValue["mergeDuplicateID"]]
    ];


getIDFromStringAsList[string_String] :=
    string//StringCases[$arXivIDPattern]//DeleteDuplicates;
getIDFromStringAsList[stringList_List] :=
    stringList//Map[getIDFromStringAsList]//Flatten//DeleteDuplicates;



getIDDataFromPDFFirstPageAsList[file_] :=
    Module[ {idList,idNumber},
        idList = file//importPDFFirstPageAsString//getIDFromStringAsList;
        idNumber = Length@idList;
        Switch[ idNumber,
            0,
                {<|"ID"->"notFound","file"->{file},"IDLocation"->{"notFoundInFirstPage"}|>},
            1,
                {<|"ID"->First@idList,"file"->{file},"IDLocation"->{"foundInFirstPage"}|>},
            _,
                MapThread[
                    <|"ID"->#1,"file"->#2,"IDLocation"->#3|>&,
                    {idList,ConstantArray[{file},idNumber],ConstantArray[{"extraInFirstPage"},idNumber]}
                ]                
        ]
    ];


getIDDataFromPDFAsList//Options = {
    "tryFileName"->True,
    "hideDirectory"->True
};
getIDDataFromPDFAsList[opts:OptionsPattern[]][file_] :=
    Module[ {idData,idNumber,idList},
        If[ OptionValue["tryFileName"],
            (*True*)
            idList = getIDFromStringAsList[file];
            idNumber = Length@idList;
            idData = 
                Switch[ idNumber,
                    0,
                        getIDDataFromPDFFirstPageAsList[file],
                    1,
                        {<|"ID"->First@idList,"file"->{file},"IDLocation"->{"foundInFileName"}|>},
                    _,
                        MapThread[
                            <|"ID"->#1,"file"->#2,"IDLocation"->#3|>&,
                            {idList,ConstantArray[{file},idNumber],ConstantArray[{"extraInFileName"},idNumber]}
                        ]
                ],
            (*False*)
            idData = getIDDataFromPDFFirstPageAsList[file]
        ];
        If[ OptionValue["hideDirectory"],
            idData//Query[All,<|#,"file"->hideDirectory[#file]|>&],
            idData
        ]
    ];
getIDDataFromPDFAsList[opts:OptionsPattern[]][fileList_List] :=
    fileList//Map[getIDDataFromPDFAsList[opts]]//Flatten;


hideDirectory[file_] :=
    getFileNameByExtension["pdf"][file];


getPDFFromPathAsList[pathOrPathList_] :=
    getFileByExtension["pdf"][pathOrPathList];


ifGatherAndSortByID[True][list_] :=
    GatherBy[list,#ID&]//Map[Merge[Flatten@*Join]]//Query[All,<|#,"ID"->First@#ID|>&]//Query[SortBy[#ID&]];
ifGatherAndSortByID[False][list_] :=
    list//Query[SortBy[#ID&]];


importPDFFirstPageAsString[file_] :=
    Quiet[
        Check[
            Import[file,{"Plaintext",1}],
            (*fail*)
            Message[extractID::pdffailimport,file];
            {}
        ],
        All,
        {extractID::pdffailimport}
    ];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
