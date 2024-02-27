(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`BlueArXiv`extractID`"];


Needs["Yurie`BlueArXiv`"];

Needs["Yurie`BlueArXiv`Common`"];

Needs["Yurie`BlueArXiv`Default`"];


(* ::Section:: *)
(*Public*)


extractID::usage =
    "extract arXiv IDs from string or PDF file/folder path.";


extractIDFromStringAsItemList;

extractIDFromPathAsItemList;


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Option*)


getIDDataFromPDFAsList//Options = {
    "tryFileName"->True,
    "hideDirectory"->True
};

extractIDFromPathAsItemList//Options = {
    "mergeDuplicateID"->True,
    Splice@Options@getIDDataFromPDFAsList
};

extractID//Options = {
    "clickToCopy"->True,
    Splice@Options@extractIDFromPathAsItemList
};


(* ::Subsection:: *)
(*Message*)


extractID::pdffailimport =
    "the PDF file fails to import: \n``";


(* ::Subsection:: *)
(*Main*)


extractID["string",opts:OptionsPattern[]][stringOrStringList_] :=
    stringOrStringList//extractIDFromStringAsItemList//ifAddButton[OptionValue["clickToCopy"]];

extractID["path",opts:OptionsPattern[]][pathOrPathList_] :=
    Module[ {fopts},
        fopts = FilterRules[{opts,Options[extractID]},Options[extractIDFromPathAsItemList]];
        pathOrPathList//extractIDFromPathAsItemList[fopts]//ifAddButton[OptionValue["clickToCopy"],"ID"]//Dataset
    ];

extractID[opts:OptionsPattern[]][stringOrStringList_] :=
    extractID["string",opts][stringOrStringList];


(* ::Subsection:: *)
(*Helper*)


(*act on string*)
extractIDFromStringAsItemList[stringOrStringList_] :=
    stringOrStringList//getIDFromStringAsList//Sort;


(*act on path*)
extractIDFromPathAsItemList[opts:OptionsPattern[]][pathOrPathList_] :=
    Module[ {fopts},
        fopts = FilterRules[{opts,Options[extractIDFromPathAsItemList]},Options[getIDDataFromPDFAsList]];
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


getIDDataFromPDFAsList[OptionsPattern[]][file_] :=
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
    getFilePathByExtension["pdf"][pathOrPathList];


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
