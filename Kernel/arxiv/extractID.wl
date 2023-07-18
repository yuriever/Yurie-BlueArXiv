(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["lily`arxiv`extractID`"];

Needs["lily`paper`common`"];
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
extractID["string"][stringOrStringList_] :=
    extractIDFromStringAsItemList[stringOrStringList];
extractID["path",opts:OptionsPattern[]][pathOrPathList_] :=
    Module[ {fopts},
        fopts = FilterRules[{opts},Options[extractIDFromPathAsItemList]];
        extractIDFromPathAsItemList[fopts][pathOrPathList]//ifAddButtonTo[OptionValue["clickToCopy"],"ID"]//Dataset
    ];

extractID[][stringOrStringList_] :=
    extractID["string"][stringOrStringList];


(*act on string*)
extractIDFromStringAsItemList[stringOrStringList_] :=
    getIDFromStringAsList[stringOrStringList]//Sort;


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
    string//StringCases[Longest[id__]/;arXivIDQ[id]:>id]//DeleteDuplicates;
getIDFromStringAsList[stringList_List] :=
    stringList//Map[getIDFromStringAsList]//Flatten//DeleteDuplicates;


getIDDataFromPDFFirstPageAsList[file_] :=
    Module[ {idList,idNumber},
        idList = file//importPDFFirstPageAsStringList//getIDFromStringAsList;
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


importPDFFirstPageAsStringList[file_] :=
    Quiet[
        Check[
            StringSplit@Import[file,{"Plaintext",1}],
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
