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
    "extract arXiv IDs from string, image or PDF file/directory path.";


extractIDData;


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Option*)


getIDDataFromPDF//Options = {
    "TryFileName"->True,
    "HideDirectory"->True
};

getIDDataFromPath//Options =
    Options@getIDDataFromPDF;

getIDDataFromImage//Options = {
    "ShowHighlightedImage"->True
};

extractIDData//Options = {
    "MergeDuplicateID"->True,
    Splice@Options@getIDDataFromPath,
    Splice@Options@getIDDataFromImage
};

extractID//Options = {
    "ClickToCopy"->True,
    Splice@Options@extractIDData
};


(* ::Subsection:: *)
(*Main*)


extractID[tag:$tagPattern:"string",opts:OptionsPattern[]][input_] :=
    Module[ {idData},
        idData =
            input//throwWrongTypeInput[tag]//extractIDData[tag,FilterRules[{opts,Options[extractID]},Options[extractIDData]]];
        idData//ifAddButton[OptionValue["ClickToCopy"],"ID"]//Dataset
    ]//Catch;


(* ::Subsection:: *)
(*Helper*)


extractIDData[tag:$tagPattern,opts:OptionsPattern[]][input_] :=
    Module[ {idData},
        idData =
            Switch[tag,
                "string",
                    input//getIDDataFromString,
                "image",
                    input//getIDDataFromImage[FilterRules[{opts,Options[extractIDData]},Options[getIDDataFromImage]]],
                "path",
                    input//getIDDataFromPath[FilterRules[{opts,Options[extractIDData]},Options[getIDDataFromPath]]]
            ];
        idData//ifMergeDuplicateID[OptionValue["MergeDuplicateID"]]//Query[SortBy[#ID&]]
    ];


(*the map from strings, images and files to IDs is many-to-one, hence all other key values except ID should be list.*)

ifMergeDuplicateID[True][list_List] :=
    GatherBy[list,#ID&]//Map[mergeDataByKey[{"ID"->First},Flatten@*Join]]

ifMergeDuplicateID[False][list_List] :=
    list;


(* ::Subsubsection:: *)
(*String*)


getIDDataFromString[str_String] :=
    str//getIDListFromString//Map[<|"ID"->#|>&];


getIDListFromString[str_String] :=
    StringCases[str,$arXivIDPattern];


(* ::Subsubsection:: *)
(*Image*)


getIDDataFromImage[OptionsPattern[]][Null] :=
    {};

getIDDataFromImage[OptionsPattern[]][img_Image] :=
    Module[ {idData},
        idData =
            img//getStringListFromImage//alignToStringList//
		        Query[All,<|"ID"->First[getIDListFromString@#[[1]],""],"Position"->{#[[2]]}|>&]//
		            Query[Select[#ID=!=""&]];
        If[ OptionValue["ShowHighlightedImage"],
            showHighlightedImage[idData][img];
            idData//Query[All,KeyDrop["Position"]],
            (*Else*)
            idData
        ]
    ];


getStringListFromImage[img_Image] :=
    TextRecognize[img,"Word",{"Text","BoundingBox"}];


alignToStringList[list_List] :=
    Which[
        (*if there is no text recognized, return an empty list.*)
        list==={},
            {},
        (*if there is only one text recognized, the returned value of TextRecognize is not a list of list.*)
        Head@First@list===String,
            {list},
        True,
            list
    ];


(*if there is no text recognized, do not show the image.*)
showHighlightedImage[{}][img_Image] :=
    Null;

showHighlightedImage[idData_List][img_Image] :=
    CellPrint@ExpressionCell[
        HighlightImage[img,{EdgeForm[{Transparent}],idData//Query[All,Tooltip[#Position,#ID]&]}],
        "Output",
        CellLabel->"Image="
    ];


(* ::Subsubsection:: *)
(*Path*)


getIDDataFromPath[opts:OptionsPattern[]][path:$pathPattern] :=
    path//getPDFListFromPath//Map[getIDDataFromPDF[FilterRules[{opts,Options@getIDDataFromPath},Options@getIDDataFromPDF]]]//Flatten;


getPDFListFromPath[path:$pathPattern] :=
    getFilePathByExtension["pdf"][path];


getIDDataFromPDF[OptionsPattern[]][filePath_String] :=
    Module[ {idData,idNumber,idList},
        If[ OptionValue["TryFileName"]===True,
            idList =
                getIDListFromString[filePath];
            idNumber =
                Length@idList;
            idData =
                Switch[ idNumber,
                    0,
                        getIDDataFromPDFFirstPage[filePath],
                    1,
                        {<|"ID"->First@idList,"FileName"->{filePath},"IDLocation"->{"FileName"}|>},
                    (*edge case: there are multiple IDs in the file name.*)
                    _,
                        MapThread[
                            <|"ID"->#1,"FileName"->#2,"IDLocation"->#3|>&,
                            {idList,ConstantArray[{filePath},idNumber],ConstantArray[{"FileNameExtra"},idNumber]}
                        ]
                ],
            (*Else*)
            idData =
                getIDDataFromPDFFirstPage[filePath]
        ];
        If[ OptionValue["HideDirectory"],
            idData//Query[All,<|#,"FileName"->hideDirectory[#FileName]|>&],
            (*Else*)
            idData
        ]
    ];


getIDDataFromPDFFirstPage[filePath_String] :=
    Module[ {idList,idNumber},
        idList =
            filePath//tryImport["",{"Plaintext",1}]//getIDListFromString;
        idNumber =
            Length@idList;
        Switch[ idNumber,
            0,
                {<|"ID"->"NotFound","FileName"->{filePath},"IDLocation"->{"None"}|>},
            1,
                {<|"ID"->First@idList,"FileName"->{filePath},"IDLocation"->{"FirstPage"}|>},
            (*edge case: there are multiple IDs in the first page.*)
            _,
                MapThread[
                    <|"ID"->#1,"FileName"->#2,"IDLocation"->#3|>&,
                    {idList,ConstantArray[{filePath},idNumber],ConstantArray[{"FirstPageExtra"},idNumber]}
                ]
        ]
    ];


hideDirectory[filePathList_List] :=
    getFileNameByExtension["pdf"][filePathList];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
