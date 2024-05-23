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
    Module[ {fopts,idData},
        fopts =
            FilterRules[{opts,Options[extractID]},Options[extractIDData]];
        idData =
            extractIDData[tag,fopts][input];
        idData//ifAddButton[OptionValue["ClickToCopy"],"ID"]//Dataset
    ];


(* ::Subsection:: *)
(*Helper*)


extractIDData[tag_,opts:OptionsPattern[]][input_] :=
    Module[ {fopts,idData},
        fopts["image"] =
            FilterRules[{opts,Options[extractIDData]},Options[getIDDataFromImage]];
        fopts["path"] =
            FilterRules[{opts,Options[extractIDData]},Options[getIDDataFromPath]];
        idData =
            Switch[tag,
                "string",
                    input//getIDDataFromString,
                "image",
                    input//getIDDataFromImage[fopts["image"]],
                "path",
                    input//getIDDataFromPath[fopts["path"]]
            ];
        idData//ifMergeDuplicateID[OptionValue["MergeDuplicateID"]]//Query[SortBy[#ID&]]
    ];


(*the map from strings, images and files to IDs is many-to-one, hence all other key values except ID should be list.*)

ifMergeDuplicateID[True][list_] :=
    GatherBy[list,#ID&]//Map[mergeDataByKey[{"ID"->First},Flatten@*Join]]

ifMergeDuplicateID[False][list_] :=
    list;


(* ::Subsubsection:: *)
(*String*)


getIDDataFromString[str_String] :=
    str//getIDListFromString//Map[<|"ID"->#|>&];


getIDListFromString[str_String] :=
    StringCases[str,$arXivIDPattern];


(* ::Subsubsection:: *)
(*Image*)


getIDDataFromImage[opts:OptionsPattern[]][img_Image] :=
    Module[ {idData},
        idData =
            img//getStringListFromImage//
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
    TextRecognize[img,"Word",{"Text","BoundingBox"}]//alignToStringList;


(*if there is only one text recognized, the return value of TextRecognize is not a list of list.*)

alignToStringList[list_] :=
    If[ Head@First@list===String,
        {list},
        (*Else*)
        list
    ];


showHighlightedImage[idData_][img_] :=
    CellPrint@ExpressionCell[
        HighlightImage[img,{EdgeForm[{Transparent}],idData//Query[All,Tooltip[#Position,#ID]&]}],
        "Output",
        CellLabel->"Image="
    ];


(* ::Subsubsection:: *)
(*Path*)


getIDDataFromPath[opts:OptionsPattern[]][file_] :=
    file//getPDFListFromPath//Map[getIDDataFromPDF[opts]]//Flatten;


getPDFListFromPath[path_] :=
    getFilePathByExtension["pdf"][path];


getIDDataFromPDF[OptionsPattern[]][file_] :=
    Module[ {idData,idNumber,idList},
        If[ OptionValue["TryFileName"]===True,
            idList =
                getIDListFromString[file];
            idNumber =
                Length@idList;
            idData =
                Switch[ idNumber,
                    0,
                        getIDDataFromPDFFirstPage[file],
                    1,
                        {<|"ID"->First@idList,"FileName"->{file},"IDLocation"->{"FileName"}|>},
                    (*edge case: there are multiple IDs in the file name.*)
                    _,
                        MapThread[
                            <|"ID"->#1,"FileName"->#2,"IDLocation"->#3|>&,
                            {idList,ConstantArray[{file},idNumber],ConstantArray[{"FileNameExtra"},idNumber]}
                        ]
                ],
            (*Else*)
            idData =
                getIDDataFromPDFFirstPage[file]
        ];
        If[ OptionValue["HideDirectory"],
            idData//Query[All,<|#,"FileName"->hideDirectory[#FileName]|>&],
            (*Else*)
            idData
        ]
    ];


getIDDataFromPDFFirstPage[file_] :=
    Module[ {idList,idNumber},
        idList =
            file//tryImport["",{"Plaintext",1}]//getIDListFromString;
        idNumber =
            Length@idList;
        Switch[ idNumber,
            0,
                {<|"ID"->"NotFound","FileName"->{file},"IDLocation"->{"None"}|>},
            1,
                {<|"ID"->First@idList,"FileName"->{file},"IDLocation"->{"FirstPage"}|>},
            (*edge case: there are multiple IDs in the first page.*)
            _,
                MapThread[
                    <|"ID"->#1,"FileName"->#2,"IDLocation"->#3|>&,
                    {idList,ConstantArray[{file},idNumber],ConstantArray[{"FirstPageExtra"},idNumber]}
                ]
        ]
    ];


hideDirectory[file_] :=
    getFileNameByExtension["pdf"][file];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
