(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`PaperTool`extractTitleFromPDF`"];


Needs["Yurie`PaperTool`"];


(* ::Section:: *)
(*Public*)


extractTitleFromPDF;
extractTitleFromPathAsItemList;


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


Needs["Yurie`BlueArXiv`Common`"];
Needs["Yurie`BlueArXiv`Default`"];


(* ::Subsection:: *)
(*Options*)


getTitleFromPDFAsItemList//Options = {
    "hideDirectory"->True,
    "titleExtractMethod"->"sortYAndFontSize",
    "YResolution"->25
};

extractTitleFromPathAsItemList//Options = 
    Options@getTitleFromPDFAsItemList;

extractTitleFromPDF//Options = {
    "clickToCopy"->True,
    Splice@Options@extractTitleFromPathAsItemList
};


(* ::Subsection:: *)
(*Messages*)


extractTitleFromPDF::pdffailimport = 
    "the PDF file fails to import: \n``";


(* ::Subsection:: *)
(*extractTitleFromPDF*)


extractTitleFromPDF[opts:OptionsPattern[]][pathOrPathList_] :=
    Module[ {fopts},
        fopts = FilterRules[{opts},Options[extractTitleFromPathAsItemList]];
        pathOrPathList//extractTitleFromPathAsItemList[fopts]//ifAddButtonTo[OptionValue["clickToCopy"],"title"]//Dataset
    ];


extractTitleFromPathAsItemList[opts:OptionsPattern[]][pathOrPathList_] :=
    pathOrPathList//getPDFFromPathAsList//getTitleFromPDFAsItemList[opts]//DeleteDuplicates//Query[SortBy[#file&]];


getTitleFromPDFAsItemList[opts:OptionsPattern[]][file_] :=
    Module[ {title,itemList},
        title = file//recognizeTitleFromPDFBy[OptionValue["titleExtractMethod"],OptionValue["YResolution"]]//regulateTitle;
        itemList = {<|"title"->title,"file"->file|>};
        If[ OptionValue["hideDirectory"],
            itemList//Query[All,<|#,"file"->hideDirectory[#file]|>&],
            itemList
        ]
    ];

getTitleFromPDFAsItemList[opts:OptionsPattern[]][fileList_List] :=
    fileList//Map[getTitleFromPDFAsItemList[opts]]//Flatten;


(*search grouped texts with larger Y coordinate and fontsize.*)
recognizeTitleFromPDFBy["sortYAndFontSize",yresolution_][file_] :=
    Module[ {textData,counter,resultTextData,searchFirstNTexts},
        textData = 
            file//importFirstPageAsTextList//regulateTextList[yresolution];
        searchFirstNTexts[data_List,n_] :=
            Intersection[
                data//Query[ReverseSortBy[#Y&]]//Query[1;;n],
                data//Query[ReverseSortBy[#FontSize&]]//Query[1;;n]
            ];
        counter = 1;
        If[ textData==={},
            resultTextData = {},
            While[
                (resultTextData = searchFirstNTexts[textData,counter])==={},
                counter++
            ]
        ];
        (*if there are multiple texts, select one with longest #string.*)
        resultTextData//Query[MaximalBy[StringLength[#string]&]]//Query[1,#string&]
    ];

recognizeTitleFromPDFBy["sumYAndFontSize",yresolution_][file_] :=
    Module[ {textData,maxY,maxFontSize,resultTextData},
        textData = 
            file//importFirstPageAsTextList//regulateTextList[yresolution];
        maxY = 
            textData//Query[All,#Y&]//Max;
        maxFontSize = 
            textData//Query[All,#FontSize&]//Max;
        resultTextData = 
            textData//Query[All,<|#,"weight"->(#Y/maxY+#FontSize/maxFontSize)|>&]//Query[MaximalBy[#weight&]];
        (*if there are multiple texts, select one with longest #string.*)
        resultTextData//Query[MaximalBy[StringLength[#string]&]]//Query[1,#string&]
    ];


regulateTextList[yresolution_][text_Text] :=
    text/.Text[Style[string_String,_,styleOptions___Rule],coords_List,offset_List]:>
        KeyMap[ToString]@<|
            "string"->string,
            FilterRules[{styleOptions},{FontSize}],
            "X"->coords[[1]],
            "Y"->Round[coords[[2]],yresolution],
            "offset"->offset
        |>;

regulateTextList[yresolution_][textList_List] :=
    Module[ {textData},
        textData = regulateTextList[yresolution]/@textList;
        GatherBy[textData,#Y&]//Map[SortBy[#X&]]//Map[mergeByKey[{"string"->StringJoin,"X"->Min},First]]
    ];


hideDirectory[file_] :=
    First@getFileNameByExtension["pdf"][file];


getPDFFromPathAsList[pathOrPathList_] :=
    getFileByExtension["pdf"][pathOrPathList];


importFirstPageAsTextList[file_] :=
    Quiet[
        Check[
            Import[file,{"PagePositionedText",1}],
            Message[extractTitleFromPDF::pdffailimport,file];
            {Text[""]}
        ],
        All,
        {extractTitleFromPDF::pdffailimport}
    ];


regulateTitle//Attributes = 
    {Listable};

regulateTitle[""] = 
    "";

regulateTitle[arg_Missing] :=
    arg;

regulateTitle[string_String] :=
    string//StringSplit//Map[toLowerCase/*capitalize/*regulateFileName]//StringRiffle;


toLowerCase[string_] :=
    If[ Not@LowerCaseQ[string],
        ToLowerCase[string],
        string
    ];


capitalize[string_String] :=
    (*ignore the stop words.*)
    If[ DeleteStopwords[#]==#&[string],
        (*deal with hyphenated names.*)
        StringSplit[string,"-"]//Capitalize//StringRiffle[#,"-"]&,
        string
    ];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
