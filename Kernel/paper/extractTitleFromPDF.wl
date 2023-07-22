(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["lily`paper`extractTitleFromPDF`"];

Needs["lily`arxiv`common`"];
Needs["lily`paper`"];


extractTitleFromPDF;
extractTitleFromPathAsItemList;


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*extractTitleFromPDF*)


extractTitleFromPDF::pdffailimport = 
    "the PDF file fails to import: \n``";
extractTitleFromPDF//Options = {
    "hideDirectory"->True,
    "clickToCopy"->True,
    "titleExtractMethod"->"sortYAndFontSize",
    "YResolution"->25
};
extractTitleFromPDF[opts:OptionsPattern[]][pathOrPathList_] :=
    Module[ {fopts},
        fopts = FilterRules[{opts},Options[extractTitleFromPathAsItemList]];
        pathOrPathList//extractTitleFromPathAsItemList[fopts]//ifAddButtonTo[OptionValue["clickToCopy"],"title"]//Dataset
    ];


extractTitleFromPathAsItemList//Options = {
    "hideDirectory"->True,
    "titleExtractMethod"->"sortYAndFontSize",
    "YResolution"->25
};

extractTitleFromPathAsItemList[opts:OptionsPattern[]][pathOrPathList_] :=
    Module[ {fopts},
        fopts = FilterRules[{opts},Options[getTitleFromPDFAsItemList]];
        pathOrPathList//getPDFFromPathAsList//getTitleFromPDFAsItemList[fopts]//DeleteDuplicates//Query[SortBy[#file&]]
    ];


getTitleFromPDFAsItemList//Options = {
    "hideDirectory"->True,
    "titleExtractMethod"->"sortYAndFontSize",
    "YResolution"->25
};
getTitleFromPDFAsItemList[opts:OptionsPattern[]][file_] :=
    Module[ {title,fopts,itemList},
        fopts = FilterRules[{opts},Options[recognizeTitleFromPDFBy]];
        title = file//recognizeTitleFromPDFBy[OptionValue["titleExtractMethod"],fopts]//regulateTitle;
        itemList = {<|"title"->title,"file"->file|>};
        If[ OptionValue["hideDirectory"],
            itemList//Query[All,<|#,"file"->hideDirectory[#file]|>&],
            itemList
        ]
    ];
getTitleFromPDFAsItemList[opts:OptionsPattern[]][fileList_List] :=
    fileList//Map[getTitleFromPDFAsItemList[opts]]//Flatten;


recognizeTitleFromPDFBy//Options = {
    "YResolution"->25
};
(*search grouped texts with larger Y coordinate and fontsize.*)
recognizeTitleFromPDFBy["sortYAndFontSize",opts:OptionsPattern[]][file_] :=
    Module[ {textData,counter,resultTextData,searchFirstNTexts},
        textData = 
            file//importFirstPageAsTextList//regulateTextList[opts];
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
recognizeTitleFromPDFBy["sumYAndFontSize",opts:OptionsPattern[]][file_] :=
    Module[ {textData,maxY,maxFontSize,resultTextData},
        textData = 
            file//importFirstPageAsTextList//regulateTextList[opts];
        maxY = 
            textData//Query[All,#Y&]//Max;
        maxFontSize = 
            textData//Query[All,#FontSize&]//Max;
        resultTextData = 
            textData//Query[All,<|#,"weight"->(#Y/maxY+#FontSize/maxFontSize)|>&]//Query[MaximalBy[#weight&]];
        (*if there are multiple texts, select one with longest #string.*)
        resultTextData//Query[MaximalBy[StringLength[#string]&]]//Query[1,#string&]
    ];


regulateTextList//Options = {
    "YResolution"->25
};
regulateTextList[OptionsPattern[]][text_Text] :=
    text/.Text[Style[string_String,_,styleOptions___Rule],coords_List,offset_List]:>
        KeyMap[ToString]@<|
            "string"->string,
            FilterRules[{styleOptions},{FontSize}],
            "X"->coords[[1]],
            "Y"->Round[coords[[2]],OptionValue["YResolution"]],
            "offset"->offset
        |>;
regulateTextList[opts:OptionsPattern[]][textList_List] :=
    Module[ {textData},
        textData = regulateTextList[opts]/@textList;
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
    string//StringSplit//toLowerCase//capitalize//StringReplace[regulateTitleRuleList]//StringRiffle;
regulateTitleRuleList =
    {
        ":"->"/",
        "\n"->" ",
        "\[CloseCurlyQuote]"->"'"
    };


toLowerCase//Attributes = 
    {Listable};
toLowerCase[string_] :=
    If[ Not@LowerCaseQ[string],
        ToLowerCase[string],
        string
    ];


capitalize//Attributes = 
    {Listable};
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
