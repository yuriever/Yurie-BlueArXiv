(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["lily`paper`extractTitleFromPDF`"];

Needs["lily`paper`common`"];
Needs["lily`paper`"];


extractTitleFromPDF;


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
    "hidePath"->True,
    "titleExtractMethod"->"plusYAndFontSize",
    "YResolution"->25
};
extractTitleFromPDF[tag:"path"|"file",opts:OptionsPattern[]][arg_] :=
    kernel[tag,opts][arg]//addButtonTo["title"]//Dataset;


(* ::Subsubsection:: *)
(*Kernel*)


kernel//Options = {
    "hidePath"->True,
    "titleExtractMethod"->"sortYAndFontSize",
    "YResolution"->25
};
kernel[tag:"path"|"file",opts:OptionsPattern[]][fileOrPath_String] :=
    Module[ {fopts},
        fopts = FilterRules[{opts},Options[getTitleFromFileOrPath]];
        getTitleFromFileOrPath[fileOrPath,tag,fopts]//Query[SortBy[#file&]]
    ];
kernel[tag:"path"|"file",opts:OptionsPattern[]][list_List] :=
    Module[ {fopts},
        fopts = FilterRules[{opts},Options[getTitleFromFileOrPath]];
        getTitleFromFileOrPath[#,tag,fopts]&/@list//Flatten//DeleteDuplicates//Query[SortBy[#file&]]
    ];


(* ::Subsubsection:: *)
(*Helper*)


getTitleFromFileOrPath//Options = {
    "hidePath"->True,
    "titleExtractMethod"->"sortYAndFontSize",
    "YResolution"->25
};
getTitleFromFileOrPath[file_String,"file",opts:OptionsPattern[]] :=
    Module[ {title,fopts},
        fopts = FilterRules[{opts},Options[useMethod]];
        title = useMethod[file,OptionValue["titleExtractMethod"],fopts]//regulateTitle;
        {<|"title"->title,"file"->file|>}
    ];
getTitleFromFileOrPath[path_String,"path",opts:OptionsPattern[]] :=
    Module[ {fileList,titleDataList},
        fileList = FileNames[__~~".pdf"~~EndOfString,path];
        titleDataList = getTitleFromFileOrPath[#,"file",opts]&/@fileList//Flatten;
        If[ OptionValue["hidePath"]===True,
            titleDataList = titleDataList//Query[All,<|#,"file"->hidePath[path,#file]|>&]
        ];
        titleDataList
    ];


useMethod//Options = {
    "YResolution"->25
};
useMethod[file_,"sortYAndFontSize",opts:OptionsPattern[]] :=
    Module[ {textData,counter,resultTextData,searchFirstNTexts},
        textData = 
            textRegulate[importFirstPage@file,opts];
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
        resultTextData//Query[MaximalBy[StringLength[#string]&],FailureAction->"Replace"]//Query[1,#string&,FailureAction->"Replace"]
    ];
useMethod[file_,"plusYAndFontSize",opts:OptionsPattern[]] :=
    Module[ {textData,maxY,maxFontSize,textDataNormalized},
        textData = 
            textRegulate[importFirstPage@file,opts];
        maxY = 
            textData//Query[All,#Y&]//Max;
        maxFontSize = 
            textData//Query[All,#FontSize&]//Max;
        textDataNormalized = 
            textData//Query[All,<|#,"weight"->(#Y/maxY+#FontSize/maxFontSize)|>&];
        textDataNormalized//Query[MaximalBy[StringLength[#string]&],FailureAction->"Replace"]//Query[1,#string&,FailureAction->"Replace"]
    ];


textRegulate//Options = {
    "YResolution"->25
};
textRegulate[text_Text,opts:OptionsPattern[]] :=
    text/.Text[Style[string_String,color_,styleOptions___Rule],coords_List,offset_List]:>
        KeyMap[ToString]@<|
            "string"->string,
            FilterRules[{styleOptions},{FontSize}],
            "X"->coords[[1]],
            "Y"->Round[coords[[2]],OptionValue["YResolution"]],
            "offset"->offset
        |>;
textRegulate[textList_List,opts:OptionsPattern[]] :=
    Module[ {textData},
        textData = textRegulate[#,opts]&/@textList;
        GatherBy[textData,#Y&]//Map[SortBy[#X&]]//
			Map[mergeByKey[{"string"->StringJoin,"X"->Min},First]]
    ];


hidePath[path_,file_] :=
    StringReplace[file,path~~"/"~~Longest[title__]~~EndOfString:>title];


importFirstPage[file_] :=
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
    string//StringSplit//toLowerCase//capitalize//
    	StringReplace[regulateTitleRuleList]//StringRiffle;
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
