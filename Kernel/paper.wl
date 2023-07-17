(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["lily`paper`"];


Unprotect@@Names[$Context<>"*"];
ClearAll@@Names[$Context<>"*"]


(* ::Section:: *)
(*Usage*)


extractTitleFromPDF::usage = 
    "extract title from first page of PDF by searching grouped texts with larger Y coordinates and fontsize.";

extractCiteKeyFromTeX::usage = 
    "extract cite keys from TeX files.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Common*)


mergeByKey::usage =
    "ResourceFunction[\"MergeByKey\"]: merge a list of associations using different merge functions according to keys. The default merging function is Identity.\n"<>
    "mergeByKey[{assoc1,assoc2,...},{key1->f1,key2->f2,...},f]\n"<>
    "mergeByKey[{assoc1,assoc2,...},{...,{keyi1,keyi2,...}->fi,...},...]";
mergeByKey[ruleList:{___Rule},default:_:Identity][data:{___?AssociationQ}] :=
    mergeByKey[data,ruleList,default];
mergeByKey[{<||>...},{___Rule},Repeated[_,{0,1}]] :=
    <||>;
mergeByKey[data:{__?AssociationQ},ruleList:{___Rule},default:_:Identity] :=
    Module[ {missingToken,assoc,keys,queryRules,mergeRules},
        (*missingToken: unique symbol that is used for identifying where the undefined keys were after transposing the association *)
        mergeRules = 
            Replace[
                Flatten@Replace[
                    ruleList,
                    Verbatim[Rule][list_List,fun_]:>Thread[list->fun],
                    {1}
                ],
                Verbatim[Rule][Key[k_],fun_]:>Rule[k,fun],
                {1}
            ];
        (*avoid KeyUnion if it's not necessary.*)
        If[ SameQ@@Keys[data],
            assoc = data,
            assoc = KeyUnion[DeleteCases[data,<||>],missingToken&]
        ];
        keys = Keys@First@assoc;
        (*this is essentially how GeneralUtilities`AssociationTranspose works.*)
        assoc = 
            AssociationThread[
                keys,
                If[ SameQ@@Keys[data],
                    Transpose@Values[assoc],
                    DeleteCases[Transpose@Values[assoc],missingToken,{2}]
                ]
            ];
        keys = Key/@keys;
        queryRules = 
            DeleteCases[
                Thread[
                    keys->Lookup[mergeRules,keys,default]
                ],
                _->Identity
            ];
        If[ MatchQ[queryRules,{__Rule}],
            Query[queryRules]@assoc,
            assoc
        ]
    ];


addButtonTo[key_String][list_] :=
    With[ {$$key = key},
        list//Query[All,<|#,$$key->lily`paper`Private`addButtonTo`copyToClipboard[Slot[$$key]]|>&]
    ];
addButtonTo[key_,restKeys__][list_] :=
    list//addButtonTo[key]//addButtonTo[restKeys];

lily`paper`Private`addButtonTo`copyToClipboard[value_String] :=
    Interpretation[{},
        Button[value,CopyToClipboard@value,Appearance->"Frameless",FrameMargins->Small],
        value
    ];
lily`paper`Private`addButtonTo`copyToClipboard[_] :=
    Missing["Failed"];


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
    extractTitleFromPDF`kernel[tag,opts][arg]//addButtonTo["title"]//Dataset;


(* ::Subsubsection:: *)
(*Kernel*)


extractTitleFromPDF`kernel//Options = {
    "hidePath"->True,
    "titleExtractMethod"->"plusYAndFontSize",
    "YResolution"->25
};
extractTitleFromPDF`kernel[tag:"path"|"file",opts:OptionsPattern[]][fileOrPath_String] :=
    Module[ {fopts},
        fopts = FilterRules[{opts},Options[extractTitleFromPDF`getTitleFromFileOrPath]];
        extractTitleFromPDF`getTitleFromFileOrPath[fileOrPath,tag,fopts]//Query[SortBy[#file&]]
    ];
extractTitleFromPDF`kernel[tag:"path"|"file",opts:OptionsPattern[]][list_List] :=
    Module[ {fopts},
        fopts = FilterRules[{opts},Options[extractTitleFromPDF`getTitleFromFileOrPath]];
        extractTitleFromPDF`getTitleFromFileOrPath[#,tag,fopts]&/@list//Flatten//DeleteDuplicates//Query[SortBy[#file&]]
    ];


(* ::Subsubsection:: *)
(*Helper*)


extractTitleFromPDF`getTitleFromFileOrPath//Options = {
    "hidePath"->True,
    "titleExtractMethod"->"plusYAndFontSize",
    "YResolution"->25
};
extractTitleFromPDF`getTitleFromFileOrPath[file_String,"file",opts:OptionsPattern[]] :=
    Module[ {title,fopts},
        fopts = FilterRules[{opts},Options[extractTitleFromPDF`useMethod]];
        title = extractTitleFromPDF`useMethod[file,OptionValue["titleExtractMethod"],fopts]//extractTitleFromPDF`regulateTitle;
        {<|"title"->title,"file"->file|>}
    ];
extractTitleFromPDF`getTitleFromFileOrPath[path_String,"path",opts:OptionsPattern[]] :=
    Module[ {fileList,titleDataList},
        fileList = FileNames[__~~".pdf"~~EndOfString,path];
        titleDataList = extractTitleFromPDF`getTitleFromFileOrPath[#,"file",opts]&/@fileList//Flatten;
        If[ OptionValue["hidePath"]===True,
            titleDataList = titleDataList//Query[All,<|#,"file"->extractTitleFromPDF`hidePath[path,#file]|>&]
        ];
        titleDataList
    ];

extractTitleFromPDF`useMethod//Options = {
    "YResolution"->25
};
extractTitleFromPDF`useMethod[file_,"sortYAndFontSize",opts:OptionsPattern[]] :=
    Module[ {textData,counter,resultTextData,searchFirstNTexts},
        textData = 
            extractTitleFromPDF`textRegulate[extractTitleFromPDF`importFirstPage@file,opts];
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
extractTitleFromPDF`useMethod[file_,"plusYAndFontSize",opts:OptionsPattern[]] :=
    Module[ {textData,maxY,maxFontSize,textDataNormalized},
        textData = 
            extractTitleFromPDF`textRegulate[extractTitleFromPDF`importFirstPage@file,opts];
        maxY = 
            textData//Query[All,#Y&]//Max;
        maxFontSize = 
            textData//Query[All,#FontSize&]//Max;
        textDataNormalized = 
            textData//Query[All,<|#,"weight"->(#Y/maxY+#FontSize/maxFontSize)|>&];
        textDataNormalized//Query[MaximalBy[StringLength[#string]&],FailureAction->"Replace"]//Query[1,#string&,FailureAction->"Replace"]
    ];

extractTitleFromPDF`textRegulate//Options = {
    "YResolution"->25
};
extractTitleFromPDF`textRegulate[text_Text,opts:OptionsPattern[]] :=
    text/.Text[Style[string_String,color_,styleOptions___Rule],coords_List,offset_List]:>
        KeyMap[ToString]@<|
            "string"->string,
            FilterRules[{styleOptions},{FontSize}],
            "X"->coords[[1]],
            "Y"->Round[coords[[2]],OptionValue["YResolution"]],
            "offset"->offset
        |>;
extractTitleFromPDF`textRegulate[textList_List,opts:OptionsPattern[]] :=
    Module[ {textData},
        textData = extractTitleFromPDF`textRegulate[#,opts]&/@textList;
        GatherBy[textData,#Y&]//Map[SortBy[#X&]]//
			Map[mergeByKey[{"string"->StringJoin,"X"->Min},First]]
    ];

extractTitleFromPDF`hidePath[path_,file_] :=
    StringReplace[file,path~~"/"~~Longest[title__]~~EndOfString:>title];

extractTitleFromPDF`importFirstPage[file_] :=
    Quiet[
        Check[
            Import[file,{"PagePositionedText",1}],
            Message[extractTitleFromPDF::pdffailimport,file];
            Text[""]
        ],
        All,
        {extractTitleFromPDF::pdffailimport}
    ];

extractTitleFromPDF`regulateTitle//Attributes = 
    {Listable};
extractTitleFromPDF`regulateTitle[""] = 
    "";
extractTitleFromPDF`regulateTitle[arg_Missing] :=
    arg;
extractTitleFromPDF`regulateTitle[string_String] :=
    string//StringSplit//extractTitleFromPDF`toLowerCase//extractTitleFromPDF`capitalize//
    	StringReplace[extractTitleFromPDF`regulateTitleRuleList]//StringRiffle;
extractTitleFromPDF`regulateTitleRuleList =
    {
        ":"->"/",
        "\n"->" ",
        "\[CloseCurlyQuote]"->"'"
    };

extractTitleFromPDF`toLowerCase//Attributes = 
    {Listable};
extractTitleFromPDF`toLowerCase[string_] :=
    If[ Not@LowerCaseQ[string],
        ToLowerCase[string],
        string
    ];

extractTitleFromPDF`capitalize//Attributes = 
    {Listable};
extractTitleFromPDF`capitalize[string_String] :=
    (*ignore the stop words.*)
    If[ DeleteStopwords[#]==#&[string],
        (*deal with hyphenated names.*)
        StringSplit[string,"-"]//Capitalize//StringRiffle[#,"-"]&,
        string
    ];


(* ::Subsection:: *)
(*extractCiteKeyFromTeX*)


extractCiteKeyFromTeX::texfailimport = 
    "the TeX file fails to import: \n``";
extractCiteKeyFromTeX//Options = {
};
extractCiteKeyFromTeX[tag:"path"|"file",opts:OptionsPattern[]][arg_] :=
    {};


(* ::Subsubsection:: *)
(*Kernel*)


extractCiteKeyFromTeX`kernel//Options = {
};
extractCiteKeyFromTeX`kernel[tag:"path"|"file",opts:OptionsPattern[]][fileOrPath_String] :=
    Module[ {},
        extractCiteKeyFromTeX`getCiteKeyFromTeX[fileOrPath,tag,fopts]//Query[SortBy[#file&]]
    ];
extractCiteKeyFromTeX`kernel[tag:"path"|"file",opts:OptionsPattern[]][list_List] :=
    Module[ {fopts},
        fopts = FilterRules[{opts},Options[getTitleFromFileOrPath]];
        getTitleFromFileOrPath[#,tag,fopts]&/@list//Flatten//DeleteDuplicates//Query[SortBy[#file&]]
    ];


extractCiteKeyFromTeX`getCiteKeyFromTeX[file_,"file"] :=
    file//extractCiteKeyFromTeX`importStringFromTeX//StringSplit//StringCases[RegularExpression["(\\\\cite{)(\\S*?)(})"]:>"$2"]//
    	Flatten//StringSplit[#,","]&//Flatten//DeleteDuplicates//DeleteCases[""];

extractCiteKeyFromTeX`getCiteKeyFromTeX[path_,"path"] :=
    extractCiteKeyFromTeX`getCiteKeyFromTeX[#,"file"]&/@FileNames[__~~".tex"~~EndOfString,path];


extractCiteKeyFromTeX`importStringFromTeX[file_] :=
    Quiet[
        Check[
            Import[file,"Text"],
            Message[extractCiteKeyFromTeX::texfailimport,file];
            ""
        ],
        All,
        {extractCiteKeyFromTeX::texfailimport}
    ];    


(* ::Subsubsection:: *)
(*Helper*)


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
