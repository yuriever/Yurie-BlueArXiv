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
    Button[value,CopyToClipboard@value,Appearance->"Frameless",FrameMargins->Small];
lily`paper`Private`addButtonTo`copyToClipboard[_] :=
    Missing["Failed"];


(* ::Subsection:: *)
(*extractTitle*)


extractTitleFromPDF//Options = {
    "hidePath"->True,
    "titleExtractMethod"->"plusYAndFontSize",
    "YResolution"->25
};
extractTitleFromPDF[tag:"path"|"file",opts:OptionsPattern[]][arg_] :=
    lily`paper`extractTitle`kernel[tag,opts][arg]//addButtonTo["title"]//Dataset;


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*extractTitle*)


(* ::Subsection:: *)
(*Begin*)


Begin["`extractTitle`"];


(* ::Subsection:: *)
(*Kernel*)


kernel//Options = {
    "hidePath"->True,
    "titleExtractMethod"->"plusYAndFontSize",
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


(* ::Subsection:: *)
(*Helper*)


getTitleFromFileOrPath//Options = {
    "hidePath"->True,
    "titleExtractMethod"->"sortYAndFontSize",
    "YResolution"->25
};
(*acting on file*)
getTitleFromFileOrPath[file_String,"file",opts:OptionsPattern[]] :=
    Module[ {title,fopts},
        fopts = FilterRules[{opts},Options[useMethod]];
        title = useMethod[file,OptionValue["titleExtractMethod"],fopts]//regulateTitle;
        {<|"title"->title,"file"->file|>}
    ];
(*acting on path*)
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
            textRegulate[Import[file,{"PagePositionedText",1}],opts];
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
            textRegulate[Import[file,{"PagePositionedText",1}],opts];
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
			Map[lily`paper`Private`mergeByKey[{"string"->StringJoin,"X"->Min},First]]
    ];


hidePath[path_,file_] :=
    StringReplace[file,path~~"/"~~Longest[title__]~~EndOfString:>title];


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
