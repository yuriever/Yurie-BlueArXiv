(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["lily`arxiv`"];
Unprotect@@Names[$Context<>"*"];
ClearAll@@Names[$Context<>"*"]


arXivConnect::usage =
    "connect to arXiv API.";
arXivDisconnect::usage =
    "disconnect from arXiv API.";
arXivIDQ::usage =
    "check whether a string is a valid arXiv ID.";


fileNameFormat::usage = 
    "set the format of file names.";
fileNameInPath::usage =
    "return a list of PDF file names in the path";
fileNameRegulate::usage =
    "regulate the file name with characters like \"/\" and \"\n\".";


extractID::usage =
    "extract arXiv IDs from string, file name or path.";
searchByID::usage = 
    "search by IDs extracted from string, file or path, 
    and return the found items on arXiv with formatted names by fileNameFormatter";
downloadByID::usage = 
    "download by IDs extracted from string, file or path to the target path, 
    and return the file objects with formatted names by fileNameFormatter."


extractTitle::usage = 
    "extract title from PDF by searching grouped texts with larger Y coordinates and fontsize.";
searchByTitle::usage = 
    "search by titles extracted from file or path, 
    and return the best-matched items on arXiv with formatted names by fileNameFormatter. 
	The best match item is picked by minimizing EditDistance.";
downloadByTitle::usage = 
    "download by titles extracted from file or path to the target path, 
    and return the file objects with formatted names by fileNameFormatter."


Begin["`Private`"];


(* ::Section:: *)
(*Private*)


pink[expr_] :=
    Style[expr,RGBColor[1,0.5,0.5]];
violet[expr_] :=
    Style[expr,RGBColor[0.5,0.5,1]];
orange[expr_] :=
    Style[expr,RGBColor[1,0.5,0]];

echo//Attributes = {HoldAll};
echo[code_] :=
    Module[ {codeResult},
        codeResult = code;
        Print[
            pink@ToString@Unevaluated@code,
            " = ",
            violet@codeResult
        ];
        codeResult
    ];

associationTranspose =
    GeneralUtilities`AssociationTranspose;

mergeByKey[rules:{___Rule},default:_:Identity][data:{___?AssociationQ}] :=
    mergeByKey[data,rules,default];
mergeByKey[{<||>...},{___Rule},Repeated[_,{0,1}]] :=
    <||>;
mergeByKey[data:{__?AssociationQ},rules:{___Rule},default:_:Identity] :=
    Module[ {
            (* unique symbol that is used for identifying where the undefined keys were after transposing the association *)
            missingToken,
            assoc,
            keys,
            queryRules,
            mergeRules = 
                Replace[
                    Flatten@Replace[
                        rules,
                        Verbatim[Rule][lst_List,fun_]:>Thread[lst->fun],
                        {1}
                    ],
                    Verbatim[Rule][Key[k_],fun_]:>k->fun,
                    {1}
                ],
            keysSameQ = SameQ@@Keys[data]
        },
        (* avoid KeyUnion if it's not necessary *)
        If[ keysSameQ,
            assoc = data,
            assoc = KeyUnion[DeleteCases[data,<||>],missingToken&]
        ];
        keys = Keys[First@assoc];
        (* this is essentially how GeneralUtilities`AssociationTranspose works *)
        assoc = 
            AssociationThread[
                keys,
                If[ keysSameQ,
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


(* ::Subsection:: *)
(*arXivConnector*)


arXivConnector::usage =
    "a private symbol representing the connector to arXiv API.";
arXivConnect[] :=
    arXivConnector = ServiceConnect["ArXiv"];
arXivDisconnect[] :=
    ServiceDisconnect[arXivConnector];


arXivIDQ[string_String] :=
    StringMatchQ[
        string,
        arXivIDQ`IDNewFormat|arXivIDQ`IDOldFormat
    ];
arXivIDQ[_] = False;

arXivIDQ`IDNewFormat =
    RegularExpression["\\d{4}\\.\\d{4,5}"];    
arXivIDQ`IDOldFormat =
    RegularExpression[
        "astro-ph|cond-mat|gr-qc|hep-ex|hep-lat|hep-ph|hep-th|math-ph|
        	nlin|nucl-ex|nucl-th|physics|quant-ph|math|cs"
    ]~~"/"~~RegularExpression["\\d{7}"];


(* ::Subsection:: *)
(*fileNameFormat*)


fileNameFormatter::usage = 
    "formattor of file names, set by fileNameFormat.";
fileNameFormat//Attributes = {HoldAll};
fileNameFormat[format_] :=
    Module[ {},
        fileNameFormatter = Hold[format]/.fileNameFormat`keyWordToFunction/.
            {Hold[expr_]:>Hold[(expr)&]}//ReleaseHold;
    ];
fileNameFormat`keyWordToFunction = {
    "ID":>Query["ID"][#],
    "date":>DateString[Query["Published"][#],"ISODate"],
    "title":>Query["Title"][#],
    "abs":>Query["Summary"][#],
    "author":>StringRiffle[Query["Author",All,"Name"][#],", "],
    "firstAuthor":>Query["Author",1,"Name"][#],
    "firstThreeAuthor":>StringRiffle[Query["Author",1;;3,"Name"][#],", "],
    "journal":>DeleteMissing@Query["JournalReference"][#]
};


fileNameInPath[path_] :=
    FileNames[__~~".pdf"~~EndOfString,path]//
		Map@StringReplace[path~~"/"~~Longest[title__]~~".pdf":>title];


fileNameRegulate//Attributes = {Listable};
fileNameRegulate[string_String] :=
    StringReplace[string,fileNameRegulate`ruleList];
fileNameRegulate[Missing[arg___]] :=
    Missing[arg];
fileNameRegulate`ruleList = {
    "/"->"::",
    "\n"->" ",
    "\[CloseCurlyQuote]"->"'"
};


(* ::Subsection:: *)
(*extractID*)


(*public function, wrapped by Dataset*)

extractID//Options = {
    "tryFileName"->True,
    "hidePath"->True,
    "mergeDuplicateID"->True
};
extractID[][arg_] :=
    extractID`kernel["string"][arg];
extractID["string"][arg_] :=
    extractID`kernel["string"][arg];
extractID[tag:"path"|"file",opts:OptionsPattern[]][arg_] :=
    extractID`kernel[tag,opts][arg]//Dataset;


(*kernel function, called by others*)

extractID`kernel//Options = {
    "tryFileName"->True,
    "hidePath"->True,
    "mergeDuplicateID"->True
};
(*acting on string*)
extractID`kernel["string"][string_String] :=
    extractID`getIDListFromString[string]//Sort;
extractID`kernel["string"][list_List] :=
    extractID`getIDListFromString/@list//Flatten//DeleteDuplicates//Sort;
(*acting on file or path*)
extractID`kernel[tag:"path"|"file",opts:OptionsPattern[]][fileOrPath_String] :=
    Module[ {optsFiltered},
        optsFiltered = FilterRules[{opts},Options[extractID`getIDListFromFileOrPath]];
        extractID`getIDListFromFileOrPath[fileOrPath,tag,optsFiltered]//
		    extractID`gatherAndSortByID[OptionValue["mergeDuplicateID"],#]&
    ];
extractID`kernel[tag:"path"|"file",opts:OptionsPattern[]][list_List] :=
    Module[ {optsFiltered},
        optsFiltered = FilterRules[{opts},Options[extractID`getIDListFromFileOrPath]];
        extractID`getIDListFromFileOrPath[#,tag,optsFiltered]&/@list//Flatten//DeleteDuplicates//
		    extractID`gatherAndSortByID[OptionValue["mergeDuplicateID"],#]&
    ];


(*helper functions*)

extractID`getIDListFromString[string_String] :=
    DeleteDuplicates@StringCases[string,Longest[ID__]/;arXivIDQ[ID]:>ID];
    
extractID`getIDListFromFileOrPath//Options = {
    "tryFileName"->True,
    "hidePath"->True
};
extractID`getIDListFromFileOrPath[file_String,"file","notTryFileName"] :=
    Module[ {IDList,IDNumber},
        IDList = Import[file,{"Plaintext",1}]//StringSplit[#,RegularExpression["\\s"]]&//
        	Map[extractID`getIDListFromString]//Flatten//DeleteDuplicates;
        IDNumber = Length@IDList;
        Which[
            IDNumber===0,
                {<|"ID"->"notFound","file"->{file},"IDLocation"->{"notFoundInFirstPage"}|>},
            IDNumber===1,
                {<|"ID"->First@IDList,"file"->{file},"IDLocation"->{"foundInFirstPage"}|>},
            IDNumber>=2,
                MapThread[
                    <|"ID"->#1,"file"->#2,"IDLocation"->#3|>&,
                    {IDList,ConstantArray[{file},IDNumber],ConstantArray[{"extraInFirstPage"},IDNumber]}
                ]                
        ]
    ];
extractID`getIDListFromFileOrPath[file_String,"file",opts:OptionsPattern[]] :=
    Module[ {IDData,IDNumber,IDList},
        If[ OptionValue["tryFileName"]==False,
            IDData = extractID`getIDListFromFileOrPath[file,"file","notTryFileName"],
            IDList = extractID`getIDListFromString[file];
            IDNumber = Length@IDList;
            IDData = Which[
                IDNumber===0,
                    extractID`getIDListFromFileOrPath[file,"file","notTryFileName"],
                IDNumber===1,
                    {<|"ID"->First@IDList,"file"->{file},"IDLocation"->{"foundInFileName"}|>},
                IDNumber>=2,
                    MapThread[
                        <|"ID"->#1,"file"->#2,"IDLocation"->#3|>&,
                        {IDList,ConstantArray[{file},IDNumber],ConstantArray[{"extraInFileName"},IDNumber]}
                    ]
            ]
        ];
        IDData
    ];
extractID`getIDListFromFileOrPath[path_String,"path",opts:OptionsPattern[]] :=
    Module[ {fileList,IDDataList},
        fileList = FileNames[__~~".pdf"~~EndOfString,path];
        IDDataList = extractID`getIDListFromFileOrPath[#,"file",opts]&/@fileList//Flatten;
        If[ OptionValue["hidePath"]==True,
            IDDataList = IDDataList//Query[All,<|#,"file"->extractID`hidePath[path,#file]|>&]
        ];
        IDDataList
    ];

extractID`hidePath[path_,file_] :=
    StringReplace[file,path~~"/"~~Longest[title__]~~EndOfString:>title];

extractID`gatherAndSortByID[mergeDuplicateID_,list_] :=
    Switch[mergeDuplicateID,
        False,
            list//Query[SortBy[#ID&]],
        True,
            GatherBy[list,#ID&]//Map[Merge[Flatten@*Join]]//
				Query[All,<|#,"ID"->First@#ID|>&]//Query[SortBy[#ID&]]
    ];


(* ::Subsection:: *)
(*searchByID*)


searchByID//Options = {
    "tryFileName"->True,
    "hidePath"->True,
    "mergeDuplicateID"->True,
    "fileNameRegulate"->True
};
searchByID[opts:OptionsPattern[]][arg_] :=
    searchByID`kernel["string",opts][arg]//Dataset;    
searchByID["string",opts:OptionsPattern[]][arg_] :=
    searchByID`kernel["string",opts][arg]//Dataset;    
searchByID[tag:"path"|"file",opts:OptionsPattern[]][arg_] :=
    searchByID`kernel[tag,opts][arg]//Dataset;    


searchByID`kernel//Options = {
    "tryFileName"->True,
    "hidePath"->True,
    "mergeDuplicateID"->True,
    "fileNameRegulate"->True
};
(*acting on string*)
searchByID`kernel["string",opts:OptionsPattern[]][arg_] :=
    Module[ {IDList,optsFiltered},
        optsFiltered = FilterRules[{opts},Options[searchByID`getItemDataFromID]];
        IDList = extractID`kernel["string"][arg];
        searchByID`getItemDataFromID[IDList,optsFiltered]
    ];
(*acting on file or path*)
searchByID`kernel[tag:"path"|"file",opts:OptionsPattern[]][arg_] :=
    Module[ {IDDataList,IDList,optsFiltered},
        optsFiltered[1] = FilterRules[{opts},Options[extractID`kernel]];
        optsFiltered[2] = FilterRules[{opts},Options[searchByID`getItemDataFromID]];
        IDDataList = extractID`kernel[tag,optsFiltered[1]][arg];
        IDList = IDDataList//Query[All,#ID&];
        JoinAcross[
            searchByID`getItemDataFromID[IDList,optsFiltered[2]],
            IDDataList,
            "ID"
        ]
    ];


(*helper functions*)

searchByID`getItemDataFromID//Options = {
    "fileNameRegulate"->True
};
searchByID`getItemDataFromID[IDList_,opts:OptionsPattern[]] :=
    Module[ {itemList,itemNameList,URLList},
        itemList = Normal@arXivConnector["Search",{"ID"->IDList}];
        itemNameList = itemList//Query[All,fileNameFormatter,FailureAction->"Replace"]//
	        searchByID`fileNameRegulate[OptionValue["fileNameRegulate"]];
        URLList = Map[searchByID`getURLFromItem,itemList];
        MapThread[
            Association["ID"->#1,"item"->#2,"URL"->#3]&,
            {IDList,itemNameList,URLList}
        ]
    ];

searchByID`getURLFromItem::usage = 
    "get the Download URL from \"Link\".";
searchByID`getURLFromItem[item_Association]/;MissingQ[item["ID"]] :=
    Missing["Failed"];
searchByID`getURLFromItem[item_Association] :=
    item["Link"]//KeyUnion//
    	Query[Select[#Type=="application/pdf"&],FailureAction->"Replace"]//
		Query[All,"Href",FailureAction->"Replace"]//
		First//StringJoin[#,".pdf"]&;

searchByID`fileNameRegulate[ifRegulate_] :=
    If[ ifRegulate===True,
        fileNameRegulate,
        Identity
    ];


(* ::Subsection:: *)
(*downloadByID*)


downloadByID//Options = {
    "tryFileName"->True,
    "hidePath"->True,
    "mergeDuplicateID"->True,
    "fileNameRegulate"->True
};
downloadByID[targetPath_,opts:OptionsPattern[]][arg_] :=
    downloadByID`kernel[targetPath,"string",opts][arg]//Dataset;
downloadByID[targetPath_String,tag:"string"|"file"|"path",opts:OptionsPattern[]][arg_] :=
    downloadByID`kernel[targetPath,tag,opts][arg]//Dataset;


downloadByID`kernel//Options = {
    "tryFileName"->True,
    "hidePath"->True,
    "mergeDuplicateID"->True,
    "fileNameRegulate"->True
};
downloadByID`kernel[targetPath_String,tag:"string"|"file"|"path",opts:OptionsPattern[]][arg_] :=
    Module[ {IDDataList,optsFiltered},
        optsFiltered = FilterRules[{opts},Options[searchByID`kernel]];
        IDDataList = searchByID`kernel[tag,optsFiltered][arg];
        (*download to the target path and return file objects*)
        IDDataList//Query[All,<|#,"fileObject"->downloadByID`download[targetPath,#URL,#item]|>&]
    ];


(*helper functions*)

downloadByID`download[targetPath_,URL_,Missing["Failed"]] :=
    Missing["Failed"];
downloadByID`download[targetPath_,URL_,item_String] :=
    URLDownload[URL,FileNameJoin@{targetPath,item<>".pdf"}];


(* ::Subsection:: *)
(*extractTitle*)


extractTitle//Options = {
    "hidePath"->True,
    "mergeDuplicateTitle"->True,
    "titleExtractMethod"->"plusYAndFontSize",
    "YResolution"->25
};
extractTitle[tag:"path"|"file",opts:OptionsPattern[]][arg_] :=
    extractTitle`kernel[tag,opts][arg]//Dataset;


extractTitle`kernel//Options = {
    "hidePath"->True,
    "mergeDuplicateTitle"->True,
    "titleExtractMethod"->"plusYAndFontSize",
    "YResolution"->25
};
extractTitle`kernel[tag:"path"|"file",opts:OptionsPattern[]][fileOrPath_String] :=
    Module[ {optsFiltered},
        optsFiltered = FilterRules[{opts},Options[extractTitle`getTitleFromFileOrPath]];
        extractTitle`getTitleFromFileOrPath[fileOrPath,tag,optsFiltered]//
		    extractTitle`gatherAndSortByTitle[OptionValue["mergeDuplicateTitle"],#]&
    ];
extractTitle`kernel[tag:"path"|"file",opts:OptionsPattern[]][list_List] :=
    Module[ {optsFiltered},
        optsFiltered = FilterRules[{opts},Options[extractTitle`getTitleFromFileOrPath]];
        extractTitle`getTitleFromFileOrPath[#,tag,optsFiltered]&/@list//Flatten//DeleteDuplicates//
		    extractTitle`gatherAndSortByTitle[OptionValue["mergeDuplicateTitle"],#]&
    ];


(*helper functions*)

extractTitle`getTitleFromFileOrPath//Options = {
    "hidePath"->True,
    "titleExtractMethod"->"sortYAndFontSize",
    "YResolution"->25
};
(*acting on file*)
extractTitle`getTitleFromFileOrPath[file_String,"file",opts:OptionsPattern[]] :=
    Module[ {title,optsFiltered},
        optsFiltered = FilterRules[{opts},Options[extractTitle`method]];
        title = extractTitle`method[file,OptionValue["titleExtractMethod"],optsFiltered];
        {<|"title"->title,"file"->{file}|>}
    ];
(*acting on path*)
extractTitle`getTitleFromFileOrPath[path_String,"path",opts:OptionsPattern[]] :=
    Module[ {fileList,titleDataList},
        fileList = FileNames[__~~".pdf"~~EndOfString,path];
        titleDataList = extractTitle`getTitleFromFileOrPath[#,"file",opts]&/@fileList//Flatten;
        If[ OptionValue["hidePath"]==True,
            titleDataList = titleDataList//Query[All,<|#,"file"->extractTitle`hidePath[path,#file]|>&]
        ];
        titleDataList
    ];

extractTitle`textRegulate//Options = {
    "YResolution"->25
};
extractTitle`textRegulate[text_Text,opts:OptionsPattern[]] :=
    text/.Text[Style[string_String,color_,styleOptions___Rule],coords_List,offset_List]:>
        KeyMap[ToString]@<|
            "string"->string,
            FilterRules[{styleOptions},{FontSize}],
            "X"->coords[[1]],
            "Y"->Round[coords[[2]],OptionValue["YResolution"]],
            "offset"->offset
        |>;
extractTitle`textRegulate[textList_List,opts:OptionsPattern[]] :=
    Module[ {textData},
        textData = extractTitle`textRegulate[#,opts]&/@textList;
        GatherBy[textData,#Y&]//Map[SortBy[#X&]]//
			Map[mergeByKey[{"string"->StringJoin,"X"->Min},First]]
    ];

extractTitle`method//Options = {
    "YResolution"->25
};
extractTitle`method[file_,"sortYAndFontSize",opts:OptionsPattern[]] :=
    Module[ {textData,counter,resultTextData,searchFirstNTexts},
        textData = extractTitle`textRegulate[Import[file,{"PagePositionedText",1}],opts];
        searchFirstNTexts[data_List,n_] :=
            Intersection[
                data//Query[ReverseSortBy[#Y&]]//Query[1;;n],
                data//Query[ReverseSortBy[#FontSize&]]//Query[1;;n]
            ];
        counter = 1;
        While[
            (resultTextData = searchFirstNTexts[textData,counter])=={},
            counter++
        ];
        resultTextData//Query[MaximalBy[StringLength[#string]&]]//Query[1,#string&]//fileNameRegulate
    ];
extractTitle`method[file_,"plusYAndFontSize",opts:OptionsPattern[]] :=
    Module[ {textData,maxY,maxFontSize,textDataNormalized},
        textData = extractTitle`textRegulate[Import[file,{"PagePositionedText",1}],opts];
        maxY = textData//Query[All,#Y&]//Max;
        maxFontSize = textData//Query[All,#FontSize&]//Max;
        textDataNormalized = textData//Query[All,<|#,"weight"->(#Y/maxY+#FontSize/maxFontSize)|>&];
        textDataNormalized//Query[MaximalBy[#weight&]]//Query[1,"string"]//fileNameRegulate
    ];

extractTitle`hidePath = extractID`hidePath;

extractTitle`gatherAndSortByTitle[mergeDuplicateTitle_,list_] :=
    Switch[mergeDuplicateTitle,
        False,
            list//Query[SortBy[#title&]],
        True,
            GatherBy[list,#title&]//Map[Merge[Join]]//
				Query[All,<|"title"->First@#title,"file"->Flatten@#file|>&]//Query[SortBy[#title&]]
    ];


(* ::Subsection:: *)
(*searchByTitle*)


searchByTitle//Options = {
    "hidePath"->True,
    "mergeDuplicateTitle"->True,
    "titleExtractMethod"->"plusYAndFontSize",
    "YResolution"->25,
    "fileNameRegulate"->True,
    "maxItems"->10
};
searchByTitle[tag:"path"|"file",opts:OptionsPattern[]][arg_] :=
    searchByTitle`kernel[tag,opts][arg]//Dataset;


searchByTitle`kernel//Options = {
    "hidePath"->True,
    "mergeDuplicateTitle"->True,
    "titleExtractMethod"->"plusYAndFontSize",
    "YResolution"->25,
    "fileNameRegulate"->True,
    "maxItems"->10
};
searchByTitle`kernel[tag:"path"|"file",opts:OptionsPattern[]][arg_] :=
    Module[ {titleDataList,titleList,itemDataList,optsFiltered},
        optsFiltered[1] = FilterRules[{opts},Options[extractTitle`kernel]];
        optsFiltered[2] = FilterRules[{opts},Options[searchByTitle`getBestMatchItemFromTitle]];
        titleDataList = extractTitle`kernel[tag,optsFiltered[1]][arg];
        titleList = titleDataList//Query[All,#title&];
        itemDataList = searchByTitle`getBestMatchItemFromTitle[#,optsFiltered[2]]&/@titleList;
        JoinAcross[
            itemDataList,
            titleDataList,
            "title"
        ]
    ];


(*helper functions*)

searchByTitle`getBestMatchItemFromTitle//Options = {
    "fileNameRegulate"->True,
    "maxItems"->10
};
searchByTitle`getBestMatchItemFromTitle[title_,opts:OptionsPattern[]] :=
    Module[ {itemList,itemBestMatch,itemName},
        itemList = Normal@arXivConnector["TitleSearch",{"Query"->title,"MaxItems"->OptionValue["maxItems"]}];
        itemBestMatch = itemList//Query[MinimalBy[EditDistance[title,#Title]&]]//First;
        itemName = itemBestMatch//Query[fileNameFormatter,FailureAction->"Replace"]//
	        searchByTitle`fileNameRegulate[OptionValue["fileNameRegulate"]];
        <|
            "title"->title,
            "item"->itemName,
            "URL"->searchByTitle`getURLFromItem[itemBestMatch],
            "distance"->EditDistance[title,itemBestMatch["Title"]]
        |>
    ];

searchByTitle`fileNameRegulate = searchByID`fileNameRegulate;

searchByTitle`getURLFromItem = searchByID`getURLFromItem; 


(* ::Subsection:: *)
(*downloadByTitle*)


downloadByTitle//Options = {
    "hidePath"->True,
    "mergeDuplicateTitle"->True,
    "titleExtractMethod"->"plusYAndFontSize",
    "YResolution"->25,
    "fileNameRegulate"->True,
    "maxItems"->10
};
downloadByTitle[targetPath_String,tag:"file"|"path",opts:OptionsPattern[]][arg_] :=
    downloadByTitle`kernel[targetPath,tag,opts][arg]//Dataset;


downloadByTitle`kernel//Options = {
    "hidePath"->True,
    "mergeDuplicateTitle"->True,
    "titleExtractMethod"->"plusYAndFontSize",
    "YResolution"->25,
    "fileNameRegulate"->True,
    "maxItems"->10
};
downloadByTitle`kernel[targetPath_String,tag:"file"|"path",opts:OptionsPattern[]][arg_] :=
    Module[ {titleDataList,optsFiltered},
        optsFiltered = FilterRules[{opts},Options[searchByTitle`kernel]];
        titleDataList = searchByTitle`kernel[tag,optsFiltered][arg];
        (*download to the target path and return file objects*)
        titleDataList//Query[All,<|#,"fileObject"->downloadByTitle`download[targetPath,#URL,#item]|>&]
    ];


(*helper functions*)
downloadByTitle`download = downloadByID`download;


(* ::Section:: *)
(*Default setting of file names*)


fileNameFormat["ID"<>" "<>"title"<>", "<>"firstAuthor"];


(* ::Section:: *)
(*End*)


End[];

Protect@@Names[$Context<>"*"];

EndPackage[];
