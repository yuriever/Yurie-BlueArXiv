(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["lily`arxiv`"];


Unprotect@@Names[$Context<>"*"];
ClearAll@@Names[$Context<>"*"]


(* ::Section:: *)
(*Usage*)


arXivIDQ::usage =
    "check whether a string is a valid arXiv ID.";


fileNameFormat::usage = 
    "set the format of file names.";


extractID::usage =
    "extract arXiv IDs from string, file name or path.";
searchByID::usage = 
    "search by IDs extracted from string, file or path, "<>
    "and return the found items on arXiv with formatted names by fileNameFormatter.";
downloadByID::usage = 
    "download by IDs extracted from string, file or path to the target path, "<>
    "and return the file objects with formatted names by fileNameFormatter."
generateBibTeXByID::usage = 
    "export the found BibTeX entries on inspirehep by IDs extracted from string, file or path, "<>
    "and return the BibTeX keys.";


arXivInterface::usage = 
    "show the interface.";


(* ::Section:: *)
(*Context alias*)


(*move the helper functions under lily`arxiv`, but can slow down the package loading.*)

(*$ContextAliases["fileNameFormat`"] = "lily`arxiv`fileNameFormat`";
$ContextAliases["addButtonTo`"] = "lily`arxiv`addButtonTo`";
$ContextAliases["extractID`"] = "lily`arxiv`extractID`";
$ContextAliases["searchByID`"] = "lily`arxiv`searchByID`";
$ContextAliases["downloadByID`"] = "lily`arxiv`downloadByID`";
$ContextAliases["generateBibTeXByID`"] = "lily`arxiv`generateBibTeXByID`";
$ContextAliases["arXivInterface`"] = "lily`arxiv`arXivInterface`";*)


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*arXivIDQ*)


arXivIDQ[string_String] :=
    StringMatchQ[
        string,
        RegularExpression["(\\d{4}\\.\\d{4,5})|((astro-ph|cond-mat|gr-qc|hep-ex|hep-lat|hep-ph|hep-th|math-ph|nlin|nucl-ex|nucl-th|physics|quant-ph|math|cs)/\\d{7})"]
    ];
arXivIDQ[_] = False;


(* ::Subsection:: *)
(*fileName**)


fileNameFormatter::usage = 
    "formattor of file names, set by fileNameFormat.";
fileNameFormat//Attributes = 
    {HoldAll};
fileNameFormat[format_] :=
    (
        fileNameFormatter = Hold[format]/.fileNameFormat`keywordToFunction/.{Hold[expr_]:>Hold[(expr)&]}//ReleaseHold;
    );


fileNameFormat`keywordToFunction = {
    "ID":>Query["ID"][#],
    "date":>DateString[Query["Published"][#],"ISODate"],
    "title":>Query["Title"][#],
    "abs":>Query["Summary"][#],
    "author":>RemoveDiacritics@StringRiffle[Query["Author",All,"Name"][#],", "],
    "firstAuthor":>RemoveDiacritics@Query["Author",1,"Name"][#],
    "firstThreeAuthor":>RemoveDiacritics@StringRiffle[Query["Author",1;;3,"Name"][#],", "],
    "journal":>DeleteMissing@Query["JournalReference"][#]
};


fileNameInPath::usage =
    "return a list of PDF file names in the path.";
fileNameInPath[path_] :=
    FileNames[__~~".pdf"~~EndOfString,path]//
		Map@StringReplace[path~~"/"~~Longest[title__]~~".pdf":>title];


fileNameRegulate::usage =
    "regulate the file name with characters like \"/\" and \"\n\".";
fileNameRegulate//Attributes = 
    {Listable};
fileNameRegulate[string_String] :=
    StringReplace[
        string,
        {
            "/"->"::",
            "\n"->" ",
            "\[CloseCurlyQuote]"->"'"
        }
    ];
fileNameRegulate[arg_Missing] :=
    arg;


(* ::Subsection:: *)
(*addButtonTo*)


ifAddButtonTo[True,keys__][list_] :=
    addButtonTo[keys][list];
ifAddButtonTo[False,keys__][list_] :=
    list;

addButtonTo[key_String][list_] :=
    With[ {$$key = key},
        list//Query[All,<|#,$$key->addButtonTo`copyToClipboard[Slot[$$key]]|>&]
    ];
addButtonTo["URL"][list_] :=
    list//Query[All,<|#,"URL"->addButtonTo`hyperlink[#URL]|>&];
addButtonTo[key_,restKeys__][list_] :=
    list//addButtonTo[key]//addButtonTo[restKeys];


addButtonTo`hyperlink[value_String] :=
    Hyperlink[value,value,FrameMargins->Small];
addButtonTo`hyperlink[_] :=
    Missing["Failed"];

addButtonTo`copyToClipboard[value_String] :=
    Interpretation[{},
        Button[value,CopyToClipboard@value,Appearance->"Frameless",FrameMargins->Small],
        value
    ];
addButtonTo`copyToClipboard[_] :=
    Missing["Failed"];


(* ::Subsection:: *)
(*extractID*)


extractID//Options = {
    "tryFileName"->True,
    "hidePath"->True,
    "mergeDuplicateID"->True,
    "clickToCopy"->True
};
extractID::pdffailimport = 
    "the PDF file fails to import: \n``";
extractID[tag:PatternSequence[]|"string"][arg_] :=
    extractID`kernel["string"][arg];
extractID[tag:"path"|"file",opts:OptionsPattern[]][arg_] :=
    Module[ {fopts},
        fopts = FilterRules[{opts},Options[extractID`kernel]];
        extractID`kernel[tag,fopts][arg]//ifAddButtonTo[OptionValue["clickToCopy"],"ID"]//Dataset
    ];


extractID`kernel//Options = {
    "tryFileName"->True,
    "hidePath"->True,
    "mergeDuplicateID"->True
};
(*act on string*)
extractID`kernel["string"][string_String] :=
    extractID`getIDListFromString[string]//Sort;
extractID`kernel["string"][list_List] :=
    extractID`getIDListFromString/@list//Flatten//DeleteDuplicates//Sort;
(*act on file or path*)
extractID`kernel[tag:"path"|"file",opts:OptionsPattern[]][fileOrPath_String] :=
    Module[ {fopts},
        fopts = FilterRules[{opts},Options[extractID`getIDListFromFileOrPath]];
        extractID`getIDListFromFileOrPath[fileOrPath,tag,fopts]//
		    extractID`gatherAndSortByID[OptionValue["mergeDuplicateID"],#]&
    ];
extractID`kernel[tag:"path"|"file",opts:OptionsPattern[]][list_List] :=
    Module[ {fopts},
        fopts = FilterRules[{opts},Options[extractID`getIDListFromFileOrPath]];
        extractID`getIDListFromFileOrPath[#,tag,fopts]&/@list//Flatten//DeleteDuplicates//
		    extractID`gatherAndSortByID[OptionValue["mergeDuplicateID"],#]&
    ];


extractID`getIDListFromString[string_String] :=
    DeleteDuplicates@StringCases[string,Longest[id__]/;arXivIDQ[id]:>id];

extractID`getIDListFromFirstPageOfFile[file_String] :=
    Module[ {idList,idNumber},
        idList = file//extractID`importFirstPage//StringSplit[#,RegularExpression["\\s"]]&//
        	Map[extractID`getIDListFromString]//Flatten//DeleteDuplicates;
        idNumber = Length@idList;
        Which[
            idNumber===0,
                {<|"ID"->"notFound","file"->{file},"IDLocation"->{"notFoundInFirstPage"}|>},
            idNumber===1,
                {<|"ID"->First@idList,"file"->{file},"IDLocation"->{"foundInFirstPage"}|>},
            idNumber>=2,
                MapThread[
                    <|"ID"->#1,"file"->#2,"IDLocation"->#3|>&,
                    {idList,ConstantArray[{file},idNumber],ConstantArray[{"extraInFirstPage"},idNumber]}
                ]                
        ]
    ];

extractID`getIDListFromFileOrPath//Options = {
    "tryFileName"->True,
    "hidePath"->True
};
extractID`getIDListFromFileOrPath[file_String,"file",opts:OptionsPattern[]] :=
    Module[ {idData,idNumber,idList},
        If[ OptionValue["tryFileName"]===False,
            idData = extractID`getIDListFromFirstPageOfFile[file],
            idList = extractID`getIDListFromString[file];
            idNumber = Length@idList;
            idData = Which[
                idNumber===0,
                    extractID`getIDListFromFirstPageOfFile[file],
                idNumber===1,
                    {<|"ID"->First@idList,"file"->{file},"IDLocation"->{"foundInFileName"}|>},
                idNumber>=2,
                    MapThread[
                        <|"ID"->#1,"file"->#2,"IDLocation"->#3|>&,
                        {idList,ConstantArray[{file},idNumber],ConstantArray[{"extraInFileName"},idNumber]}
                    ]
            ]
        ];
        idData
    ];
extractID`getIDListFromFileOrPath[path_String,"path",opts:OptionsPattern[]] :=
    Module[ {fileList,idDataList},
        fileList = FileNames[__~~".pdf"~~EndOfString,path];
        idDataList = extractID`getIDListFromFileOrPath[#,"file",opts]&/@fileList//Flatten;
        If[ OptionValue["hidePath"]===True,
            idDataList = idDataList//Query[All,<|#,"file"->extractID`hidePath[path,#file]|>&]
        ];
        idDataList
    ];

extractID`hidePath[path_,file_] :=
    StringReplace[file,path~~"/"~~Longest[title__]~~EndOfString:>title];

extractID`gatherAndSortByID[mergeDuplicateID_,list_] :=
    Switch[mergeDuplicateID,
        False,
            list,
        True,
            GatherBy[list,#ID&]//Map[Merge[Flatten@*Join]]//Query[All,<|#,"ID"->First@#ID|>&]
    ]//Query[SortBy[#ID&]];

extractID`importFirstPage[file_] :=
    Quiet[
        Check[
            Import[file,{"Plaintext",1}],
            Message[extractID::pdffailimport,file];
            ""
        ],
        All,
        {extractID::pdffailimport}
    ];


(* ::Subsection:: *)
(*searchByID*)


searchByID//Options = {
    "tryFileName"->True,
    "hidePath"->True,
    "mergeDuplicateID"->True,
    "fileNameRegulate"->True,
    "clickToCopy"->True
};
searchByID::connectionfailed =
    "connection failed.";
searchByID[opts:OptionsPattern[]][arg_] :=
    searchByID["string",opts][arg];
searchByID[tag:"string"|"file"|"path",opts:OptionsPattern[]][arg_] :=
    Module[ {fopts},
        fopts = FilterRules[{opts},Options[searchByID`kernel]];
        searchByID`kernel[tag,fopts][arg]//ifAddButtonTo[OptionValue["clickToCopy"],"ID","item","URL"]//Dataset
    ];


searchByID`kernel//Options = {
    "tryFileName"->True,
    "hidePath"->True,
    "mergeDuplicateID"->True,
    "fileNameRegulate"->True
};
searchByID`kernel["string",opts:OptionsPattern[]][arg_] :=
    Module[ {idList,fopts},
        fopts = FilterRules[{opts},Options[searchByID`getItemDataFromID]];
        idList = extractID`kernel["string"][arg];
        searchByID`getItemDataFromID[idList,fopts]
    ];
searchByID`kernel[tag:"path"|"file",opts:OptionsPattern[]][arg_] :=
    Module[ {idDataList,idList,fopts},
        fopts[1] = FilterRules[{opts},Options[extractID`kernel]];
        fopts[2] = FilterRules[{opts},Options[searchByID`getItemDataFromID]];
        idDataList = extractID`kernel[tag,fopts[1]][arg];
        idList = idDataList//Query[All,#ID&];
        JoinAcross[
            searchByID`getItemDataFromID[idList,fopts[2]],
            idDataList,
            "ID"
        ]
    ];


searchByID`getItemDataFromID//Options = {
    "fileNameRegulate"->True
};
searchByID`getItemDataFromID[idList_,opts:OptionsPattern[]] :=
    Module[ {itemList,idValidList,itemNameList,urlList,notFoundNumber,itemData},
        idValidList = DeleteDuplicates@DeleteCases[idList,"notFound"];
        notFoundNumber = Count[idList,"notFound"];
        (*itemList = 
            Quiet@Normal@ServiceExecute["ArXiv","Search",{"ID"->idValidList,MaxItems->Length@idValidList}];*)
        itemList = 
            (*if no ID found, return empty list.*)
            If[ idValidList==={},
                {},
                (*if the connection fails, return list of empty associations.*)
                Enclose[
                    ConfirmMatch[
                        Quiet@Normal@ServiceExecute["ArXiv","Search",{"ID"->idValidList,MaxItems->Length@idValidList}],
                        _List,
                        Message[searchByID::connectionfailed]
                    ],
                    Table[<||>,Length@idValidList]&
                ]
            ];
        itemNameList = itemList//Query[All,fileNameFormatter,FailureAction->"Replace"]//
	        searchByID`fileNameRegulate[OptionValue["fileNameRegulate"]];
        urlList = Map[searchByID`getURLFromItem,itemList];
        itemData = MapThread[
            <|"ID"->#1,"item"->#2,"URL"->#3|>&,
            {idValidList,itemNameList,urlList}
        ];
        If[ notFoundNumber===0,
            itemData,
            Join[
                itemData,
                {<|"ID"->"notFound","item"->Missing["Failed"],"URL"->Missing["Failed"]|>}
            ]
        ]
    ];

searchByID`getURLFromItem::usage = 
    "get the Download URL from \"Link\".";
searchByID`getURLFromItem[item_Association]/;MissingQ[item["ID"]] :=
    Missing["Failed"];
searchByID`getURLFromItem[item_Association] :=
    item["Link"]//KeyUnion//
    	Query[Select[#Type==="application/pdf"&],FailureAction->"Replace"]//
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
    "fileNameRegulate"->True,
    "clickToCopy"->True
};
downloadByID[targetPath_String,opts:OptionsPattern[]][arg_] :=
    downloadByID[targetPath,"string",opts][arg];
downloadByID[targetPath_String,tag:"string"|"file"|"path",opts:OptionsPattern[]][arg_] :=
    Module[ {fopts},
        fopts = FilterRules[{opts},Options[downloadByID`kernel]];
        downloadByID`kernel[targetPath,tag,fopts][arg]//ifAddButtonTo[OptionValue["clickToCopy"],"ID","item","URL"]//Dataset
    ];


downloadByID`kernel//Options = {
    "tryFileName"->True,
    "hidePath"->True,
    "mergeDuplicateID"->True,
    "fileNameRegulate"->True
};
downloadByID`kernel[targetPath_String,tag:"string"|"file"|"path",opts:OptionsPattern[]][arg_] :=
    Module[ {idDataList,fopts},
        fopts = FilterRules[{opts},Options[searchByID`kernel]];
        idDataList = searchByID`kernel[tag,fopts][arg];
        (*download to the target path and return file objects*)
        idDataList//Query[All,<|#,"fileObject"->downloadByID`download[targetPath,#URL,#item]|>&]
    ];


downloadByID`download[targetPath_,url_,Missing["Failed"]] :=
    Missing["Failed"];
downloadByID`download[targetPath_,url_,item_String] :=
    URLDownload[url,FileNameJoin@{targetPath,item<>".pdf"}];


(* ::Subsection:: *)
(*generateBibTeXByID*)


generateBibTeXByID//Options = {
    "tryFileName"->True,
    "hidePath"->True,
    "mergeDuplicateID"->True,
    "clickToCopy"->True
};
generateBibTeXByID[targetPath_String,bibName_String,opts:OptionsPattern[]][arg_] :=
    generateBibTeXByID[targetPath,bibName,"string",opts][arg];
generateBibTeXByID[targetPath_String,bibName_String,tag:"string"|"path"|"file",opts:OptionsPattern[]][arg_] :=
    Module[ {fopts},
        fopts = FilterRules[{opts},Options[generateBibTeXByID`kernel]];
        generateBibTeXByID`kernel[targetPath,bibName,tag,fopts][arg]//ifAddButtonTo[OptionValue["clickToCopy"],"key","ID","BibTeX"]//
	    	Dataset[#,HiddenItems->{"BibTeX"->True}]&
    ];


generateBibTeXByID`kernel//Options = {
    "tryFileName"->True,
    "hidePath"->True,
    "mergeDuplicateID"->True
};
generateBibTeXByID`kernel[targetPath_String,bibName_String,"string",opts:OptionsPattern[]][arg_] :=
    Module[ {idList,fopts,itemList},
        fopts = FilterRules[{opts},Options[generateBibTeXByID`getBibTeXItemFromID]];
        idList = extractID`kernel["string"][arg];
        itemList = generateBibTeXByID`getBibTeXItemFromID[idList];
        generateBibTeXByID`exportBibTeX[targetPath,bibName,itemList];
        itemList
    ];
generateBibTeXByID`kernel[targetPath_String,bibName_String,tag:"path"|"file",opts:OptionsPattern[]][arg_] :=
    Module[ {idDataList,idList,fopts,itemList},
        fopts = FilterRules[{opts},Options[extractID`kernel]];
        idDataList = extractID`kernel[tag,fopts][arg];
        idList = idDataList//Query[All,#ID&];
        itemList = JoinAcross[
            generateBibTeXByID`getBibTeXItemFromID[idList],
            idDataList,
            "ID"
        ];
        generateBibTeXByID`exportBibTeX[targetPath,bibName,itemList];
        itemList
    ];


generateBibTeXByID`getBibTeXItemFromID[idList_] :=
    Map[
        <|"ID"->#,"BibTeX"->URLExecute@HTTPRequest["https://inspirehep.net/api/arxiv/"<>#<>"?format=bibtex"]|>&,
        idList
    ]//Query[All,<|"key"->generateBibTeXByID`extractBibTeXKey[#BibTeX],#|>&];
    
generateBibTeXByID`extractBibTeXKey[bibtex_String] :=
    First@StringCases[bibtex,StartOfString~~Shortest[__]~~"{"~~Shortest[key__]~~",\n"~~__:>key];    
generateBibTeXByID`extractBibTeXKey[_] :=
    Missing["Failed"];

generateBibTeXByID`exportBibTeX[targetPath_String,bibName_String,itemList_] :=
    Export[
        FileNameJoin@{targetPath,bibName},
        itemList//Query[Select[Head[#BibTeX]===String&],#BibTeX&]//Riffle[#,""]&,
        "List"
    ];


(* ::Subsection:: *)
(*arXivInterface*)


arXivInterface[] :=
    CellPrint@ExpressionCell[
        Interpretation[
            {    
                fun = "download",
                tag = "string",
                string = "",
                target = FileNameJoin@{$HomeDirectory,"Downloads"}
            },
            Panel@Column@{
                "Function:",
                Row@{
                    PopupMenu[Dynamic[fun],{"extract","search","download","generate BibTeX"},Appearance->"DialogBox",ImageSize->Small],
                    " from ",
                    PopupMenu[Dynamic[tag],{"string",Delimiter,"file","path"},Appearance->"DialogBox",ImageSize->Small]
                },
                "",
                Sequence@@arXivInterface`targetUnit,
                "",
                Sequence@@arXivInterface`inputUnit
            },
            Switch[fun,
                "extract",
                    extractID[tag][string],
                "search",
                    searchByID[tag][string],
                "download",
                    downloadByID[target,tag][string],
                "generate BibTeX",
                    generateBibTeXByID[target,"refs-"<>ToString@RandomInteger[{100,1000}]<>".bib",tag][string]
            ]
        ],
        "Input"
    ];


arXivInterface`targetUnit =
    Hold["Downloads location:",InputField[Dynamic[target],String,FieldHint->"Enter the downloads location.",FieldSize->{First@CurrentValue[WindowSize]/20,1}]];
arXivInterface`inputUnit =
    Hold["Input string/file/path:",InputField[Dynamic[string],String,FieldHint->"Enter a string/file/path containing arXiv IDs.",FieldSize->{First@CurrentValue[WindowSize]/20,Last@CurrentValue[WindowSize]/80}]];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


Protect@@Names[$Context<>"*"];


(*default setting of file names*)
fileNameFormat["ID"<>" "<>"title"<>", "<>"firstAuthor"];


EndPackage[];
