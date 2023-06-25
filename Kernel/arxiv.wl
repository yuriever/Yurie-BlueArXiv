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
fileNameInPath::usage =
    "return a list of PDF file names in the path.";
fileNameRegulate::usage =
    "regulate the file name with characters like \"/\" and \"\n\".";


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
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*lily`base`*)


echo//Attributes = {HoldAll};
echo[code_] :=
    Module[ {codeResult},
        codeResult = code;
        Print[
            ToString@Unevaluated@code,
            " = ",
            codeResult
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
(*arXivIDQ*)


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
extractID::pdffailimport = 
    "The PDF file fails to import: \n``";
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
    DeleteDuplicates@StringCases[string,Longest[id__]/;arXivIDQ[id]:>id];
    
extractID`getIDListFromFileOrPath//Options = {
    "tryFileName"->True,
    "hidePath"->True
};
extractID`getIDListFromFileOrPath[file_String,"file","notTryFileName"] :=
    Module[ {idList,idNumber},
        idList = file//extractID`import//StringSplit[#,RegularExpression["\\s"]]&//
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
extractID`getIDListFromFileOrPath[file_String,"file",opts:OptionsPattern[]] :=
    Module[ {idData,idNumber,idList},
        If[ OptionValue["tryFileName"]===False,
            idData = extractID`getIDListFromFileOrPath[file,"file","notTryFileName"],
            idList = extractID`getIDListFromString[file];
            idNumber = Length@idList;
            idData = Which[
                idNumber===0,
                    extractID`getIDListFromFileOrPath[file,"file","notTryFileName"],
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
            list//Query[SortBy[#ID&]],
        True,
            GatherBy[list,#ID&]//Map[Merge[Flatten@*Join]]//
				Query[All,<|#,"ID"->First@#ID|>&]//Query[SortBy[#ID&]]
    ];

extractID`import[file_] :=
    Quiet[
        Check[
            Import[file,{"Plaintext",1}],
            Message[extractID::pdffailimport,file];
            "",
            Import::fmterr
        ],
        {Import::fmterr}
    ];
    

(* ::Subsection:: *)
(*searchByID*)


searchByID//Options = {
    "tryFileName"->True,
    "hidePath"->True,
    "mergeDuplicateID"->True,
    "fileNameRegulate"->True
};
searchByID::connectionfailed =
    "connection failed.";
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
    Module[ {idList,optsFiltered},
        optsFiltered = FilterRules[{opts},Options[searchByID`getItemDataFromID]];
        idList = extractID`kernel["string"][arg];
        searchByID`getItemDataFromID[idList,optsFiltered]
    ];
(*acting on file or path*)
searchByID`kernel[tag:"path"|"file",opts:OptionsPattern[]][arg_] :=
    Module[ {idDataList,idList,optsFiltered},
        optsFiltered[1] = FilterRules[{opts},Options[extractID`kernel]];
        optsFiltered[2] = FilterRules[{opts},Options[searchByID`getItemDataFromID]];
        idDataList = extractID`kernel[tag,optsFiltered[1]][arg];
        idList = idDataList//Query[All,#ID&];
        JoinAcross[
            searchByID`getItemDataFromID[idList,optsFiltered[2]],
            idDataList,
            "ID"
        ]
    ];


(*helper functions*)


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
    "fileNameRegulate"->True
};
downloadByID[targetPath_String,opts:OptionsPattern[]][arg_] :=
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
    Module[ {idDataList,optsFiltered},
        optsFiltered = FilterRules[{opts},Options[searchByID`kernel]];
        idDataList = searchByID`kernel[tag,optsFiltered][arg];
        (*download to the target path and return file objects*)
        idDataList//Query[All,<|#,"fileObject"->downloadByID`download[targetPath,#URL,#item]|>&]
    ];
     

(*helper functions*)

downloadByID`download[targetPath_,url_,Missing["Failed"]] :=
    Missing["Failed"];
downloadByID`download[targetPath_,url_,item_String] :=
    URLDownload[url,FileNameJoin@{targetPath,item<>".pdf"}];


(* ::Subsection:: *)
(*generateBibTeXByID*)


generateBibTeXByID//Options = {
    "tryFileName"->True,
    "hidePath"->True,
    "mergeDuplicateID"->True
};
generateBibTeXByID[targetPath_String,bibName_String,opts:OptionsPattern[]][arg_] :=
    generateBibTeXByID`kernel[targetPath,bibName,"string",opts][arg]//Dataset[#,HiddenItems->{"BibTeX"->True}]&;    
generateBibTeXByID[targetPath_String,bibName_String,"string",opts:OptionsPattern[]][arg_] :=
    generateBibTeXByID`kernel[targetPath,bibName,"string",opts][arg]//Dataset[#,HiddenItems->{"BibTeX"->True}]&;    
generateBibTeXByID[targetPath_String,bibName_String,tag:"path"|"file",opts:OptionsPattern[]][arg_] :=
    generateBibTeXByID`kernel[targetPath,bibName,tag,opts][arg]//Dataset[#,HiddenItems->{"BibTeX"->True}]&;    


generateBibTeXByID`kernel//Options = {
    "tryFileName"->True,
    "hidePath"->True,
    "mergeDuplicateID"->True
};
(*acting on string*)
generateBibTeXByID`kernel[targetPath_String,bibName_String,"string",opts:OptionsPattern[]][arg_] :=
    Module[ {idList,optsFiltered,itemList},
        optsFiltered = FilterRules[{opts},Options[generateBibTeXByID`getBibTeXItemFromID]];
        idList = extractID`kernel["string"][arg];
        itemList = generateBibTeXByID`getBibTeXItemFromID[idList];
        generateBibTeXByID`exportBibTeX[targetPath,bibName,itemList];
        itemList
    ];
(*acting on file or path*)
generateBibTeXByID`kernel[targetPath_String,bibName_String,tag:"path"|"file",opts:OptionsPattern[]][arg_] :=
    Module[ {idDataList,idList,optsFiltered,itemList},
        optsFiltered = FilterRules[{opts},Options[extractID`kernel]];
        idDataList = extractID`kernel[tag,optsFiltered][arg];
        idList = idDataList//Query[All,#ID&];
        itemList = JoinAcross[
            generateBibTeXByID`getBibTeXItemFromID[idList],
            idDataList,
            "ID"
        ];
        generateBibTeXByID`exportBibTeX[targetPath,bibName,itemList];
        itemList
    ];


(*helper functions*)

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


(*Default setting of file names*)
fileNameFormat["ID"<>" "<>"title"<>", "<>"firstAuthor"];


EndPackage[];
