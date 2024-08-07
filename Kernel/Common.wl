(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`BlueArXiv`Common`"];


(* ::Section:: *)
(*Public*)


$arXivIDPattern::usage =
    "string pattern of valid arXiv ID.";

$citeKeyPattern::usage =
    "string pattern of cite key.";

$tagPattern::usage =
    "pattern of supported tags.";

$imagePattern::usage =
    "pattern of images.";

$pathPattern::usage =
    "pattern of paths.";


throwWrongTypeInput::usage =
    "check the input type according to the tag.";

regulateFileName::usage =
    "regulate special characters in file name.";

getFilePathByExtension::usage =
    "get file paths from path or list of paths by specifying the extension.";

getFileNameByExtension::usage =
    "get file names from path or list of paths by specifying the extension.";

ifAddButton::usage =
    "whether to add click-to-copy/hyperlink button to list of associations.";

tryImport::usage =
    "try to import the file, otherwise return the specified value and message.";

mergeDataByKey::usage =
    "merge a list of associations using different merge functions according to keys.";


(* ::Section:: *)
(*Private*)


(* ::Subsection::Closed:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*Constant*)


$arXivIDPattern =
    RegularExpression["(\\d{4}\\.\\d{4,5})|((astro-ph|cond-mat|gr-qc|hep-ex|hep-lat|hep-ph|hep-th|math-ph|nlin|nucl-ex|nucl-th|physics|quant-ph|math|cs)/\\d{7})"];


$citeKeyPattern =
    (*no whitespace tolerance.*)
    RegularExpression["(\\\\cite{)(\\S*?)(})"];


$tagPattern =
    "string"|"image"|"path";


$imagePattern =
    _Image|Null;


$pathPattern =
    _String|_File;


(* ::Subsection:: *)
(*Main*)


throwWrongTypeInput[tag_][input_] :=
    Which[
        tag==="string"&&MatchQ[input,_String],
            input,
        tag==="image"&&MatchQ[input,$imagePattern],
            input,
        tag==="path"&&MatchQ[input,$pathPattern],
            input,
        True,
            Message[General::invencin,input];
            input//Throw
    ];


(* ::Subsubsection::Closed:: *)
(*regulateFileName*)


regulateFileName[string_String] :=
    RemoveDiacritics@StringReplace[
        string,
        {
            ":"->" -",
            "/"->"_",
            "\n"|"\r"->" ",
            "\[Dash]"->"-",
            "\[CloseCurlyQuote]"->"'"
        }
    ];


(* ::Subsubsection::Closed:: *)
(*getFilePathByExtension*)


getFilePathByExtension[extension_][path:_String|_File] :=
    Which[
        DirectoryQ[path],
            FileNames[__~~"."~~extension~~EndOfString,path],
        FileExistsQ[path]&&FileExtension[path]===extension,
            {path},
        True,
            {}
    ];

getFilePathByExtension[extension_][pathList_List] :=
    pathList//Map[getFilePathByExtension[extension]]//Flatten;


(* ::Subsubsection::Closed:: *)
(*getFileNameByExtension*)


getFileNameByExtension[extension_][pathOrPathList:_String|_File|_List] :=
    pathOrPathList//getFilePathByExtension[extension]//Map[FileNameTake];


(* ::Subsubsection::Closed:: *)
(*ifAddButton*)


ifAddButton[True,keys__][data_List] :=
    addButton[keys][data];

ifAddButton[False,___][data_List] :=
    data;


addButton[key_String][data_] :=
    data//Query[All,<|#,key->addCopyButtonToString[Slot[key]]|>&]

addButton["URL"][data_] :=
    data//Query[All,<|#,"URL"->addHyperlinkToURL[#URL]|>&];

addButton[key_String,restKeys__String][data_] :=
    data//addButton[key]//addButton[restKeys];


addHyperlinkToURL[value_String] :=
    Hyperlink[value,value,FrameMargins->Small];

addHyperlinkToURL[Missing[any_]] :=
    Missing[any];


addCopyButtonToString[value_String] :=
    Interpretation[{},
        Button[value,CopyToClipboard@value,Appearance->"Frameless",FrameMargins->Small],
        value
    ];

addCopyButtonToString[Missing[any_]] :=
    Missing[any];


(* ::Subsubsection::Closed:: *)
(*tryImport*)


tryImport[return_,args___][filePath_String] :=
    Check[
        Import[filePath,args],
        (*fail*)
        return
    ];


(* ::Subsubsection::Closed:: *)
(*mergeDataByID*)


mergeDataByKey[ruleList:{___Rule},default:_:Identity][assocList:{___Association}] :=
    mergeDataByIDKernel[assocList,ruleList,default];

mergeDataByKey[assocList:{___Association},ruleList:{___Rule},default:_:Identity] :=
    mergeDataByIDKernel[assocList,ruleList,default];


mergeDataByIDKernel[{<||>...},_,_] :=
    <||>;

mergeDataByIDKernel[assocList_,{},Identity] :=
    (*in this case queryRuleList=={}, and Query[{}][...] will unexpectedly return an empty association.*)
    getTransposedAssocListAndKeyList[assocList,{}]//First;

mergeDataByIDKernel[assocList_,ruleList_,default_] :=
    Module[ {keyList,dataMerged,queryRuleList},
        {dataMerged,keyList} =
            getTransposedAssocListAndKeyList[assocList,ruleList];
        queryRuleList =
            prepareQueryRuleList[ruleList,keyList,default];
        Query[queryRuleList]@dataMerged
    ];


getTransposedAssocListAndKeyList[assocList_,ruleList_] :=
    Module[ {keyList,keyListList,dataPadded,dataMerged,missing},
        keyListList =
            Keys[assocList];
        (*pad the list of associations by the placeholder missing if necessary.*)
        If[ SameQ@@keyListList,
            keyList =
                First@keyListList;
            dataMerged =
                AssociationThread[
                    keyList,
                    Transpose@Values[assocList]
                ],
            (*Else*)
            dataPadded =
                KeyUnion[assocList,missing&];
            keyList =
                Keys@First@dataPadded;
            dataMerged =
                AssociationThread[
                    keyList,
                    DeleteCases[Transpose@Values[dataPadded],missing,{2}]
                ];
        ];
        {dataMerged,Key/@keyList}
    ];


(*prepare the rules for query and delete the unnecessary Identity query.*)

prepareQueryRuleList[ruleList_,keyList_,default_] :=
    DeleteCases[
        Thread[
            keyList->Lookup[ruleList,keyList,default]
        ],
        _->Identity
    ];


(* ::Subsection::Closed:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
