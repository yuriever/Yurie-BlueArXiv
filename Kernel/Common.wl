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


(* ::Subsection:: *)
(*Main*)


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


getFilePathByExtension[extension_][path_] :=
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


getFileNameByExtension[extension_][pathOrPathList_] :=
    pathOrPathList//getFilePathByExtension[extension]//Map[FileNameTake];


(* ::Subsubsection::Closed:: *)
(*ifAddButton*)


ifAddButton[True,keys__][list:{___Association}] :=
    addButton[keys][list];

ifAddButton[False,___][list_] :=
    list;


addButton[key_String][list_] :=
    list//Query[All,<|#,key->addCopyButtonToString[Slot[key]]|>&]

addButton["URL"][list_] :=
    list//Query[All,<|#,"URL"->addHyperlinkToURL[#URL]|>&];

addButton[key_,restKeys__][list_] :=
    list//addButton[key]//addButton[restKeys];


addHyperlinkToURL[value_String] :=
    Hyperlink[value,value,FrameMargins->Small];

addHyperlinkToURL[_Missing] :=
    Missing["Failed"];


addCopyButtonToString[value_String] :=
    Interpretation[{},
        Button[value,CopyToClipboard@value,Appearance->"Frameless",FrameMargins->Small],
        value
    ];

addCopyButtonToString[_Missing] :=
    Missing["Failed"];


(* ::Subsubsection::Closed:: *)
(*tryImport*)


tryImport[return_,args___][file_] :=
    Check[
        Import[file,args],
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
