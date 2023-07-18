(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["lily`paper`common`"];


(* ::Section:: *)
(*Usage*)


$arXivPDFNameFormat::usage = 
    "formattor of file names, set by arXivPDFNameFormat.";


fileNameRegulate::usage =
    "regulate the file name with characters like \"/\" and \"\n\".";

getFileByExtension::usage = 
    "get files in path or list of paths by specifying the extension.";
getFileNameByExtension::usage =
    "get file names in path or list of paths by specifying the extension.";

ifAddButtonTo::usage = 
    "whether to add click-to-copy button to list of associations.";
addButtonTo::usage = 
    "add click-to-copy button to list of associations.";

mergeByKey::usage =
    "ResourceFunction[\"MergeByKey\"]: merge a list of associations using different merge functions according to keys. The default merging function is Identity.\n"<>
    "mergeByKey[{assoc1,assoc2,...},{key1->f1,key2->f2,...},f]\n"<>
    "mergeByKey[{assoc1,assoc2,...},{...,{keyi1,keyi2,...}->fi,...},...]";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*fileNameRegulate*)


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
(*getFileByExtension|getFileNameByExtension*)


getFileByExtension[extension_][path_] :=
    lily`paper`common`getFileByExtension`kernel[extension][path];
getFileByExtension[extension_][pathList_List] :=
    lily`paper`common`getFileByExtension`kernel[extension]/@pathList//Flatten//DeleteDuplicates;


lily`paper`common`getFileByExtension`kernel[extension_][path_] :=
    Which[
        DirectoryQ[path],
            FileNames[__~~"."~~extension~~EndOfString,path],
        FileExistsQ[path]&&FileExtension[path]===extension,
            {path},
        True,
            {}
    ];


getFileNameByExtension[extension_][pathOrPathList_] :=
    StringReplace[
        getFileByExtension[extension][pathOrPathList],
        __~~"/"~~Longest[fileName__]~~"."~~extension~~EndOfString:>fileName
    ];


(* ::Subsection:: *)
(*addButtonTo*)


ifAddButtonTo[True,keys__][list_] :=
    addButtonTo[keys][list];
ifAddButtonTo[False,keys__][list_] :=
    list;

addButtonTo[key_String][list_] :=
    With[ {$$key = key},
        list//Query[All,<|#,$$key->lily`paper`common`addButtonTo`copyToClipboard[Slot[$$key]]|>&]
    ];
addButtonTo["URL"][list_] :=
    list//Query[All,<|#,"URL"->lily`paper`common`addButtonTo`hyperlink[#URL]|>&];
addButtonTo[key_,restKeys__][list_] :=
    list//addButtonTo[key]//addButtonTo[restKeys];


lily`paper`common`addButtonTo`hyperlink[value_String] :=
    Hyperlink[value,value,FrameMargins->Small];
lily`paper`common`addButtonTo`hyperlink[_] :=
    Missing["Failed"];

lily`paper`common`addButtonTo`copyToClipboard[value_String] :=
    Interpretation[{},
        Button[value,CopyToClipboard@value,Appearance->"Frameless",FrameMargins->Small],
        value
    ];
lily`paper`common`addButtonTo`copyToClipboard[_] :=
    Missing["Failed"];


(* ::Subsection:: *)
(*mergeByKey*)


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

(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
