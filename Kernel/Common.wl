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


regulateFileName::usage =
    "regulate special characters in file name.";

getFilePathByExtension::usage =
    "get file paths from path or list of paths by specifying the extension.";

getFileNameByExtension::usage =
    "get file names from path or list of paths by specifying the extension.";

ifAddButton::usage =
    "whether to add click-to-copy/hyperlink button to list of associations.";

addButton::usage =
    "add click-to-copy/hyperlink button to list of associations.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Constant*)


$arXivIDPattern =
    RegularExpression["(\\d{4}\\.\\d{4,5})|((astro-ph|cond-mat|gr-qc|hep-ex|hep-lat|hep-ph|hep-th|math-ph|nlin|nucl-ex|nucl-th|physics|quant-ph|math|cs)/\\d{7})"];


$citeKeyPattern =
    (*no whitespace tolerance.*)
    RegularExpression["(\\\\cite{)(\\S*?)(})"];


(* ::Subsection:: *)
(*Main*)


(* ::Subsubsection:: *)
(*regulateFileName*)


regulateFileName[string_String] :=
    RemoveDiacritics@StringReplace[
        string,
        {
            ":"->" -",
            "/"->"_",
            "\n"|"\r"->" ",
            "\[CloseCurlyQuote]"->"'"
        }
    ];


(* ::Subsubsection:: *)
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


(* ::Subsubsection:: *)
(*getFileNameByExtension*)


getFileNameByExtension[extension_][pathOrPathList_] :=
    pathOrPathList//getFilePathByExtension[extension]//Map[FileBaseName];


(* ::Subsubsection:: *)
(*ifAddButton*)


ifAddButton[True][list:{___String}] :=
    addButtonWithCopyToClipboard/@list;

ifAddButton[True,keys__][list:{___Association}] :=
    addButton[keys][list];

ifAddButton[False,___][list_] :=
    list;


addButton[key_String][list_] :=
    With[ {key0 = key},
        list//Query[All,<|#,key0->addButtonWithCopyToClipboard[Slot[key0]]|>&]
    ];

addButton["URL"][list_] :=
    list//Query[All,<|#,"URL"->addButtonWithHyperlink[#URL]|>&];

addButton[key_,restKeys__][list_] :=
    list//addButton[key]//addButton[restKeys];


addButtonWithHyperlink[value_String] :=
    Hyperlink[value,value,FrameMargins->Small];

addButtonWithHyperlink[_] :=
    Missing["Failed"];


addButtonWithCopyToClipboard[value_] :=
    Interpretation[{},
        Button[value,CopyToClipboard@value,Appearance->"Frameless",FrameMargins->Small],
        value
    ];

addButtonWithCopyToClipboard[_Missing] :=
    Missing["Failed"];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
