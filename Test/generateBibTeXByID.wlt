

(*generateBibTeXByID.nb*)

VerificationTest[
    Begin["Global`"];
	ClearAll["`*"]
    ,
    Null
    ,
    TestID->"0-generateBibTeXByID.nb"
]

VerificationTest[
    Get["Yurie`BlueArXiv`Sample`"]; 
    Get["Yurie`BlueArXiv`"]
    ,
    Null
    ,
    TestID->"1-generateBibTeXByID.nb"
]

VerificationTest[
    SetOptions[generateBibTeXByID, "ClickToCopy" -> False]; 
    ,
    Null
    ,
    TestID->"2-generateBibTeXByID.nb"
]

VerificationTest[
    dir = FileNameJoin[{$HomeDirectory, "Downloads"}]; 
    Block[{Yurie`BlueArXiv`generateBibTeXByID`Private`exportBib}, Normal[Query[All, KeyDrop["BibTeX"]][generateBibTeXByID["string", dir, "refs-string.bib"][sampleString["ID"]]]]]
    ,
    {Association["BibKey" -> Missing["BibKeyNotFound"], "ID" -> "0000.00001"], Association["BibKey" -> "ATLAS:2012yve", "ID" -> "1207.7214"], Association["BibKey" -> "Vaswani:2017lxt", "ID" -> "1706.03762"], Association["BibKey" -> "Witten:1998qj", "ID" -> "hep-th/9802150"]}
    ,
    TestID->"3-generateBibTeXByID.nb"
]

VerificationTest[
    img = Import[FileNames[All, sampleFileDirectory["png"]][[1]]]; 
    Block[{Yurie`BlueArXiv`generateBibTeXByID`Private`exportBib, Yurie`BlueArXiv`extractID`Private`showHighlightedImage = Nothing}, Normal[Query[All, KeyDrop["BibTeX"]][generateBibTeXByID["image", dir, "refs-path.bib"][img]]]]
    ,
    {Association["BibKey" -> Missing["BibKeyNotFound"], "ID" -> "0000.00001"], Association["BibKey" -> "ATLAS:2012yve", "ID" -> "1207.7214"], Association["BibKey" -> "Vaswani:2017lxt", "ID" -> "1706.03762"], Association["BibKey" -> "Witten:1998qj", "ID" -> "hep-th/9802150"]}
    ,
    TestID->"4-generateBibTeXByID.nb"
]

VerificationTest[
    Block[{Yurie`BlueArXiv`generateBibTeXByID`Private`exportBib}, Normal[Query[All, KeyDrop["BibTeX"]][generateBibTeXByID["path", dir, "refs-path.bib"][sampleFileDirectory["pdf"]]]]]
    ,
    {Association["BibKey" -> Missing["BibKeyNotFound"], "ID" -> "0000.00001", "FileName" -> {"wrongID-0000.00001.pdf"}, "IDLocation" -> {"FileName"}], Association["BibKey" -> "ATLAS:2012yve", "ID" -> "1207.7214", "FileName" -> {"newID-1207.7214.pdf"}, "IDLocation" -> {"FileName"}], Association["BibKey" -> "Vaswani:2017lxt", "ID" -> "1706.03762", "FileName" -> {"csID-1706.03762.pdf"}, "IDLocation" -> {"FileName"}], Association["BibKey" -> "Witten:1998qj", "ID" -> "hep-th/9802150", "FileName" -> {"oldID-9802150.pdf", "oldID-9802150.pdf"}, "IDLocation" -> {"FirstPageExtra", "FirstPageExtra"}], Association["BibKey" -> Missing["IDNotExist"], "ID" -> "NotFound", "FileName" -> {"noID.pdf"}, "IDLocation" -> {"None"}]}
    ,
    TestID->"5-generateBibTeXByID.nb"
]

VerificationTest[
    ClearAll["`*"];
	End[]
    ,
    "Global`"
    ,
    TestID->"∞-generateBibTeXByID.nb"
]