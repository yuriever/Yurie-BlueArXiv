

(*downloadByID.nb*)

VerificationTest[
    Begin["Global`"];
	ClearAll["`*"]
    ,
    Null
    ,
    TestID->"0-downloadByID.nb"
]

VerificationTest[
    Get["Yurie`BlueArXiv`Sample`"]; 
    Get["Yurie`BlueArXiv`"]
    ,
    Null
    ,
    TestID->"1-downloadByID.nb"
]

VerificationTest[
    SetOptions[downloadByID, "ClickToCopy" -> False, "HideFile" -> True]; 
    ,
    Null
    ,
    TestID->"2-downloadByID.nb"
]

VerificationTest[
    dir = FileNameJoin[{$HomeDirectory, "Downloads"}]; 
    Block[{Yurie`BlueArXiv`downloadByID`Private`downloadByURL}, Normal[downloadByID[dir][sampleString["ID"]]]]
    ,
    {Association["ID" -> "0000.00001", "Paper" -> Missing["Failed"], "URL" -> Missing["IDNotExist"]], Association["ID" -> "1207.7214", "Paper" -> "1207.7214v2 Observation of a new particle in the search for the Standard Model Higgs boson with the ATLAS detector at the LHC, The ATLAS Collaboration", "URL" -> "http://arxiv.org/pdf/1207.7214v2.pdf"], Association["ID" -> "1706.03762", "Paper" -> "1706.03762v7 Attention Is All You Need, Ashish Vaswani", "URL" -> "http://arxiv.org/pdf/1706.03762v7.pdf"], Association["ID" -> "hep-th/9802150", "Paper" -> "9802150v2 Anti De Sitter Space And Holography, Edward Witten", "URL" -> "http://arxiv.org/pdf/hep-th/9802150v2.pdf"]}
    ,
    TestID->"3-downloadByID.nb"
]

VerificationTest[
    img = Import[FileNames[All, sampleFileDirectory["png"]][[1]]]; 
    Block[{Yurie`BlueArXiv`downloadByID`Private`downloadByURL, Yurie`BlueArXiv`extractID`Private`showHighlightedImage = Nothing}, Normal[downloadByID["image", dir][img]]]
    ,
    {Association["ID" -> "0000.00001", "Paper" -> Missing["Failed"], "URL" -> Missing["IDNotExist"]], Association["ID" -> "1207.7214", "Paper" -> "1207.7214v2 Observation of a new particle in the search for the Standard Model Higgs boson with the ATLAS detector at the LHC, The ATLAS Collaboration", "URL" -> "http://arxiv.org/pdf/1207.7214v2.pdf"], Association["ID" -> "1706.03762", "Paper" -> "1706.03762v7 Attention Is All You Need, Ashish Vaswani", "URL" -> "http://arxiv.org/pdf/1706.03762v7.pdf"], Association["ID" -> "hep-th/9802150", "Paper" -> "9802150v2 Anti De Sitter Space And Holography, Edward Witten", "URL" -> "http://arxiv.org/pdf/hep-th/9802150v2.pdf"]}
    ,
    TestID->"4-downloadByID.nb"
]

VerificationTest[
    Block[{Yurie`BlueArXiv`downloadByID`Private`downloadByURL}, Normal[downloadByID["path", dir][sampleFileDirectory["pdf"]]]]
    ,
    {Association["ID" -> "0000.00001", "Paper" -> Missing["Failed"], "URL" -> Missing["IDNotExist"], "FileName" -> {"wrongID-0000.00001.pdf"}, "IDLocation" -> {"FileName"}], Association["ID" -> "1207.7214", "Paper" -> "1207.7214v2 Observation of a new particle in the search for the Standard Model Higgs boson with the ATLAS detector at the LHC, The ATLAS Collaboration", "URL" -> "http://arxiv.org/pdf/1207.7214v2.pdf", "FileName" -> {"newID-1207.7214.pdf"}, "IDLocation" -> {"FileName"}], Association["ID" -> "1706.03762", "Paper" -> "1706.03762v7 Attention Is All You Need, Ashish Vaswani", "URL" -> "http://arxiv.org/pdf/1706.03762v7.pdf", "FileName" -> {"csID-1706.03762.pdf"}, "IDLocation" -> {"FileName"}], Association["ID" -> "hep-th/9802150", "Paper" -> "9802150v2 Anti De Sitter Space And Holography, Edward Witten", "URL" -> "http://arxiv.org/pdf/hep-th/9802150v2.pdf", "FileName" -> {"oldID-9802150.pdf", "oldID-9802150.pdf"}, "IDLocation" -> {"FirstPageExtra", "FirstPageExtra"}], Association["ID" -> "NotFound", "Paper" -> Missing["IDNotExist"], "URL" -> Missing["IDNotExist"], "FileName" -> {"noID.pdf"}, "IDLocation" -> {"None"}]}
    ,
    TestID->"5-downloadByID.nb"
]

VerificationTest[
    ClearAll["`*"];
	End[]
    ,
    "Global`"
    ,
    TestID->"∞-downloadByID.nb"
]