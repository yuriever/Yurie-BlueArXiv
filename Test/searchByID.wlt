

(*searchByID.nb*)

VerificationTest[
	Begin["Global`"];
	ClearAll["`*"]
	,
	Null
	,
	TestID->"0-searchByID.nb"
]

VerificationTest[
	Get["Yurie`BlueArXiv`"]; 
	Get["Yurie`BlueArXiv`Sample`"]
	,
	Null
	,
	TestID->"1-searchByID.nb"
]

VerificationTest[
	Off[$ContextAliases::cxinuse]; 
	$ContextAliases["dev`"] = "Yurie`BlueArXiv`searchByID`"; 
	($ContextAliases["devp`"] = "Yurie`BlueArXiv`searchByID`Private`"; )
	,
	Null
	,
	TestID->"2-searchByID.nb"
]

VerificationTest[
	idData = Normal[extractID["string", "ClickToCopy" -> False][sampleString["ID"]]]; 
	idList = Query[All, #ID & ][idData]
	,
	{"0000.00001", "1207.7214", "1706.03762", "hep-th/9802150"}
	,
	TestID->"3-searchByID.nb"
]

VerificationTest[
	devp`getPaperDataFromIDData[idData]
	,
	{Association["ID" -> "0000.00001", "Paper" -> Missing["Failed"], "URL" -> Missing["IDNotExist"]], Association["ID" -> "1207.7214", "Paper" -> "1207.7214v2 Observation of a new particle in the search for the Standard Model Higgs boson with the ATLAS detector at the LHC, The ATLAS Collaboration", "URL" -> "http://arxiv.org/pdf/1207.7214v2.pdf"], Association["ID" -> "1706.03762", "Paper" -> "1706.03762v7 Attention Is All You Need, Ashish Vaswani", "URL" -> "http://arxiv.org/pdf/1706.03762v7.pdf"], Association["ID" -> "hep-th/9802150", "Paper" -> "9802150v2 Anti De Sitter Space And Holography, Edward Witten", "URL" -> "http://arxiv.org/pdf/hep-th/9802150v2.pdf"]}
	,
	TestID->"4-searchByID.nb"
]

VerificationTest[
	rawPaperData = devp`getRawPaperDataFromIDList[idList]; 
	First[rawPaperData]
	,
	Association["PrimaryCategory" -> {Missing["NotAvailable"]}, "Category" -> {Missing["NotAvailable"]}]
	,
	TestID->"5-searchByID.nb"
]

VerificationTest[
	devp`getPaperNameListFromPaperData[rawPaperData]
	,
	{Missing["Failed"], "1207.7214v2 Observation of a new particle in the search for the Standard Model Higgs boson with the ATLAS detector at the LHC, The ATLAS Collaboration", "1706.03762v7 Attention Is All You Need, Ashish Vaswani", "9802150v2 Anti De Sitter Space And Holography, Edward Witten"}
	,
	TestID->"6-searchByID.nb"
]

VerificationTest[
	devp`getURLListFromPaperData[rawPaperData]
	,
	{Missing["IDNotExist"], "http://arxiv.org/pdf/1207.7214v2.pdf", "http://arxiv.org/pdf/1706.03762v7.pdf", "http://arxiv.org/pdf/hep-th/9802150v2.pdf"}
	,
	TestID->"7-searchByID.nb"
]

VerificationTest[
	devp`getURL[rawPaperData[[1]]]
	,
	Missing["IDNotExist"]
	,
	TestID->"8-searchByID.nb"
]

VerificationTest[
	img = Import[FileNames[All, sampleFileDirectory["png"]][[1]]]; 
	,
	Null
	,
	TestID->"9-searchByID.nb"
]

VerificationTest[
	Block[{Yurie`BlueArXiv`extractID`Private`showHighlightedImage = Nothing}, dev`searchByIDAsPaperData["image"][img]]
	,
	{Association["ID" -> "0000.00001", "Paper" -> Missing["Failed"], "URL" -> Missing["IDNotExist"]], Association["ID" -> "1207.7214", "Paper" -> "1207.7214v2 Observation of a new particle in the search for the Standard Model Higgs boson with the ATLAS detector at the LHC, The ATLAS Collaboration", "URL" -> "http://arxiv.org/pdf/1207.7214v2.pdf"], Association["ID" -> "1706.03762", "Paper" -> "1706.03762v7 Attention Is All You Need, Ashish Vaswani", "URL" -> "http://arxiv.org/pdf/1706.03762v7.pdf"], Association["ID" -> "hep-th/9802150", "Paper" -> "9802150v2 Anti De Sitter Space And Holography, Edward Witten", "URL" -> "http://arxiv.org/pdf/hep-th/9802150v2.pdf"]}
	,
	TestID->"10-searchByID.nb"
]

VerificationTest[
	dir = sampleFileDirectory["pdf"]; 
	,
	Null
	,
	TestID->"11-searchByID.nb"
]

VerificationTest[
	Normal[searchByID["path", "ClickToCopy" -> False][dir]]
	,
	{Association["ID" -> "0000.00001", "Paper" -> Missing["Failed"], "URL" -> Missing["IDNotExist"], "FileName" -> {"wrongID-0000.00001.pdf"}, "IDLocation" -> {"FileName"}], Association["ID" -> "1207.7214", "Paper" -> "1207.7214v2 Observation of a new particle in the search for the Standard Model Higgs boson with the ATLAS detector at the LHC, The ATLAS Collaboration", "URL" -> "http://arxiv.org/pdf/1207.7214v2.pdf", "FileName" -> {"newID-1207.7214.pdf"}, "IDLocation" -> {"FileName"}], Association["ID" -> "1706.03762", "Paper" -> "1706.03762v7 Attention Is All You Need, Ashish Vaswani", "URL" -> "http://arxiv.org/pdf/1706.03762v7.pdf", "FileName" -> {"csID-1706.03762.pdf"}, "IDLocation" -> {"FileName"}], Association["ID" -> "hep-th/9802150", "Paper" -> "9802150v2 Anti De Sitter Space And Holography, Edward Witten", "URL" -> "http://arxiv.org/pdf/hep-th/9802150v2.pdf", "FileName" -> {"oldID-9802150.pdf", "oldID-9802150.pdf"}, "IDLocation" -> {"FirstPageExtra", "FirstPageExtra"}], Association["ID" -> "NotFound", "Paper" -> Missing["IDNotExist"], "URL" -> Missing["IDNotExist"], "FileName" -> {"noID.pdf"}, "IDLocation" -> {"None"}]}
	,
	TestID->"12-searchByID.nb"
]

VerificationTest[
	$ContextAliases =. 
	,
	Null
	,
	TestID->"13-searchByID.nb"
]

VerificationTest[
	ClearAll["`*"];
	End[]
	,
	"Global`"
	,
	TestID->"∞-searchByID.nb"
]