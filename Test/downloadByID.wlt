

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
	SetOptions[downloadByID, "clickToCopy" -> False, "hideFileObject" -> True]; 
	,
	Null
	,
	TestID->"2-downloadByID.nb"
]

VerificationTest[
	dir = FileNameJoin[{$HomeDirectory, "Downloads"}]; 
	,
	Null
	,
	TestID->"3-downloadByID.nb"
]

VerificationTest[
	Block[{Yurie`BlueArXiv`downloadByID`Private`downloadPDFFromURLAsFileObject}, Normal[downloadByID[dir][sampleString["ID"]]]]
	,
	{Association["ID" -> "0000.00001", "item" -> Missing["Failed"], "URL" -> Missing["Failed"]], Association["ID" -> "1207.7214", "item" -> "1207.7214v2 Observation of a new particle in the search for the Standard Model Higgs boson with the ATLAS detector at the LHC, The ATLAS Collaboration", "URL" -> "http://arxiv.org/pdf/1207.7214v2.pdf"], Association["ID" -> "1706.03762", "item" -> "1706.03762v7 Attention Is All You Need, Ashish Vaswani", "URL" -> "http://arxiv.org/pdf/1706.03762v7.pdf"], Association["ID" -> "hep-th/9802150", "item" -> "9802150v2 Anti De Sitter Space And Holography, Edward Witten", "URL" -> "http://arxiv.org/pdf/hep-th/9802150v2.pdf"]}
	,
	TestID->"4-downloadByID.nb"
]

VerificationTest[
	Block[{Yurie`BlueArXiv`downloadByID`Private`downloadPDFFromURLAsFileObject}, Normal[downloadByID["path", dir][sampleFileDirectory["pdf"]]]]
	,
	{Association["ID" -> "0000.00001", "item" -> Missing["Failed"], "URL" -> Missing["Failed"], "file" -> {"wrongID-0000.00001"}, "IDLocation" -> {"foundInFileName"}], Association["ID" -> "1207.7214", "item" -> "1207.7214v2 Observation of a new particle in the search for the Standard Model Higgs boson with the ATLAS detector at the LHC, The ATLAS Collaboration", "URL" -> "http://arxiv.org/pdf/1207.7214v2.pdf", "file" -> {"newID-1207.7214"}, "IDLocation" -> {"foundInFileName"}], Association["ID" -> "1706.03762", "item" -> "1706.03762v7 Attention Is All You Need, Ashish Vaswani", "URL" -> "http://arxiv.org/pdf/1706.03762v7.pdf", "file" -> {"csID-1706.03762"}, "IDLocation" -> {"foundInFileName"}], Association["ID" -> "hep-th/9802150", "item" -> "9802150v2 Anti De Sitter Space And Holography, Edward Witten", "URL" -> "http://arxiv.org/pdf/hep-th/9802150v2.pdf", "file" -> {"oldID-9802150"}, "IDLocation" -> {"foundInFirstPage"}]}
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