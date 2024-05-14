

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
	SetOptions[generateBibTeXByID, "clickToCopy" -> False]; 
	,
	Null
	,
	TestID->"2-generateBibTeXByID.nb"
]

VerificationTest[
	dir = FileNameJoin[{$HomeDirectory, "Downloads"}]; 
	,
	Null
	,
	TestID->"3-generateBibTeXByID.nb"
]

VerificationTest[
	Block[{Yurie`BlueArXiv`generateBibTeXByID`Private`exportBibTeXFile}, Normal[Query[All, KeyDrop["BibTeX"]][generateBibTeXByID[dir, "refs-string.bib"][sampleString["ID"]]]]]
	,
	{Association["key" -> Missing["Failed"], "ID" -> "0000.00001"], Association["key" -> "ATLAS:2012yve", "ID" -> "1207.7214"], Association["key" -> "Vaswani:2017lxt", "ID" -> "1706.03762"], Association["key" -> "Witten:1998qj", "ID" -> "hep-th/9802150"]}
	,
	TestID->"4-generateBibTeXByID.nb"
]

VerificationTest[
	Block[{Yurie`BlueArXiv`generateBibTeXByID`Private`exportBibTeXFile}, Normal[Query[All, KeyDrop["BibTeX"]][generateBibTeXByID["path", dir, "refs-path.bib"][sampleFileDirectory["pdf"]]]]]
	,
	{Association["key" -> Missing["Failed"], "ID" -> "0000.00001", "file" -> {"wrongID-0000.00001"}, "IDLocation" -> {"foundInFileName"}], Association["key" -> "ATLAS:2012yve", "ID" -> "1207.7214", "file" -> {"newID-1207.7214"}, "IDLocation" -> {"foundInFileName"}], Association["key" -> "Vaswani:2017lxt", "ID" -> "1706.03762", "file" -> {"csID-1706.03762"}, "IDLocation" -> {"foundInFileName"}], Association["key" -> "Witten:1998qj", "ID" -> "hep-th/9802150", "file" -> {"oldID-9802150"}, "IDLocation" -> {"foundInFirstPage"}]}
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