

(*extractTitleFromPDF.nb*)

VerificationTest[
	Begin["Global`"];
	ClearAll["`*"]
	,
	Null
	,
	TestID->"init-extractTitleFromPDF.nb"
]

VerificationTest[
	Get["Yurie`BlueArXiv`Sample`"]; 
	Get["Yurie`PaperTool`"]
	,
	Null
	,
	TestID->1
]

VerificationTest[
	SetOptions[extractTitleFromPDF, "clickToCopy" -> False]; 
	,
	Null
	,
	TestID->2
]

VerificationTest[
	Normal[extractTitleFromPDF[][sampleFileDirectory["pdf"]]]
	,
	{Association["title" -> "Attention is all you Need", "file" -> "csID-1706.03762"], Association["title" -> "European Organisation for Nuclear Research (cern)", "file" -> "newID-1207.7214"], Association["title" -> "Anti De Sitter Space and Holography", "file" -> "oldID-9802150"], Association["title" -> "this is an Example File.", "file" -> "wrongID-0000.00001"]}
	,
	TestID->3
]

VerificationTest[
	ClearAll["`*"];
	End[]
	,
	"Global`"
	,
	TestID->"cleanup-extractTitleFromPDF.nb"
]