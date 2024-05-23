

(*extractTitleFromPDF.nb*)

VerificationTest[
	Begin["Global`"];
	ClearAll["`*"]
	,
	Null
	,
	TestID->"0-extractTitleFromPDF.nb"
]

VerificationTest[
	Get["Yurie`BlueArXiv`Sample`"]; 
	Get["Yurie`PaperTool`"]
	,
	Null
	,
	TestID->"1-extractTitleFromPDF.nb"
]

VerificationTest[
	SetOptions[extractTitleFromPDF, "ClickToCopy" -> False]; 
	,
	Null
	,
	TestID->"2-extractTitleFromPDF.nb"
]

VerificationTest[
	Normal[extractTitleFromPDF[][sampleFileDirectory["pdf"]]]
	,
	{Association["Title" -> "Attention is all you Need", "FileName" -> {"csID-1706.03762.pdf"}], Association["Title" -> "European Organisation for Nuclear Research (cern)", "FileName" -> {"newID-1207.7214.pdf"}], Association["Title" -> "this is a Sample Pdf File without Id.", "FileName" -> {"noID.pdf"}], Association["Title" -> "Anti De Sitter Space and Holography", "FileName" -> {"oldID-9802150.pdf"}], Association["Title" -> "this is a Sample Pdf File with Wrong Id 0000.00001.", "FileName" -> {"wrongID-0000.00001.pdf"}]}
	,
	TestID->"3-extractTitleFromPDF.nb"
]

VerificationTest[
	ClearAll["`*"];
	End[]
	,
	"Global`"
	,
	TestID->"∞-extractTitleFromPDF.nb"
]