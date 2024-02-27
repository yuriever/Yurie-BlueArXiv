

(*extractCiteKey.nb*)

VerificationTest[
	Begin["Global`"];
	ClearAll["`*"]
	,
	Null
	,
	TestID->"init-extractCiteKey.nb"
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
	SetOptions[extractCiteKey, "clickToCopy" -> False]; 
	,
	Null
	,
	TestID->2
]

VerificationTest[
	extractCiteKey[][sampleString["citeKey"]]
	,
	{"ATLAS:2012yve", "dieudonné1969treatise", "vaswani2017attention", "Witten:1998qj"}
	,
	TestID->3
]

VerificationTest[
	Normal[extractCiteKey["path"][sampleFileDirectory["tex"]]]
	,
	{Association["citeKey" -> "ATLAS:2012yve", "file" -> "citeKey1"], Association["citeKey" -> "dieudonné1969treatise", "file" -> "citeKey2"], Association["citeKey" -> "vaswani2017attention", "file" -> "citeKey1"], Association["citeKey" -> "Witten:1998qj", "file" -> "citeKey1"]}
	,
	TestID->4
]

VerificationTest[
	ClearAll["`*"];
	End[]
	,
	"Global`"
	,
	TestID->"cleanup-extractCiteKey.nb"
]