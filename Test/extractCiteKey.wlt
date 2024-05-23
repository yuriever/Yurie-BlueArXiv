

(*extractCiteKey.nb*)

VerificationTest[
	Begin["Global`"];
	ClearAll["`*"]
	,
	Null
	,
	TestID->"0-extractCiteKey.nb"
]

VerificationTest[
	Get["Yurie`BlueArXiv`Sample`"]; 
	Get["Yurie`PaperTool`"]
	,
	Null
	,
	TestID->"1-extractCiteKey.nb"
]

VerificationTest[
	SetOptions[extractCiteKey, "ClickToCopy" -> False]; 
	,
	Null
	,
	TestID->"2-extractCiteKey.nb"
]

VerificationTest[
	extractCiteKey["string", "RawCiteKey" -> True][sampleString["CiteKey"]]
	,
	{"ATLAS:2012yve", "dieudonné1969treatise", "vaswani2017attention", "Witten:1998qj"}
	,
	TestID->"3-extractCiteKey.nb"
]

VerificationTest[
	Normal[extractCiteKey["string"][sampleString["CiteKey"]]]
	,
	{Association["CiteKey" -> "ATLAS:2012yve"], Association["CiteKey" -> "dieudonné1969treatise"], Association["CiteKey" -> "vaswani2017attention"], Association["CiteKey" -> "Witten:1998qj"]}
	,
	TestID->"4-extractCiteKey.nb"
]

VerificationTest[
	extractCiteKey["path", "RawCiteKey" -> True][sampleFileDirectory["tex"]]
	,
	{"ATLAS:2012yve", "dieudonné1969treatise", "vaswani2017attention", "Witten:1998qj"}
	,
	TestID->"5-extractCiteKey.nb"
]

VerificationTest[
	Normal[extractCiteKey["path"][sampleFileDirectory["tex"]]]
	,
	{Association["CiteKey" -> "ATLAS:2012yve", "FileName" -> {"citeKey1.tex"}], Association["CiteKey" -> "dieudonné1969treatise", "FileName" -> {"citeKey2.tex"}], Association["CiteKey" -> "vaswani2017attention", "FileName" -> {"citeKey1.tex"}], Association["CiteKey" -> "Witten:1998qj", "FileName" -> {"citeKey1.tex"}]}
	,
	TestID->"6-extractCiteKey.nb"
]

VerificationTest[
	ClearAll["`*"];
	End[]
	,
	"Global`"
	,
	TestID->"∞-extractCiteKey.nb"
]