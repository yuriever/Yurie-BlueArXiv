

(*extractID.nb*)

VerificationTest[
	Begin["Global`"];
	ClearAll["`*"]
	,
	Null
	,
	TestID->"0-extractID.nb"
]

VerificationTest[
	Get["Yurie`BlueArXiv`"]; 
	Get["Yurie`BlueArXiv`Sample`"]
	,
	Null
	,
	TestID->"1-extractID.nb"
]

VerificationTest[
	SetOptions[extractID, "clickToCopy" -> False]; 
	,
	Null
	,
	TestID->"2-extractID.nb"
]

VerificationTest[
	extractID[][sampleString["ID"]]
	,
	{"0000.00001", "1207.7214", "1706.03762", "hep-th/9802150"}
	,
	TestID->"3-extractID.nb"
]

VerificationTest[
	Normal[extractID["path"][sampleFileDirectory["pdf"]]]
	,
	{Association["ID" -> "0000.00001", "file" -> {"wrongID-0000.00001"}, "IDLocation" -> {"foundInFileName"}], Association["ID" -> "1207.7214", "file" -> {"newID-1207.7214"}, "IDLocation" -> {"foundInFileName"}], Association["ID" -> "1706.03762", "file" -> {"csID-1706.03762"}, "IDLocation" -> {"foundInFileName"}], Association["ID" -> "hep-th/9802150", "file" -> {"oldID-9802150"}, "IDLocation" -> {"foundInFirstPage"}]}
	,
	TestID->"4-extractID.nb"
]

VerificationTest[
	ClearAll["`*"];
	End[]
	,
	"Global`"
	,
	TestID->"∞-extractID.nb"
]