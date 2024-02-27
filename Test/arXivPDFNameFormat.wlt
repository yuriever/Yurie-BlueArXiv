

(*arXivPDFNameFormat.nb*)

VerificationTest[
	Begin["Global`"];
	ClearAll["`*"]
	,
	Null
	,
	TestID->"init-arXivPDFNameFormat.nb"
]

VerificationTest[
	Get["Yurie`BlueArXiv`"]; 
	(list[] := {Yurie`BlueArXiv`Default`$arXivPDFNameFormatter, Yurie`BlueArXiv`Default`$arXivPDFNameRegulator}; )
	,
	Null
	,
	TestID->1
]

VerificationTest[
	list[]
	,
	{StringJoin[Lookup[#1, "ID"], " ", Lookup[#1, "Title", ""], ", ", Lookup[#1, "Author", "", #1[[1,"Name"]] & ]] & , Yurie`BlueArXiv`Common`regulateFileName}
	,
	TestID->2
]

VerificationTest[
	arXivPDFNameFormat["ID", f]; 
	list[]
	,
	{Lookup[#1, "ID"] & , f}
	,
	TestID->3
]

VerificationTest[
	arXivPDFNameFormat[]; 
	list[]
	,
	{StringJoin[Lookup[#1, "ID"], " ", Lookup[#1, "Title", ""], ", ", Lookup[#1, "Author", "", #1[[1,"Name"]] & ]] & , Yurie`BlueArXiv`Common`regulateFileName}
	,
	TestID->4
]

VerificationTest[
	ClearAll["`*"];
	End[]
	,
	"Global`"
	,
	TestID->"cleanup-arXivPDFNameFormat.nb"
]