

(*arXivPDFNameFormat.nb*)

VerificationTest[
	Begin["Global`"];
	ClearAll["`*"]
	,
	Null
	,
	TestID->"0-arXivPDFNameFormat.nb"
]

VerificationTest[
	Get["Yurie`BlueArXiv`"]
	,
	Null
	,
	TestID->"1-arXivPDFNameFormat.nb"
]

VerificationTest[
	list := {Yurie`BlueArXiv`Default`$arXivPDFNameFormatter, Yurie`BlueArXiv`Default`$arXivPDFNameRegulator}; 
	list
	,
	{StringJoin[Lookup[#1, "ID"], " ", Lookup[#1, "Title", ""], ", ", Lookup[#1, "Author", "", #1[[1,"Name"]] & ]] & , Yurie`BlueArXiv`Common`regulateFileName}
	,
	TestID->"2-arXivPDFNameFormat.nb"
]

VerificationTest[
	arXivPDFNameFormat["ID", f]; 
	list
	,
	{Lookup[#1, "ID"] & , f}
	,
	TestID->"3-arXivPDFNameFormat.nb"
]

VerificationTest[
	arXivPDFNameFormat[]; 
	list
	,
	{StringJoin[Lookup[#1, "ID"], " ", Lookup[#1, "Title", ""], ", ", Lookup[#1, "Author", "", #1[[1,"Name"]] & ]] & , Yurie`BlueArXiv`Common`regulateFileName}
	,
	TestID->"4-arXivPDFNameFormat.nb"
]

VerificationTest[
	ClearAll["`*"];
	End[]
	,
	"Global`"
	,
	TestID->"∞-arXivPDFNameFormat.nb"
]