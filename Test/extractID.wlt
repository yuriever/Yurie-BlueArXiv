

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
	str = sampleString["ID"]; 
	,
	Null
	,
	TestID->"2-extractID.nb"
]

VerificationTest[
	Yurie`BlueArXiv`extractID`Private`getIDDataFromString[str]
	,
	{Association["ID" -> "hep-th/9802150"], Association["ID" -> "1207.7214"], Association["ID" -> "1706.03762"], Association["ID" -> "0000.00001"]}
	,
	TestID->"3-extractID.nb"
]

VerificationTest[
	Yurie`BlueArXiv`extractID`extractIDData["string"][str]
	,
	{Association["ID" -> "0000.00001"], Association["ID" -> "1207.7214"], Association["ID" -> "1706.03762"], Association["ID" -> "hep-th/9802150"]}
	,
	TestID->"4-extractID.nb"
]

VerificationTest[
	Normal[extractID["string", "ClickToCopy" -> False, "ShowHighlightedImage" -> False][str]]
	,
	{Association["ID" -> "0000.00001"], Association["ID" -> "1207.7214"], Association["ID" -> "1706.03762"], Association["ID" -> "hep-th/9802150"]}
	,
	TestID->"5-extractID.nb"
]

VerificationTest[
	img = Import[FileNames[All, sampleFileDirectory["png"]][[1]]]; 
	,
	Null
	,
	TestID->"6-extractID.nb"
]

VerificationTest[
	Query[All, KeyDrop["Position"]][Yurie`BlueArXiv`extractID`Private`getIDDataFromImage["ShowHighlightedImage" -> False][img]]
	,
	{Association["ID" -> "hep-th/9802150"], Association["ID" -> "1207.7214"], Association["ID" -> "1706.03762"], Association["ID" -> "0000.00001"]}
	,
	TestID->"7-extractID.nb"
]

VerificationTest[
	Block[{Yurie`BlueArXiv`extractID`Private`showHighlightedImage = Nothing}, Yurie`BlueArXiv`extractID`Private`getIDDataFromImage[][img]]
	,
	{Association["ID" -> "hep-th/9802150"], Association["ID" -> "1207.7214"], Association["ID" -> "1706.03762"], Association["ID" -> "0000.00001"]}
	,
	TestID->"8-extractID.nb"
]

VerificationTest[
	Block[{Yurie`BlueArXiv`extractID`Private`showHighlightedImage = Nothing}, Yurie`BlueArXiv`extractID`extractIDData["image"][img]]
	,
	{Association["ID" -> "0000.00001"], Association["ID" -> "1207.7214"], Association["ID" -> "1706.03762"], Association["ID" -> "hep-th/9802150"]}
	,
	TestID->"9-extractID.nb"
]

VerificationTest[
	Query[All, KeyDrop["Position"]][Normal[extractID["image", "ClickToCopy" -> False, "ShowHighlightedImage" -> False][img]]]
	,
	{Association["ID" -> "0000.00001"], Association["ID" -> "1207.7214"], Association["ID" -> "1706.03762"], Association["ID" -> "hep-th/9802150"]}
	,
	TestID->"10-extractID.nb"
]

VerificationTest[
	dir = sampleFileDirectory["pdf"]; 
	(pdf = FileNames[All, dir][[1]]; )
	,
	Null
	,
	TestID->"11-extractID.nb"
]

VerificationTest[
	Yurie`BlueArXiv`extractID`Private`getIDDataFromPDF[][pdf]
	,
	{Association["ID" -> "1706.03762", "FileName" -> {"csID-1706.03762.pdf"}, "IDLocation" -> {"FileName"}]}
	,
	TestID->"12-extractID.nb"
]

VerificationTest[
	Yurie`BlueArXiv`extractID`Private`getIDDataFromPDF["TryFileName" -> False][pdf]
	,
	{Association["ID" -> "1706.03762", "FileName" -> {"csID-1706.03762.pdf"}, "IDLocation" -> {"FirstPage"}]}
	,
	TestID->"13-extractID.nb"
]

VerificationTest[
	temp = Yurie`BlueArXiv`extractID`Private`getIDDataFromPDF["HideDirectory" -> False][pdf]; 
	temp[[1,"FileName",1]] === pdf
	,
	True
	,
	TestID->"14-extractID.nb"
]

VerificationTest[
	Yurie`BlueArXiv`extractID`Private`getIDDataFromPath[][dir]
	,
	{Association["ID" -> "1706.03762", "FileName" -> {"csID-1706.03762.pdf"}, "IDLocation" -> {"FileName"}], Association["ID" -> "1207.7214", "FileName" -> {"newID-1207.7214.pdf"}, "IDLocation" -> {"FileName"}], Association["ID" -> "NotFound", "FileName" -> {"noID.pdf"}, "IDLocation" -> {"None"}], Association["ID" -> "hep-th/9802150", "FileName" -> {"oldID-9802150.pdf"}, "IDLocation" -> {"FirstPageExtra"}], Association["ID" -> "hep-th/9802150", "FileName" -> {"oldID-9802150.pdf"}, "IDLocation" -> {"FirstPageExtra"}], Association["ID" -> "0000.00001", "FileName" -> {"wrongID-0000.00001.pdf"}, "IDLocation" -> {"FileName"}]}
	,
	TestID->"15-extractID.nb"
]

VerificationTest[
	Yurie`BlueArXiv`extractID`extractIDData["path"][dir]
	,
	{Association["ID" -> "0000.00001", "FileName" -> {"wrongID-0000.00001.pdf"}, "IDLocation" -> {"FileName"}], Association["ID" -> "1207.7214", "FileName" -> {"newID-1207.7214.pdf"}, "IDLocation" -> {"FileName"}], Association["ID" -> "1706.03762", "FileName" -> {"csID-1706.03762.pdf"}, "IDLocation" -> {"FileName"}], Association["ID" -> "hep-th/9802150", "FileName" -> {"oldID-9802150.pdf", "oldID-9802150.pdf"}, "IDLocation" -> {"FirstPageExtra", "FirstPageExtra"}], Association["ID" -> "NotFound", "FileName" -> {"noID.pdf"}, "IDLocation" -> {"None"}]}
	,
	TestID->"16-extractID.nb"
]

VerificationTest[
	Normal[extractID["path", "ClickToCopy" -> False, "ShowHighlightedImage" -> False][dir]]
	,
	{Association["ID" -> "0000.00001", "FileName" -> {"wrongID-0000.00001.pdf"}, "IDLocation" -> {"FileName"}], Association["ID" -> "1207.7214", "FileName" -> {"newID-1207.7214.pdf"}, "IDLocation" -> {"FileName"}], Association["ID" -> "1706.03762", "FileName" -> {"csID-1706.03762.pdf"}, "IDLocation" -> {"FileName"}], Association["ID" -> "hep-th/9802150", "FileName" -> {"oldID-9802150.pdf", "oldID-9802150.pdf"}, "IDLocation" -> {"FirstPageExtra", "FirstPageExtra"}], Association["ID" -> "NotFound", "FileName" -> {"noID.pdf"}, "IDLocation" -> {"None"}]}
	,
	TestID->"17-extractID.nb"
]

VerificationTest[
	ClearAll["`*"];
	End[]
	,
	"Global`"
	,
	TestID->"∞-extractID.nb"
]