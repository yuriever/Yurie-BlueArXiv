

(*edge-cases.nb*)

VerificationTest[
    Begin["Global`"];
	ClearAll["`*"]
    ,
    Null
    ,
    TestID->"0-edge-cases.nb"
]

VerificationTest[
    Get["Yurie`BlueArXiv`"]; 
    Get["Yurie`BlueArXiv`Sample`"]
    ,
    Null
    ,
    TestID->"1-edge-cases.nb"
]

VerificationTest[
    Normal[extractID["string"][""]]
    ,
    {}
    ,
    TestID->"2-edge-cases.nb"
]

VerificationTest[
    extractID["string"][x]
    ,
    Quiet[x]
    ,
    {General::elmntav}
    ,
    TestID->"3-edge-cases.nb"
]

VerificationTest[
    Normal[extractID["image"][Null]]
    ,
    {}
    ,
    TestID->"4-edge-cases.nb"
]

VerificationTest[
    extractID["image"][a]
    ,
    Quiet[a]
    ,
    {General::elmntav}
    ,
    TestID->"5-edge-cases.nb"
]

VerificationTest[
    Normal[extractID["path"][""]]
    ,
    {}
    ,
    TestID->"6-edge-cases.nb"
]

VerificationTest[
    extractID["path"][a]
    ,
    Quiet[a]
    ,
    {General::elmntav}
    ,
    TestID->"7-edge-cases.nb"
]

VerificationTest[
    ClearAll["`*"];
	End[]
    ,
    "Global`"
    ,
    TestID->"∞-edge-cases.nb"
]