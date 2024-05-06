[ 1 == 1
, 1e40*0 == 1e40*0
, (float)"nan" == (float)"nan"
, (float)"nan" == 1e40*0
, 3.14==3.14
, 1 == 2
, 3.14 == 3.1399999
, 1 != 1
, (float)"nan" != (float)"nan"
, "a" != "b"
, "a" == "b"
, "a" != "a"
, "a" == "a"
, [1,2] != [3,4]
, [1,2] == [3,4]
, [1] != [2,3]
, <1,2,3,4> == <1.,2.,3.,4.2>
, <1,2,3> == <1.,2.,3.>
, (key)NULL_KEY == (key)TEXTURE_BLANK
, (key)NULL_KEY != (key)TEXTURE_BLANK
, (key)NULL_KEY == NULL_KEY
, (key)"ABCDEFAB-ABCD-ABCD-ABCD-ABCDEFABCDEF" ==
  (key)"abcdefab-abcd-abcd-abcd-abcdefabcdef"
, 1 == 1.
, 1 < 2
, 2 > 1
, 2 < 2
, 2 > 2
, 3 < 2
, 2 > 3
, -2 < -1
, -2. < -1.
, -1. < -1.
, -0. < 0.
, (float)"nan" < 2
, (float)"nan" > 2
, 2 < (float)"nan"
, 2 > (float)"nan"
, (float)"nan" < (1e40*0)
, 1e40 < 1e40
, -1e40 < 1e40
, llAbs(llGetLinkNumber()-2) >= 0
, llAbs(llGetLinkNumber()-2) < 0
, llAbs(llGetLinkNumber()-2) < -1
, llAbs(llGetLinkNumber()-2) < 3
, llFabs(llSetRegionPos(<1, 1, 1>) - 0.5) < 0
]
