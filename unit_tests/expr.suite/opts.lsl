// Operator optimization tests
[ llGetLinkNumber() + 1
, llGetLinkNumber() + 2
, llGetLinkNumber() + 3
, llGetLinkNumber() + 4
, llGetLinkNumber() - 1
, llGetLinkNumber() - 2
, llGetLinkNumber() - 3
, llGetLinkNumber() - 4
, -~-~-~llGetLinkNumber()
, ~-~-~-llGetLinkNumber()
, llGetLinkNumber() + 3 + 2
// currently optimized to -3 + -~-~llGetLinkNumber(); we could do better here
, 2 + llGetLinkNumber() - 3
, -(-llGetLinkNumber() + 2)
, llGetLinkNumber()==2
, llGetLinkNumber() + llGetLinkNumber() + llGetLinkNumber()
, llGetLinkNumber() > 2
// TODO: Reduce stack here: Swap to 2 > llGetLinkNumber()
, llGetLinkNumber() < 2
, llGetTime() + 1 + 2
, 5 + llGetLinkNumber() + 6
, llGetLinkNumber() - 5 == 0
// TODO: Can be simplified by adding 5 to both sides.
// Don't do the same with > and < because of wraparound.
, llGetLinkNumber() - 5 == 12
, llGetLinkNumber() - 5 != 12
, -(-1-(-(-1-(-(-1-(llGetLinkNumber()))))))
, -1-llGetLinkNumber()
, -~~-llGetLinkNumber()
, (llGetLinkNumber() + 2) + (llGetLinkNumber() + 2)
, (llGetLinkNumber() + 2) + (llGetLinkNumber() - 2)
, (llGetLinkNumber() - 2) + (llGetLinkNumber() - 2)
, (llGetLinkNumber() + 5) + (llGetLinkNumber() + 5)
, llGetLinkNumber()*llGetLinkNumber() - llGetLinkNumber()*llGetLinkNumber()
]
