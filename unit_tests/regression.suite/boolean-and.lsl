default{timer(){
    string s = llGetObjectDesc();
    key k = llGenerateKey();
    integer i = llGetLinkNumber();

    if (s == "A" && k != llGenerateKey() && !~llSubStringIndex(s, "B"))
        llDie();
    /* Chance for a future optimization - this is always false */
    if (i == 5 && i == 9 && llGetLinkNumber())
        llDie();
    if (i != 5 && i != 9 && llGetLinkNumber())
        llDie();
    if (1 & i == 5 && i == 7 && i == 9 && llGetLinkNumber())
        llDie();
}}