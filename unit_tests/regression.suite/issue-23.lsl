default
{
    state_entry()
    {
        string tmp0 = llGetScriptName();
        list tmp1 = [llGetOwner(), llGetOwner()];
        list tmp2 = [llGetScriptName(), llGetScriptName()];
        string encoded = llDumpList2String([tmp0, llList2CSV(tmp1), "Hello", 123, llList2CSV(tmp2)], "|");
        llOwnerSay(encoded);
    }
}
