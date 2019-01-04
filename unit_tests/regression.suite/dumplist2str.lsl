default
{
    state_entry()
    {
        string a = llGetObjectName();
        vector b = llGetPos();
        float c = llGetMass();
        llOwnerSay(llDumpList2String([a, b, c], "/"));
        llOwnerSay(llDumpList2String([], llGetObjectName()));
        llOwnerSay(llDumpList2String([1,2,3], a));
        llOwnerSay(llDumpList2String([llGetObjectName()], a));
        llOwnerSay(llDumpList2String([llGetObjectName(), llGetObjectName(), llGetObjectName()], a));
        llOwnerSay(llDumpList2String([a, b, c, llGetObjectName()], a));
        // TODO: This isn't properly optimized because the lists can't be merged:
        llOwnerSay(llDumpList2String([a] + [b, c, a + a + a], a));
        llOwnerSay(llDumpList2String([a, b, c, a + a + a], a));
        llOwnerSay(llDumpList2String([llSetRegionPos(<1,1,1>)], a));
        llOwnerSay(llDumpList2String([llSetRegionPos(<1,1,1>)], (string)llSetRegionPos(<1,1,1>)));
    }
}
