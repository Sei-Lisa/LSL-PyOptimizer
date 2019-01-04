default
{
    changed(integer ch)
    {
        if (llListFindList(llGetPrimitiveParams([PRIM_TYPE]), ["a"]) < 0)
            llDie();

        if (llSameGroup(llGetOwner()) < -3)
            llOwnerSay("a");
        if (llSameGroup(llGetOwner()) < -2)
            llOwnerSay("b");
        if (llSameGroup(llGetOwner()) < -1)
            llOwnerSay("c");
        if (llSameGroup(llGetOwner()) < 0)
            llOwnerSay("d");
        if (llSameGroup(llGetOwner()) < 1)
            llOwnerSay("e");
        if (llSameGroup(llGetOwner()) < 2)
            llOwnerSay("f");
        if (llSameGroup(llGetOwner()) < 3)
            llOwnerSay("g");
        if (llSameGroup(llGetOwner()) < 4)
            llOwnerSay("h");
        if (llSameGroup(llGetOwner()) > -3)
            llOwnerSay("A");
        if (llSameGroup(llGetOwner()) > -2)
            llOwnerSay("B");
        if (llSameGroup(llGetOwner()) > -1)
            llOwnerSay("C");
        if (llSameGroup(llGetOwner()) > 0)
            llOwnerSay("D");
        if (llSameGroup(llGetOwner()) > 1)
            llOwnerSay("E");
        if (llSameGroup(llGetOwner()) > 2)
            llOwnerSay("F");
        if (llSameGroup(llGetOwner()) > 3)
            llOwnerSay("G");
        if (llSameGroup(llGetOwner()) > 4)
            llOwnerSay("H");
    }
}
