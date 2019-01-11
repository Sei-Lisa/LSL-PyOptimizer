default
{
    state_entry()
    {
        list L1 = [1, 2, 3, 4, 5];
        integer N = llGetListLength(L1);
        integer i;
        for (i = 0; i < N; i++)
        {
            llOwnerSay(llList2String(L1, i));
            L1 = llDeleteSubList(L1, -1, -1);
        }

        // Swap two variables
        integer u = llGetLinkNumber();
        integer v = llGetNumberOfPrims();
        if (llFrand(1) < 0.5)
        {
            integer temp = u;
            u = v;
            v = temp;
        }
        llOwnerSay((string)u+(string)v);
    }
}
