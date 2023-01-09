default
{
    state_entry()
    {
        llOwnerSay(llList2Key(llGetObjectDetails("", [1]),0));
        llOwnerSay(llChar(0xA1));
    }
}
