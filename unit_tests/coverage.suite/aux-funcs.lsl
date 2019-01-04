// Cover auxiliary functions.
default
{
    timer()
    {
        llOwnerSay(llStringToBase64(llList2Key([TEXTURE_BLANK],0)));
        llOwnerSay(llStringToBase64(llList2Key([(key)TEXTURE_BLANK],0)));
    }
}
