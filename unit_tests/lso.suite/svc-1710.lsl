default
{
    timer()
    {
        llSetPrimitiveParams(
            [ llGetListEntryType((list)((key)""),0) // TYPE_STRING in LSO
            , llGetListEntryType((list)((string)llList2Key([],0)),0) // TYPE_KEY
            , llGetListEntryType([] + (key)"",0) // TYPE_KEY (does it right)
            , llGetListEntryType([] + (string)llList2Key([],0),0) // TYPE_KEY
            ]);
    }
}
