// Under Mono, cast to string/key then to list returns correct types.
default
{
    timer()
    {
        llSetPrimitiveParams(
            [ llGetListEntryType((list)((key)""),0)
            , llGetListEntryType((list)((string)llList2Key([],0)),0)
            , llGetListEntryType([] + (key)"",0)
            , llGetListEntryType([] + (string)llList2Key([],0),0)
            ]);
    }
}
