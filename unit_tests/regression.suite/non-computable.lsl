// Functions that can't be computed at compile time
default
{
    timer()
    {
        llSetPrimitiveParams( // we need a function that causes side effects,
                              // so that it isn't optimized out
            [ llFrand(1)
            , llFrand(1.1754943508222875e-38)
            , llGenerateKey()
            , llAvatarOnLinkSitTarget(2)
            , llEdgeOfWorld(<1,1,1>, <1,1,1>)
            , llGetAgentInfo(TEXTURE_BLANK)
            , llGetAgentLanguage(TEXTURE_BLANK)
            , llGetAgentList(AGENT_LIST_PARCEL, [])
            , llGetAgentSize(TEXTURE_DEFAULT)
            , llGetAlpha(0)
            , llGetAnimation(TEXTURE_BLANK)
            , llGetAnimationList(TEXTURE_BLANK)
            , llGetBoundingBox(TEXTURE_BLANK)
            , llGetColor(0)
            , llGetDisplayName(TEXTURE_BLANK)
            , llGetEnv("estate_name")
            , llXorBase64Strings("++++", "?")
            , llAbs(-2147483648)
            , llGetStatus(STATUS_DIE_AT_EDGE)
            , llGetStatus(STATUS_DIE_AT_NO_ENTRY)
            ]);
    }
}
