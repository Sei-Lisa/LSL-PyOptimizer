// Functions for which certain parameters produce predictable results.
default
{
    timer()
    {
        llSetPrimitiveParams( // won't be optimized out because it has side effects
            [ llFrand(0.0)
            , llFrand(-0.0)
            , llFrand(1.4e-45)
            , llFrand(-1.4e-45)
            , llFrand(1.1754942e-38) // denormal - loses 1 bit precision
            , llFrand(1e40)
            , llFrand(-1e40)
            , llFrand(1e40*0)
            , llFrand(-1e40*0)
            , llCloud(<0,1,2>)
            , llAvatarOnLinkSitTarget(256)
            , llEdgeOfWorld(<0,1,2>,<0,0,1>)
            , llGetAgentInfo(".")
            , llGetAgentLanguage("")
            , llGetAgentList(3, [])
            , llGetAgentSize(NULL_KEY)
            , llGetAlpha(9)
            , llGetAnimation("0")
            , llGetAnimationList("")
            , llGetBoundingBox("")
            , llGetColor(9)
            , llGetDisplayName("")
            , llGetEnv("")
            , llGetEnv("yadda")
            ]);
    }
}
