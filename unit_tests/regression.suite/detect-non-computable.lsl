// Test functions in a detection event that can't be precomputed.
default
{
    touch(integer n)
    {
        llSetPrimitiveParams(
            [ llDetectedGrab(0)
            , llDetectedGroup(0)
            , llDetectedKey(0)
            , llDetectedLinkNumber(0)
            , llDetectedName(0)
            , llDetectedOwner(0)
            , llDetectedPos(0)
            , llDetectedRot(0)
            , llDetectedTouchBinormal(0)
            , llDetectedTouchFace(0)
            , llDetectedTouchNormal(0)
            , llDetectedTouchPos(0)
            , llDetectedTouchST(0)
            , llDetectedTouchUV(0)
            , llDetectedType(0)
            , llDetectedVel(0)
            ]);
    }
}
