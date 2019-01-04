// Test functions inside and outside a detection event that can be precomputed.
default
{
    // non-detection event
    state_entry()
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

    // non-touch event
    collision_start(integer n)
    {
        llSetPrimitiveParams(
            [ llDetectedGrab(0)
            //, llDetectedGroup(0)
            //, llDetectedKey(0)
            //, llDetectedLinkNumber(0)
            //, llDetectedName(0)
            //, llDetectedOwner(0)
            //, llDetectedPos(0)
            //, llDetectedRot(0)
            , llDetectedTouchBinormal(0)
            , llDetectedTouchFace(0)
            , llDetectedTouchNormal(0)
            , llDetectedTouchPos(0)
            , llDetectedTouchST(0)
            , llDetectedTouchUV(0)
            //, llDetectedType(0)
            //, llDetectedVel(0)
            ]);
    }

    // touch event but not touch()
    touch_start(integer n)
    {
        llSetPrimitiveParams(
            [ llDetectedGrab(0) // only works in touch()
            , llDetectedGrab(-1) // only works in touch()
            , llDetectedGroup(16)
            , llDetectedKey(-1)
            , llDetectedLinkNumber(17)
            , llDetectedName(-1)
            , llDetectedOwner(32)
            , llDetectedPos(-1)
            , llDetectedRot(-1)
            , llDetectedTouchBinormal(-1)
            , llDetectedTouchFace(-1)
            , llDetectedTouchNormal(-1)
            , llDetectedTouchPos(-1)
            , llDetectedTouchST(-1)
            , llDetectedTouchUV(-1)
            , llDetectedType(-1)
            , llDetectedVel(-1)
            ]);
    }

    touch(integer n)
    {
        llSetPrimitiveParams(
            [ llDetectedGrab(-1)
            ]);
    }
}
