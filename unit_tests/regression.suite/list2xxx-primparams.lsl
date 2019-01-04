default{timer(){

    llParticleSystem(
        [ llList2String(llGetPrimitiveParams((list)PRIM_DESC), 0)
        , llList2Integer(llGetLinkPrimitiveParams(llGetLinkNumber(), [PRIM_DESC]), -1)
        , llList2Integer(llGetLinkPrimitiveParams(llGetLinkNumber(), [PRIM_DESC]), 2)
        , llList2Key(llGetPrimitiveParams((list)PRIM_DESC), 0)
        , llList2Key(llGetLinkPrimitiveParams(LINK_THIS, (list)PRIM_DESC), 0)
        , llList2Key(llGetLinkPrimitiveParams(LINK_THIS, (list)PRIM_DESC), -1)
        , llList2Key(llGetLinkPrimitiveParams(LINK_THIS, (list)PRIM_DESC), -2)
        , llList2Key(llGetLinkPrimitiveParams(LINK_THIS, [PRIM_DESC]), 0)
        , llList2Key(llGetLinkPrimitiveParams(LINK_THIS, [PRIM_SIZE]), 0)
        , llList2Float(llGetLinkPrimitiveParams(LINK_THIS, [PRIM_PHYSICS]), 0)
        , llList2Float(llGetLinkPrimitiveParams(LINK_THIS, [PRIM_DESC]), 0)

        , "*****"

        , llList2Key(llGetPrimitiveParams([PRIM_PHYSICS, PRIM_NAME]), 0)
        , llList2Key(llGetPrimitiveParams([PRIM_PHYSICS, PRIM_NAME]), 1)
        , llList2Key(llGetPrimitiveParams([PRIM_NAME, PRIM_PHYSICS]), 0)
        , llList2Key(llGetPrimitiveParams([PRIM_NAME, PRIM_PHYSICS]), 1)
        , llList2Key(llGetLinkPrimitiveParams(LINK_THIS, [PRIM_PHYSICS, PRIM_NAME]), 0)
        , llList2Key(llGetLinkPrimitiveParams(LINK_THIS, [PRIM_PHYSICS, PRIM_NAME]), 1)
        , llList2Key(llGetLinkPrimitiveParams(LINK_THIS, [PRIM_NAME, PRIM_PHYSICS]), 0)
        , llList2Key(llGetLinkPrimitiveParams(LINK_THIS, [PRIM_NAME, PRIM_PHYSICS]), 1)
        , llList2Integer(llGetLinkPrimitiveParams(LINK_THIS, (list)PRIM_SIZE), 0)

        // with PRIM_GLOW + face
        , llList2Vector(llGetLinkPrimitiveParams(LINK_THIS, [PRIM_SIZE, PRIM_NAME, PRIM_GLOW, 1]), 1)
        // Before PRIM_TYPE
        , llList2Vector(llGetLinkPrimitiveParams(LINK_THIS, [PRIM_SIZE, PRIM_NAME, PRIM_TYPE]), 1)
        // After PRIM_TYPE
        , llList2Vector(llGetLinkPrimitiveParams(LINK_THIS, [PRIM_TYPE, PRIM_SIZE, PRIM_NAME]), 2)
        // Negative, before PRIM_TYPE
        , llList2Vector(llGetLinkPrimitiveParams(LINK_THIS, [PRIM_SIZE, PRIM_NAME, PRIM_TYPE]), -2)
        // Negative, after PRIM_TYPE
        , llList2Vector(llGetLinkPrimitiveParams(LINK_THIS, [PRIM_TYPE, PRIM_SIZE, PRIM_NAME]), -1)
        ]);

}}
