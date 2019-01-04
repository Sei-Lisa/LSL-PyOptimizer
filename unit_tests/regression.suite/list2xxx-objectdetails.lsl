default{timer(){

    llParticleSystem([ llList2String(llGetObjectDetails(llGetKey(), (list)OBJECT_DESC), 0)
                     , llList2Integer(llGetObjectDetails(llGetKey(), (list)OBJECT_DESC), 0)
                     , llList2Key(llGetObjectDetails(llGetKey(), (list)OBJECT_DESC), 0)
                     , llList2Key(llGetObjectDetails(llGetKey(), (list)OBJECT_OWNER), 0)
                     , llList2Key(llGetObjectDetails(llGetKey(), (list)OBJECT_OWNER), -1)
                     , llList2Key(llGetObjectDetails(llGetKey(), (list)OBJECT_OWNER), -2)
                     , llList2Key(llGetObjectDetails(llGetKey(), [OBJECT_OWNER]), 0)
                     // floats can't be safely extracted with (integer)((string)...)
                     , llList2Integer(llGetObjectDetails(llGetKey(), [OBJECT_BODY_SHAPE_TYPE]), 0)
                     , llList2Float(llGetObjectDetails(llGetKey(), [OBJECT_BODY_SHAPE_TYPE]), 0)
                     // but integers and strings can
                     , llList2Integer(llGetObjectDetails(llGetKey(), [OBJECT_PHYSICS]), 0)
                     , llList2Float(llGetObjectDetails(llGetKey(), [OBJECT_PHYSICS]), 0)
                     , llList2Integer(llGetObjectDetails(llGetKey(), [OBJECT_NAME]), 0)
                     , llList2Float(llGetObjectDetails(llGetKey(), [OBJECT_NAME]), 0)
                     , "****"

                     , llList2Key(llGetObjectDetails(llGetKey(), [OBJECT_PHYSICS, OBJECT_GROUP_TAG]), 0)
                     , llList2Key(llGetObjectDetails(llGetKey(), [OBJECT_PHYSICS, OBJECT_GROUP_TAG]), 1)
                     , llList2Key(llGetObjectDetails(llGetKey(), [OBJECT_GROUP_TAG, OBJECT_PHYSICS]), 0)
                     , llList2Key(llGetObjectDetails(llGetKey(), [OBJECT_GROUP_TAG, OBJECT_PHYSICS]), 1)
                     , llList2Key(llGetObjectDetails(llGetKey(), [OBJECT_OWNER, OBJECT_OWNER]), 2)
                     , llList2Integer(llGetObjectDetails(llGetKey(), (list)OBJECT_OWNER), 0)
                     ]);

}}
