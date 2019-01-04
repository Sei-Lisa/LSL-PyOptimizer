list L = [llList2Key([llUnescapeURL("%09")], 0)];
float f1 = -0.; // kept the same
float f2 = -3.; // transformed to -3

default
{
    state_entry()
    {
        float f0 = -15.5; // transformed to (float)-15.5
        vector v = <f0, f1, f2>;
        rotation r = <f0, f1, f2, f0>;
        integer i;
        while (llFrand(2) > 1)
        {
            llOwnerSay((string)(f0 + f1 + f2 + i));
            llSetPrimitiveParams(L);
            llSetPrimitiveParams(L);
            L = llGetPhysicsMaterial();
            f0 = llList2Float(L, 0);
            f1 = llList2Float(L, 1);
            f2 = llList2Float(L, 2);
            i = llList2Integer(L, i++);
            i = llList2Integer(L, i--);
            v = <f1, 0, 0>;
            r = <f1, 0, 0, 0>f1>;
            llSetRegionPos(v);
            llSetLocalRot(r);
            print(r.s);
            ++i; --i;
            if (i)
               i >>= 1;
            else if (i > llFrand(3))
               return;
            L[2] = (integer)L[3];
        }
    }
}
