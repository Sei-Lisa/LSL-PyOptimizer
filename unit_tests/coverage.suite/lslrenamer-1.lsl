list L;
float f1 = 5;
float f2 = 5;
float f3 = 5;
float f4 = 5;
float f5 = 5;
float f6 = 5;
float f7 = 5;
float f8 = 5;
float f9 = 5;
float fA = 5;
float fB = 5;
float fC = 5;
float fD = 5;
float fE = 5;
float fF = 5;

// Transformation of function names and arguments
Test(float x, float y)
{
    llParticleSystem([x, y]);
}

default
{
    listen(integer chan, string name, key id, string msg)
    {
        float f0 = 2; // locals not transformed
        while ((integer)llFrand(2))
        {
            llOwnerSay((string)(f0 + f1 + f2 + f3 + f4 + f5 + f6 + f7
                + f8 + f9 + fA + fB + fC + fD + fE + fF));
            L = llGetPhysicsMaterial();
            f0 = llList2Float(L, 0);
            f1 = llList2Float(L, 1);
            f2 = llList2Float(L, 2);
            f3 = llList2Float(L, 3);
            f4 = llList2Float(L, 4);
            f5 = llList2Float(L, 5);
            f6 = llList2Float(L, 6);
            f7 = llList2Float(L, 7);
            f8 = llList2Float(L, 8);
            f9 = llList2Float(L, 9);
            fA = llList2Float(L, 10);
            fB = llList2Float(L, 11);
            fC = llList2Float(L, 12);
            fD = llList2Float(L, 13);
            fE = llList2Float(L, 14);
            fF = llList2Float(L, 15);
            Test(f0, f1);
            if (llFrand(1) < 0.5) jump there;
        }
        @there;
        state TestState;
    }
}

state TestState
{
    // Events with a different internal name
    on_rez(integer i)
    {
        llDie();
    }

    run_time_permissions(integer j)
    {
        llDie();
    }

    remote_data(integer evtype, key cid, key mid, string sender, integer idata, string sdata)
    {
        llDie();
    }
}
