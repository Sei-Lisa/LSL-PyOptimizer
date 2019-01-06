f1() inline
{
    llOwnerSay("f1");
}

f2(integer f2param) inline
{
    llOwnerSay("f2:" + (string)f2param);
}

vector f3(integer f3p1, string f3p2) inline
{
    f2(f3p1);
    integer f3p1; // test shading the parameter
    {
        jump x;
        llOwnerSay("f3:" + (string)f3p1 + f3p2);
    }
    @x;
    if (f3p2 != "") return <1,1,1>;

    do ; while (f4());

    while (f4()) ;

    for (f3p1=0; f4(); f3p1++, llDie())
    {
       integer f3p1 = llGetNumberOfPrims();
       llOwnerSay((string)f3p1);
    }

    return <0,0,0>;
}

integer f4() inline
{
    return llGetLinkNumber();
}

say(string s) inline
{
    llOwnerSay(s);
}

default
{
    state_entry()
    {
        f1();
        if (1) f1();
        f2(3);
        if (f3(4, "x") == ZERO_VECTOR) llOwnerSay("ok");
    }

    timer()
    {
        say("hi");
    }
}
