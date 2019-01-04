f1() inline
{
    llOwnerSay("f1");
    f2();
}

f2() inline
{
    llOwnerSay("f2");
    f1();
}

default
{
    state_entry()
    {
        f1();
    }
}
