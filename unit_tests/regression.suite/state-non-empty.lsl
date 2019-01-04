default
{
    state_entry()
    {
        if (llFrand(1) > 0.5) state s2;
        if (llFrand(1) > 0.5) state s3;
        if (llFrand(1) > 0.5) state s4;
    }
}

state s2
{
    timer()
    {
    }
}

state s3
{
    state_entry()
    {
    }

    state_exit()
    {
    }
}

state s4
{
    touch(integer n)
    {
    }
}
