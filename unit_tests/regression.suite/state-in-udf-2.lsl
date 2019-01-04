x()
{
    // FIXME: When optimizing this, a naked 'state default;' is left.
    // That is forbidden.
    do
    {
        state default;
        do
            state default;
        while (0);
        if (1)
            state default;
        if (1)
            ;
        else
            state default;
    }
    while (0);
}

default{timer(){x();}}
