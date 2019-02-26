integer x() inline
{
    return 3;
}

default
{
    touch(integer n)
    {
        x();
        n++;
    }
}
