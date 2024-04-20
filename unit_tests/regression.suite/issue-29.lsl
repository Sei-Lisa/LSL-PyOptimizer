default
{
    touch(integer num)
    {
        integer dead = 0;
        if (num == 5)
            dead = 1;
        else
            dead = 2;
        llOwnerSay("Not Dead");
    }
}
