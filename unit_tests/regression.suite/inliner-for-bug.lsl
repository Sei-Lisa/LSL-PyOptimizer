default
{
    state_entry()
    {
        for (; llSetRegionPos(<0,0,2>); llSleep(3))
        {
            llSleep(4);
        }
    }
}
