default
{
    state_entry()
    {
        list L = llGetPhysicsMaterial();
        for(L[1];llFrand(2)<1;);
    }
}
