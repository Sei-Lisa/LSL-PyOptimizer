x(){
    // All these should work
    if (1)
        return llDie();
    while (1)
        return llDie();
    do
        return llDie();
    while (1);
    for (;1;)
        return llDie();
    if (1)
    {
        return llDie();
        if (1) return llDie(); else return llDie();
        return llDie();
    }
}

default
{
    state_entry()
    {
        // Similarly, all these should work
        if (1)
            return llDie();
        while (1)
            return llDie();
        do
            return llDie();
        while (1);
        for (;1;)
            return llDie();
        if (1)
        {
            return llDie();
            if (1) return llDie(); else return llDie();
            return llDie();
        }
    }
}
