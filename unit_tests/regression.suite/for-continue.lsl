// Check that 'continue' executes the updater expression(s) in for loops

integer x;

f() inline
{
    ++x;
}

default
{
    timer()
    {
        for (x = 1; x < 5; f())
        {
            if (x == 3)
                continue; // still executes f() but not llOwnerSay()
            llOwnerSay((string)x);
        }
    }
}
