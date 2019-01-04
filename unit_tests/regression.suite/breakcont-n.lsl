//Sei's test break/continue
default
{
    state_entry()
    {
        integer i = 0;
        while (i < 10)
        {
            integer j = 0;
            while (j < 10)
            {
                if (j == 5)
                    break 2;
                if (j == 6)
                    continue 2;
                if (j == 7)
                    break;
                if (j == 8)
                    continue;
                ++j;
            }
            ++i;
            if (llFrand(5) < 3)
                break;
        }
    }
}
