// Test the extra features of the parser, to complete coverage.
string S = "a" "b"; // juxtaposition in globals
f(){}
integer f(){return 1;}
default
{
    timer()
    {
        // String juxtaposition coverage
        "a" "b";

        // Explicit cast and extended cast coverage
        integer i;
        float f;
        list L;
        f += i;
        L += (integer)(float)i;
        i = ~(integer)-2*3;
        i = ~(integer)-2.*3;
        i = ~(integer)-i*3;

        // AllowKeyConcat coverage
        ""+(key)"";
        (key)""+"";

        // Parse_statement with duplicate labels.
        @J;

        // does_something() coverage
        switch(1)
        {
            {1;}
        }

        // loops, switch and break/continue
        while (1) {break;for (;2;) {continue;break;} continue;}
        do { continue; break 1; } while (1);
        jump x;
        while (1) @x;
        jump y;
        for (;1;) @y;
        jump z;
        do @z; while (0);

        switch(1.0)
        {
            case 1: {1;}
        }
        switch(1)
        {
            default {}
        }

        while (1) continue;
        while (1) while (1) while (1) continue 3;
    }
}
