default
{
    timer()
    {
        if ("") llOwnerSay("\"\"");
        if ("a") llOwnerSay("\"a\"");
        if ("ab") llOwnerSay("\"ab\"");
        if ((key)"") llOwnerSay("(key)\"\"");
        if ((key)NULL_KEY) llOwnerSay("(key)NULL_KEY");
        if ((key)TEXTURE_BLANK) llOwnerSay("(key)TEXTURE_BLANK");
        // check also upper-case key
        if ((key)"ABCDEFAB-ABCD-ABCD-ABCD-ABCDEFABCDEF") llOwnerSay("(key)\"ABCDEFAB-ABCD-ABCD-ABCD-ABCDEFABCDEF\"");
        if (<0,0,0>) llOwnerSay("<0,0,0>");
        if (<1,0,0>) llOwnerSay("<1,0,0>");
        if (<1e40*0,1e40*0,1e40*0>) llOwnerSay("<NaN,NaN,NaN>");
        if (<0,0,0,1>) llOwnerSay("<0,0,0,1>");
        if (<0,0,0,-1>) llOwnerSay("<0,0,0,-1>");
        if (<0,0,0,0>) llOwnerSay("<0,0,0,0>");
        if (<0,0,1,0>) llOwnerSay("<0,0,1,0>");
        if (3) llOwnerSay("3");
        if (0) llOwnerSay("0");
        if (3.) llOwnerSay("3.");
        if (0.) llOwnerSay("0.");
        if (1e40) llOwnerSay("inf");
        if (1e40*0) llOwnerSay("NaN");
        if ([]) llOwnerSay("[]");
        if ([""]) llOwnerSay("[\"\"]");
        if (["",""]) llOwnerSay("[\"\",\"\"]");
        if ((integer)llFrand(1)+1) llOwnerSay("(integer)llFrand(1)+1");
        if (llFrand(1)+1) llOwnerSay("llFrand(1)+1");
        if ((string)llFrand(1)) llOwnerSay("(string)llFrand(1)");
        if ((key)((string)llFrand(1))) llOwnerSay("(key)((string)llFrand(1))");
        if (<llFrand(1)+1,0,0>) llOwnerSay("<llFrand(1)+1,0,0>");
        if (<llFrand(1)+1,0,0,1>) llOwnerSay("<llFrand(1)+1,0,0,1>");
        if ([llFrand(1)]) llOwnerSay("[llFrand(1)]");

        do llDie(); while (0);
        do llDie(); while (0.);
        do llDie(); while ("");
        do llDie(); while ((key)"");
        do llDie(); while (<0,0,0>);
        do llDie(); while ([]);
        do llDie(); while (<0,0,0,1>);
        do llDie(); while (<0,0,0,-1>);
    }
}
