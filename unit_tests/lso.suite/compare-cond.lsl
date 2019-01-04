default
{
    state_entry()
    {
        if ([]) llOwnerSay("[]");
        if ([""]) llOwnerSay("[\"\"]");
        if (["",""]) llOwnerSay("[\"\",\"\"]");
        llOwnerSay((string)("a"!="b"));
    }
}
