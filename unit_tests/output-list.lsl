string QuoteString(string s)
{
    s = llDumpList2String(llParseStringKeepNulls(s, ["\\"], []), "\\\\");
    s = llDumpList2String(llParseStringKeepNulls(s, ["\n"], []), "\\n");
    s = "\"" + llDumpList2String(llParseStringKeepNulls(s, ["\""], []), "\\\"") + "\"";
    return s;
}


string DumpFloat(float f)
{
    string s = llList2CSV((list)f);
    if (s == "inf")
        return "1e40";
    if (s == "-inf")
        return "((float)-1e40)";
    if (s == "nan")
        return "((float)\"NaN\")";
    if (s == "-nan")
        return "(1e40*0)";
    if (f == 0)
        return llGetSubString(s, 0, -7);

    integer i = -1;
    while(llGetSubString(s, i, i) == "0")
        --i;
    return llGetSubString(s, 0, i);
}

string AddLine(string lines, string lineToAdd)
{
    if (llStringLength(llStringToBase64(lines + "\n\t, " + lineToAdd)) > 1364)
    {
        llSay(0, lines);
        lines = "\n\t, " + lineToAdd;
        llSleep(1);
    }
    else
    {
        if (lines == "")
            lines = "\n\t[ " + lineToAdd;
        else
            lines = lines + "\n\t, " + lineToAdd;
    }
    return lines;
}

DumpListLSL(list L)
{
    string ret = "";
    integer len = llGetListLength(L);
    integer i;
    for (i = 0; i < len; ++i)
    {
        integer typ = llGetListEntryType(L, i);
        if (typ==TYPE_KEY|typ==TYPE_STRING)
        {
            if (typ==TYPE_KEY)
                ret = AddLine(ret, "((key)" + QuoteString(llList2String(L, i)) + ")");
            else
                ret = AddLine(ret, QuoteString(llList2String(L, i)));
        }
        else if (typ==TYPE_FLOAT|typ==TYPE_VECTOR|typ==TYPE_ROTATION)
        {
            if (typ == TYPE_FLOAT)
                ret = AddLine(ret, DumpFloat(llList2Float(L, i)));
            else if (typ == TYPE_VECTOR)
            {
                vector v = llList2Vector(L, i);
                ret = AddLine(ret, "<" + DumpFloat(v.x) + ", " + DumpFloat(v.y) + ", " + DumpFloat(v.z) + ">");
            }
            else
            {
                rotation v = llList2Rot(L, i);
                ret = AddLine(ret, "<" + DumpFloat(v.x) + ", " + DumpFloat(v.y) + ", " + DumpFloat(v.z) + ", " + DumpFloat(v.s) + ">");
            }
        }
        else
            ret = AddLine(ret, llList2CSV(llList2List(L, i, i)));
    }
    if (ret == "") ret = "\n\t[";
    llSay(0, ret + "\n\t]");
}

default
{
    state_entry()
    {
        DumpListLSL(

// Put the list to dump here:
[]

        );
    }
}
