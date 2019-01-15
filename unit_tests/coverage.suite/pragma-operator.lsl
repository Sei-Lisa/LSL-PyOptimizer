_Pragma("OPT inline")

string f(integer x) inline
{
    return "ok";
}

default
{
    state_entry()
    {
        llOwnerSay(f(5));
    }
}