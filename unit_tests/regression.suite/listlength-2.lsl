default
{
    timer()
    {
        list L = llGetAgentList(AGENT_LIST_REGION, []);
        llOwnerSay((string)llGetListLength(L));
    }
}
