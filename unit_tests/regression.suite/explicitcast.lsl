default
{
    state_entry()
    {
        key k = llList2String(llGetAgentList(AGENT_LIST_REGION, []), 0);        
        float f = llGetAgentInfo(llGetOwner());
        llOwnerSay((string)[k, f]);
    }
}
