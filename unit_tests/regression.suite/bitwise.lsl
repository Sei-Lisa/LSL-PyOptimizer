default{timer(){
    integer a = llGetUnixTime();

    llBreakLink(a & 3 | a & 5);
    llBreakLink((a | 3) & (a | 5));
    llBreakLink(a | (a & 5));
    llBreakLink(llGetAgentInfo(llGetOwner()) | (llGetAgentInfo(llGetOwner()) & 5));
    llBreakLink(llGetAgentInfo(llGetOwner()) & 3 | llGetAgentInfo(llGetOwner()) & 5);
    llBreakLink(llGetAgentInfo(llGetOwner()) & 2 && llGetAgentInfo(llGetOwner()) & 4);
}}
