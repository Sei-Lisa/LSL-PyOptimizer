default{state_entry(){

    if (llGetLinkNumber()) jump x1;
    if (llGetLinkNumber()) jump x1; else jump x3;

    llOwnerSay("ok");
    @x1;
    jump x2;
    @x3;
    llOwnerSay("blergh");
    @x2;

}}