default{timer(){

    integer x = llGetNumberOfPrims();
    integer y = llGetUnixTime();
    llOwnerSay((string)(x * 1.0 / y));
    llOwnerSay((string)(x / 1.0 / y));
    llOwnerSay((string)(x / (-1.0) / y));
    llOwnerSay((string)(x * 1.0 / y));
    llOwnerSay((string)(x * (-1.0) / y));
    llOwnerSay((string)(x * -2.0));
    llOwnerSay((string)(x * 0.0 + y));
    llOwnerSay((string)(x + 0.0 + y));

}}
