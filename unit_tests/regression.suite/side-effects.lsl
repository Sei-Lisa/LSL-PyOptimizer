// Check that the LSL functions with side effects are not computed.
default{timer(){
    llModPow(4, 1, 2);
    llXorBase64Strings("AB", "CD");
}}
