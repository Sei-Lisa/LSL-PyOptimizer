// Check that the LSL functions with side effects are not computed.
default{timer(){
    llXorBase64Strings("AB", "CD");
}}
