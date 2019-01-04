// Check that expressions that don't return a value are allowed in for()
default{timer(){
    for(llOwnerSay("x"),llDie(); 1; llOwnerSay("x"),llDie())
        ;
}}
