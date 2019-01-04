default{timer(){

list L = llDeleteSubList(llGetPhysicsMaterial(), 0, 99999)
         + [0.9999997, 0.000111222, 2, 2147483647];

// this returns 1, but llList2Integer returns 0
integer a = (integer)llList2String(L, 0);

// this returns 0.000111, but llList2Float returns 0.000111222
float b = (float)llList2String(L, 1);

// test consistency
string c = (string)llList2String(L, 0);

// this returns (key)"2", but llList2Key returns (key)""
key d = (key)llList2String(L, 2);

// same as above, the typecast is implicit
key e = llList2String(L, 2);

// this returns 0, but llList2Float would return 0.000111222
float f = (float)llList2Integer(L, 1);

// this returns -2147483648, but llList2Integer returns 2147483647
integer g = (integer)llList2Float(L, 3);

llParticleSystem([a,b,c,d,e]);

}}
