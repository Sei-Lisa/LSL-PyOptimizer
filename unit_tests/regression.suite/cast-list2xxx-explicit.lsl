default{timer(){

list L = llDeleteSubList(llGetPhysicsMaterial(), 0, -1)
         + [3];

// this should produce (key)"3" -> can't be switched to llList2Key
key a = llList2String(L, 0);

// this should produce "" -> can't be switched to llList2String
string b = llList2Key(L, 0);

llParticleSystem([a,b]);

}}
