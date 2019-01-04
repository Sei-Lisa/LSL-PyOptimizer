default{touch(integer llList2Float){

integer llList2String = llGetNumberOfPrims();

rotation q = <llList2Float, llList2String, llList2Float, llList2String>;

list L = llGetPhysicsMaterial();
llParticleSystem([llList2Float(L, 0), llList2String(L, 1), q]);

}}
