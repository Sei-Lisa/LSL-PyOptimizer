default{timer(){

list a = llGetPhysicsMaterial() + [1,2,3] + [4,5]
            + ([6,7] + [8,llGetLinkNumber()]); // this makes it not optimal :(

if (llGetLinkNumber())
  a += ["x", "y"];

a += [];

llSetPrimitiveParams(a);
// CONST node (always SEF)
llSetPrimitiveParams([1,2,3]);
// SEF LIST node
llSetPrimitiveParams([1,2,llGetLinkNumber()]);
// non-SEF list
llSetPrimitiveParams([1,2,llSetRegionPos(<0,0,0>)]);
// non-SEF + SEF
llSetPrimitiveParams([llSetRegionPos(<0,0,0>), 0] + [1,2,3]);
// this is special because the first list has 1 element
// so it's turned into element + list
llSetPrimitiveParams([llSetRegionPos(<0,0,0>)] + [0,1,2,3]);

}}