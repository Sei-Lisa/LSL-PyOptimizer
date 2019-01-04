default{touch(integer n){

if (llSameGroup(llGetOwner()) && llDetectedGroup(0)) llDie();

// TODO
// llGetEnergy() has min=0 and max=1, therefore (integer)llGetEnergy() is bool,
// however we don't yet handle it.
if ((integer)llGetEnergy() && llSameGroup(llGetOwner())) llDie();

}}
