default{touch(integer n){

if (llSameGroup(llGetOwner()) && llDetectedGroup(0)) llDie();

// TODO
// llGetEnergy() has min=0 and max=1, therefore (integer)llGetEnergy() is bool,
// however we don't handle it yet (needs min and max applied to expressions).
if ((integer)llGetEnergy() && llSameGroup(llGetOwner())) llDie();

// Check that min and max work as they should. This is always true.
if (llGetNumberOfPrims()) llDie();

}}
