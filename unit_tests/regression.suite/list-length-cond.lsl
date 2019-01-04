default{timer(){

list L = llGetPhysicsMaterial();
integer i = llSameGroup(llGetOwner());

if (llGetListLength(L) < 2 || i)
  llDie();
if ((L != []) < 2 || i)
  llDie();

}}
