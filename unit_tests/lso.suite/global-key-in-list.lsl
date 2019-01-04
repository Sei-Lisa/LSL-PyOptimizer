// SCR-295: LSO global list declarations with keys generate string entries
// It's apparently a consequence of SVC-1710 together with the fact that global
// lists reuse the values verbatim, just like type casting to list does.

// TODO: The optim. that'd allow using llGetListEntryType is not implemented.
// (When a list/vector/rot. variable is only read and not written, use the
// constant value to call the function, rather than the variable, but careful
// with llList2List etc.: ensure that the optimized result isn't bigger than
// the unoptimized input)

key k = TEXTURE_BLANK;
list L = [k,""];
default{timer(){
  llOwnerSay((string)llGetListEntryType(L, 0));
  llOwnerSay((string)llListFindList(L, (list)k)); // in LSO, this says TYPE_STRING
}}
