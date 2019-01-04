// Check constant folding of global lists
default{timer(){

  key k = TEXTURE_BLANK;
  list L = [k,""];
  llBreakLink(llListFindList(L, (list)k));

}}
