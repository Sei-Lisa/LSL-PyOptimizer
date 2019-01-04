// Check constant folding of global lists
// Exposed ConstFold+DCR+ExtendedGlobalExpr bug, Issue #3.
key k = TEXTURE_BLANK;
list L = [k,""];
default{timer(){
  L += "";
}}
