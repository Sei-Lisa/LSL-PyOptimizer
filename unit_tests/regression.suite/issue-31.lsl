default{timer(){
  integer ia = llGetUnixTime();
  integer ib = llGetUnixTime();
  integer ic = -5;
  integer id = ia*2;
  integer ie = -ic*ia;
  float fa = llGetTime();
  float fb = llGetTime();
  float fc = -5.5;
  float fd = fa*2;
  float fe = -fc*fa;
  // FIXME: This is broken in the output module.
  // The output should parenthesize the first two factors in the products,
  // just as they are in the source. Fortunately they are equivalent.
  // FIXME: The output is not optimal.
  // It would suffice to add a space without parenthesizing the --ia,
  // just like in the source.
  llOwnerSay((string)[(- --ia*ib)*ib, (-++ia*ib)*ib, --ia*ib, ie]);
  llOwnerSay((string)[(- --fa*fb)*fb, (-++fa*fb)*fb, --fa*fb, fe]);
}}
