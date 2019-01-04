// Jump to infinite loop in same scope
default{timer(){
  jump a1;
  while (0) @a1;
  jump a2;
  for (;0;) @a2;
  jump a3;
  for (llDie();0;) @a3;
  jump a4;
  for (;0;llDie()) @a4;
  jump b1;
  while (1) @b1;
  jump b2;
  for (;1;) @b2;
  jump b3;
  for (llDie();1;) @b3;
  jump b4;
  for (;1;llDie()) @b4;
}}
