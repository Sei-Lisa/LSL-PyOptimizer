// Jump to infinite loop in same scope
default{timer(){
  jump a1;
  while (0) @a1;
  jump a2;
  for (;0;) @a2;
  jump a3;
  for (llDie();0;) @a3;
  jump a4;
  for (;0;llDie()) @a4; // should remain (this executes the llDie() call)
  jump b1;
  while (1) @b1;
  jump b2;
  for (;1;) @b2;
  jump b3;
  for (llDie();1;) @b3;
  jump b4;
  for (;1;llDie()) @b4;
  jump c1;
  if (0) @c1; else ;
  jump c2;
  if (0) ; else @c2;
  jump c3;
  if (1) @c3; else ;
  jump c4;
  if (1) ; else @c4;
  jump c5;
  if (0) @c5; else llDie();
  jump c6;
  if (0) llDie(); else @c6;
  jump c7;
  if (1) @c7; else llDie();
  jump c8;
  if (1) llDie(); else @c8;
  jump c9;
  if (0) @c9;
  jump c10;
  if (1) @c10;
  jump d1;
  do @d1; while (1);
  jump d2;
  do @d2; while (0);
}}
