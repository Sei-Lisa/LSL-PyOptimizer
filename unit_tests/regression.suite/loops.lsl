default{timer(){

  jump a;
  while (1) @a;

  jump b;
  for (1;1;1) @b;

  jump c;
  @c;

  jump d;
  do @d; while(1);

  @dummy1;

  jump e;
  if (0) @e;

  jump f;
  if (1) @f;

  jump g;
  if (1) ; else @g;

  jump h;
  if (0) ; else @h;

  jump i;
  @i;
  llOwnerSay("blah");
}}
