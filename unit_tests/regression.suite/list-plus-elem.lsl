default{timer(){

  list a = llGetPhysicsMaterial();
  integer b = llGetLinkNumber();
  a += 1;
  a += b;
  a += (list)1;
  a += (list)b;
  a += [1];
  a += [b];
  a += (list)(list)(list)(list)(list)[1];
  a += (list)(list)(list)(list)(list)[b];
  a += (list)(list)(list)(list)(list)1;
  a += (list)(list)(list)(list)(list)b;

  a = b + a;
  a = 1 + a;
  a = (list)b + a;
  a = (list)1 + a;
  a = [b] + a;
  a = [1] + a;
  a = (list)(list)(list)(list)(list)[1] + a;
  a = (list)(list)(list)(list)(list)[b] + a;
  a = (list)(list)(list)(list)(list)1 + a;
  a = (list)(list)(list)(list)(list)b + a;
  a += (llGetPhysicsMaterial() + 0);
  a += (llGetPhysicsMaterial() + 0.);
  a += (llGetPhysicsMaterial() + "");
  a += (llGetPhysicsMaterial() + []);
  a += (0 + llGetPhysicsMaterial());
  a += (0. + llGetPhysicsMaterial());
  a += ("" + llGetPhysicsMaterial());
  a += ([] + llGetPhysicsMaterial());
  a += ([] + llGetPhysicsMaterial() + []);
  a += (0 + []);
  a += (0. + []);
  a += ("" + []);
  a += ([] + 0);
  a += ([] + 0.);
  a += ([] + "");
  a += ([] + []);
  llParticleSystem(a);
}}
