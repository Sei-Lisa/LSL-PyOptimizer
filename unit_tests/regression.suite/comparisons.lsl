default{timer(){

integer a = (integer)llFrand(100);
integer b = (integer)llFrand(100);
llParticleSystem([
!(a - 1),
!(a ^ 1),
!(a != 1),
!(a == 2),
!(a - 3),
!(a + -3),
!(a + 3),
!(a + -b),
!(-a + b),
!(a + b),
-a == -b,
""]);

}}