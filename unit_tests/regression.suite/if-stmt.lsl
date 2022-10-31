default{timer(){

integer a = (integer)llFrand(100);

// Check optimization of if

if (a) ;
if (a) a;
if (a) a; else a;
if (a) {@b;} else {@c;}
//if (a) @d;
if (a) llDie();
if (a) ; else llDie();
if ((float)a) ; else llDie();
if ((string)a) ; else llDie();
if ((key)((string)a)) ; else llDie();
if (<a,0,0>) ; else llDie();
if (<a,0,0,1>) ; else llDie();
if ((list)a) ; else llDie();
if (!a) llDie(); else llOwnerSay("1");
if (a == 3) llDie(); else llOwnerSay("2");
if (a > 5) if (a == 12) ; else llDie(); else llOwnerSay("3");
if (a == 12) llOwnerSay("4"); else if (a > 5) llOwnerSay("5");
if (a > 5) if (a == 12) ; else /*@f1*/; else llDie();
if (a == 12) llDie(); else if (a > 5) /*@f4*/;
// Fixed: Regression: this produces if (!(a == 3)) and no optimization kicks in
if (a != 3) llOwnerSay("fixed");

// Regression
if (a == 2)
    llOwnerSay("a");
else if (a)
    llOwnerSay("b");
else if (a == 3)
    llOwnerSay("c");

}}
