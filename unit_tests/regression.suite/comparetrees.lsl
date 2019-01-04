default{timer(){

// Stable function
if (llGetPermissions() & 2 || llGetPermissions() & 4) llDie();

// Unstable function
if (llGetUsedMemory() & 2 || llGetUsedMemory() & 4) llDie();

// Stable function with typecast
if ((integer)llGetTime() == (integer)llGetTime()) llDie();
if ((integer)llGetTime() != (integer)llGetTime()) llDie();

// Unstable function with typecast
if ((integer)llFrand(2) == (integer)llFrand(2)) llDie();

}}
