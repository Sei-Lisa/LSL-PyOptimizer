default{timer(){

integer x = 1;
integer y = x << 31;
integer z = (integer)-0x80000000;

llOwnerSay((string)y);
z += 1;
y += 1;
x += 1;

}}
