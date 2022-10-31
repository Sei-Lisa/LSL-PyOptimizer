integer FOO=1;
integer BAR=FOO;
integer BAZ=BAR;

integer FEZ=2;
list L = [1,FEZ];

integer FUZ=3;

default {
  state_entry() {
    llOwnerSay((string)BAZ+(string)L+(string)FUZ);
  }
}
