#define USE_SWITCHES
#define USE_LAZY_LISTS
default
{
    state_entry()
    {
        list a;
        a[1] = 0;
        switch(llGetListLength(a))
        {
          case 1:
            llOwnerSay("1");
            break;
          default:
            llOwnerSay("other");
        }
    }
}
