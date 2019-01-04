integer a;
list b = [<a,0,0,1>];

default
{
    timer()
    {
        llSetPrimitiveParams(b + b);
    }
}
