x(integer r)
{
  do state r; while(0);
}

default{touch(integer r){integer r;{@r;state r;}}}

state r
{
  timer(){}
}
