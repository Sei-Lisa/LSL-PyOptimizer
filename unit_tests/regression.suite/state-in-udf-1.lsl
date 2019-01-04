x()
{
    if (1)
    {
        state default;
        do
            state default;
        while (0);
        if (1)
            state default;
        if (1)
            ;
        else
            state default;
    }
}

default{timer(){
  if (1) ; else state default;
}}
