default
{
    timer()
    {
        llModPow(5, 7, 3);
        llCos(5);
    }

    // this one should be kept, as it's not marked as SEF in the test data
    no_sensor()
    {
    }

    // this one should disappear
    touch(integer k)
    {
    }
}
