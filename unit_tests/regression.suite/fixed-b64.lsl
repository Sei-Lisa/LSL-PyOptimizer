// Some Base64 functions used to return garbage, now they no longer do
default
{
    timer()
    {
        llSetPrimitiveParams( // we need a function that causes side effects,
                              // so that it isn't optimized out
            [ llXorBase64StringsCorrect("++++", "?")
            , llXorBase64("++++", "?")
            , llBase64ToInteger("ABC")
            ]);
    }
}
