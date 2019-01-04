[ llListFindList([1e40*0], [1e40*0]) // finds it...
, llListFindList([-1e40*0], [-1e40*0])
, llListFindList([1e40*0, -1e40*0], [-1e40*0, 1e40*0])
, llListFindList([-0.], [0.])
, llListFindList([0.], [-0.])
, llListFindList([1, 1e40*0, 1., 1e40*0], [1., 1e40*0])
, llListFindList([1, 1e40*0, 1., 1e40*0], [2.])
, llListFindList([<1e40*0,0.,0.>], [<1e40*0,0.,0.>]) // ... but this doesnt?!
, llListFindList([<0.,0.,0.>], [<0.,0.,0.>])
, llListFindList([<0.,0.,0.,1e40*0>], [<0.,0.,0.,1e40*0>])
, llListFindList([<0.,0.,0.,-1e40*0>], [<0.,0.,0.,-1e40*0>])
, llListFindList([(key)"12345678-ABCD-5678-1234-123456781234"],
                 [(key)"12345678-abcd-5678-1234-123456781234"]) // case differs
, llListFindList([(key)"12345678-abcd-5678-1234-123456781234"],
                 [(key)"12345678-abcd-5678-1234-123456781234"]) // identical
, llListFindList(["12345678-abcd-5678-1234-123456781234",
                  (key)"12345678-abcd-5678-1234-123456781234"],
                 [(key)"12345678-abcd-5678-1234-123456781234"])
, llListFindList([], [""])
]

