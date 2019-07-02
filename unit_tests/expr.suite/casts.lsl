[ (string)llDetectedKey(999)
, (key)"xyz"
, (integer)"3.14e+0a"
, (integer)"a3.14e+0a"
, (integer)"0XA3.14e+0a"
, (integer)"3333333333333"
, (integer)3333333333333.
, (integer)"4124567890"
, (integer)"-4124567890"
, (integer)"-4294967296"
, (integer)"-4294967295"
, (integer)"-4294967294"
, (integer)"4294967294"
, (integer)"4294967295"
, (integer)"4294967296"
, (integer)" +12345 "
, (integer)" ++12345 "
, (integer)" +-12345 "
, (integer)" + 12345 "
, (integer)" - 12345 "
, (integer)" -+12345 "
, (integer)" --12345 "
, (integer)" -12345 "
, (float)"3.14e+0a"
, (float)"+3.14e+0a"
, (float)"++3.14e+0a"
, (float)"-3.14e+0a"
, (float)"--3.14e+0a"
, (float)"0x3.14p+0a"
, (float)"1.1754944e-38"
, (float)"1.1754943e-38"
, (float)"1.1754942e-38"
, (float)"1.1754943157898258346e-38"
, (float)"1.17549431578982583459e-38"
// Known Mismatch:
//, (float)"1.175494315789825834599e-38" // should give 0 but it fails
, (vector)"<5.31,7.13,0x9.99"
, (vector)"<5.31, 7.13, 0x9.99>"
, (vector)"<5.31 , 7.13 , 0x9.99>"
, (vector)"5.31, 7.13, 0x9.99>"
, (vector)"<5.31, a7.13, 0x9.99>"
, (vector)"<1,1,2+"
, (vector)"<1,1,2a"
, (vector)"<1,1,inf"
, (vector)"<1,1,info"
, (vector)"<1,1,infi"
, (vector)"<1,1,infix"
, (vector)"<1,1,infinite"
, (vector)"<1,1,iNfInItY"
, (vector)"<1,1,infinitys"
, (vector)"<1,1,infinities"
, (vector)"<inf,1,1>"
, (vector)"<infinity,1,1>"
, (vector)"<infini,1,1>"
, (vector)"<infinite,1,1>"
, (vector)"<info,1,1>"
, (quaternion)"<-nan,nan,-nan,nan>"
, (string)-.5e-6
, (string)((float)"-0x1.0C6F7Ap-21")
, (string).5e-6
, (string)((float)"-0x1.0C6F78p-21")
, (string)((float)"0x1.0C6F78p-21")
, (string)<.5e-5,-.5e-5,(float)"-0x1.4F8B58p-18">
, (string)<(float)"0x1.4F8B58p-18",(float)"-0x1.4F8B56p-18",(float)"0x1.4F8B56p-18">
, (string)-123456789.
, (string)-123456784.
, (string)-123456740.
, (string)-12345.674
, (string)-1.2345674
, (string)-1.2345675
, (string)<-123456740., -12345.674, -1.2345674, -1.234564>
, (string)[<-123456750., -12345.675, -12345.676>]
, (string)<-123456750., -12345.675, -12345.676, -12.345675>
, (string)<1234567.5, 1234567.4, 123456.75, 123456.74>
, (string)[<1234567.5, 1234567.4, 0.>]
, (string)<12345675., 12345674., 9.999999>
, (string)1e-7
, (string)1e-6
, (string)1e-5
, (string)1e-4
, (string)1e-3
, (string)1e-2
, (string)1e-1
, (string)1e0
, (string)1e1
, (string)1e2
, (string)1e3
, (string)1e4
, (string)1e5
, (string)1e6
, (string)1e7
, (string)1e8
, (string)1e9
, (string)1e10
, (string)1e11
, (string)1e12
, (string)1e13
, (string)1e14
, (string)1e15
, (string)1e16
, (string)1e17
, (string)1e18
, (string)1e19
, (string)1e20
, (string)1e21
, (string)1e22
, (string)1e23
, (string)1e24
, (string)1e25
, (string)1e26
, (string)1e27
, (string)1e28
, (string)1e29
, (string)1e30
, (string)1e31
, (string)1e32
, (string)1e33
, (string)1e34
, (string)1e35
, (string)1e36
, (string)1e37
, (string)1e38
, (string)1e39
, (string)-1e39
, (string)(1e39*0)
, llList2CSV([1e0,1e1,1e2,1e3,1e4,1e5,1e6,1e7,1e8,1e9,1e10,1e11,1e12,1e13,1e14,
              1e15,1e16,1e17,1e18,1e19,1e20,1e21,1e22,1e23,1e24,1e25,1e26,1e27,
              1e28,1e29,1e30,1e31,1e32,1e33,1e34,1e35,1e36,1e37,1e38])
, llList2CSV((list)<(float)"NaN", 1e40*0, 1e40, -1e40>)
, llList2CSV((list)"str")
, (string)[1,3.14,(key)"blah",<1.,0.,0.,0.>]
, llSin(-2147483648)
, llSin(2147483647)
, llSin(2147483647.0)
, 2147483647 * 1.0 * 2
, 3 * 1.0 / 2
, 3 / 2
]
