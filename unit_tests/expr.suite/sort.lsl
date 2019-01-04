llListSort([<1.,2.,3.,4.>,<2.,3.,4.,5.>,<5.,4.,3.,2.>,<0.,-1.,-2.,-3.>,
              <4.,3.,2.,1.>,<3.,2.,1.,0.>],1,0)
+ "********"
+ llListSort([<1.,2.,3.,4.>,<2.,3.,4.,5.>,<5.,4.,3.,2.>,<0.,-1.,-2.,-3.>,
              <4.,3.,2.,1.>,<3.,2.,1.,0.>],1,1)
+ "********"
+ llListSort([<1.,0.,0.>,<0.,3.,0.>,<0.,0.,1.>,<3.,0.,0.>],1,1)
+ "********"
+ llListSort([2,0,1,1,2,2,2,3,2,4,1,5,2,6], 2, 1)
+ "********"
+ llListSort([2,0,1,1,2,2,2,3,2,4,1,5,2,6], 2, 0)
+ "********"
+ llListSort([2,0,1,1,2,2,2,3,2,4,1,5,2,6,3], 2, 1)
+ "********"
  // NaN in sort behaves strangely. Also when inside vectors.
+ llListSort([-1., 9., 3., 2., (float)"NaN", 5., 1.],1,1)
+ "********"
+ llListSort([<2.,0.,0.>,<1.,(float)"NaN",0.>],1,1)
+ "********"
+ llListSort([<1.,(float)"NaN",0.>,<2.,0.,0.>],1,1)
+ "********"
+ llListSort([<2.,0.,0.>,<1.,(float)"NaN",0.>],1,0)
+ "********"
+ llListSort([<1.,(float)"NaN",0.>,<2.,0.,0.>],1,0)
+ "********"
  // This proves that it does not sort by UTF-16 words, but by code points.
  // Otherwise u"\U0001d41a" would be before u"\ufb01" (because the UTF-16
  // of u"\U0001d41a" is 0xD835 0xDC1A)
+ llListSort(["ﬁ","á", "𝐚", "a"],1,1)
+ "********"
+ llListSort([2, "B", "C", 3, 1, "A"], 1, TRUE)
+ "********"
+ llListSort([2, "B", "C", 3, 1, "A"], 1, FALSE)
