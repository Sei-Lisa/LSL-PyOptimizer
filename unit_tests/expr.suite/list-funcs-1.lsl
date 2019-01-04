llListInsertList([1,2,3],[4,5],-1)
+ "********"
+ llListInsertList([1,2,3],[4,5],-5)
+ "********"
+ llListInsertList([], [1], 0)
+ "********"
+ llListInsertList([], [1], 3)
+ "********"
+ llListInsertList([], [1], -1)
+ "********"
+ llListInsertList([1,2,3,4,5],[9],-3)
+ "********"

+ llList2ListStrided([1,2,3,4,5],0,-1,2)
+ "********"
+ llList2ListStrided([1,2,3,4,5],1,-1,2)
+ "********"
+ llList2ListStrided([1,2,3,4,5],0,-2,2)
+ "********"
+ llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],0,-4,3)
+ "********"
+ llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],0,-3,3)
+ "********"
+ llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],0,-1,3)
+ "********"
+ llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],-3,3,3)
+ "********"
+ llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],-2,-3,3)
+ "********"
+ llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],-2,-2,3)
+ "********"
+ llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],-2,-1,3)
+ "********"
+ llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],4,3,3)
+ "********"
+ llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],4,4,3)
+ "********"
+ llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],4,5,3)
+ "********"
+ llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],5,5,3)
+ "********"
+ llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],5,6,3)
+ "********"
+ llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],6,5,3)
+ "********"
+ llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],6,6,3)
+ "********"
+ llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],6,2,-3)
+ "********"
+ llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],6,2,-2)
+ "********"
+ llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],6,2,-1)
+ "********"
+ llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],6,2,0)
+ "********"
+ llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],6,2,1)
+ "********"
+ llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],6,20,3)
+ "********"

+ llListReplaceList([0,1,2,3,4,5],[6,7],2,3)
+ "********"
+ llListReplaceList([0,1,2,3,4,5],[],2,3)
+ "********"
+ llListReplaceList([0,1,2,3,4,5],[6,7],2,3)
+ "********"
+ llListReplaceList([0,1,2,3,4,5],[6,7],2,-1)
+ "********"
+ llListReplaceList([0,1,2,3,4,5],[],4,1)
+ "********"
+ llListReplaceList([0,1,2,3,4,5],[6,7,8],4,1)
+ "********"
+ llListReplaceList([0,1,2,3,4,5],[6,7,8],6,6)
+ "********"
+ llListReplaceList([0,1,2,3,4,5],[6,7,8],7,6)
+ "********"
+ llListReplaceList([0,1,2,3,4,5],[6,7,8],7,8)
+ "********"

+ llParseString2List("[ 1 ]2|3|4|5", ["|"], ["[ ", " ]"])
+ "********"
+ llParseString2List("[ 1 ]2|3|4|5", ["|"], ["|", "|"])
+ "********"
+ llParseString2List("1abc2ab3abc4",["ab","abc"],[])
+ "********"
+ llParseString2List("1abc2ab3abc4",["abc","ab"],[])
+ "********"
+ llParseString2List("1abc2ab3abc4",[""],[])
+ "********"
+ llParseString2List("1abc2ab3abc4",[],[""])
+ "********"
+ llParseString2List("1bab1", ["a", "bb"], [])
+ "********"
+ llParseString2List("1bab1", [], ["a", "bb"])
+ "********"
+ llParseStringKeepNulls("1abc2ab3abc4",["ab"],["abc"])
+ "********"
+ llParseStringKeepNulls("1abc2ab3abc4",["ab"],["a"])
+ "********"
+ llParseStringKeepNulls("1abc2ab3abc4",["ab"],["ab"])
+ "********"
+ llParseStringKeepNulls("1abc2ab3abc4",[""],[])
+ "********"
+ llParseStringKeepNulls("1abc2ab3abc4",[],[""])
+ "********"
+ llParseStringKeepNulls("1bab1", ["a", "bb"], [])
+ "********"
+ llParseStringKeepNulls("1bab1", [], ["a", "bb"])
+ "********"
+ llParseStringKeepNulls("",[],[])
+ "********"
+ llParseStringKeepNulls("",[],[""])
+ "********"
+ llParseStringKeepNulls("",[""],[])
+ "********"
+ llParseStringKeepNulls("",[""],[""])
+ "********"
+ llParseString2List("",[],[])
+ "********"
+ llParseString2List("",[],[""])
+ "********"
+ llParseString2List("",[""],[])
+ "********"
+ llParseString2List("",[""],[""])
+ "********"
+ llParseStringKeepNulls("a",[""],[])
+ "********"
+ llListReplaceList([0,1,2,3],[5],-5,-4)
+ "********"
+ llListReplaceList([0,1,2,3],[5],-5,-5)
+ "********"
+ llListReplaceList([0,1,2,3],[5],-5,-6)
+ "********"
+ llListReplaceList([0,1,2,3],[5],-5,-7)
+ "********"
+ llGetListLength([])
+ llGetListLength([""])
+ llGetListLength(["",""])
