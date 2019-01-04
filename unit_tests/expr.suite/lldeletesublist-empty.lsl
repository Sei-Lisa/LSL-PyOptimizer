[ llGetListLength(llDeleteSubList([llSetRegionPos(<0,0,0>)], 0, -1))
, llGetListLength(llListReplaceList([llSetRegionPos(<0,0,0>)], [], 0, -1))
, llGetListLength(llDeleteSubList(llGetLinkPrimitiveParams(LINK_THIS,[1,2,3]), 0, -1))
, llGetListLength(llListReplaceList(
    llGetLinkPrimitiveParams(LINK_THIS,[1,2,3]), [], 0, -1
  ))
, llGetListLength(llDeleteSubList(llGetLinkPrimitiveParams(LINK_THIS,[1,2,3]), 0, -2))
]
