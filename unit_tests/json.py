from strutil import *
from lslopt.lslfuncs import *

tests = 0
errors = 0

# Begin JSON tests from http://wiki.secondlife.com/wiki/Json_usage_in_LSL/TestScript

def xrepr(x):
    if python3:
        if type(x) == unicode:
            return 'u' + repr(x)
        if type(x) == list:
            return '[' + ', '.join(xrepr(i) for i in x) + ']'
    elif python2:
        if type(x) == bytes:
            return 'b' + repr(x)
        if type(x) == list:
            return '[' + ', '.join(xrepr(i) for i in x) + ']'
    return repr(x)

def verify(msg, result, expected):
    global tests
    tests += 1
    if expected != result:
        global errors
        errors += 1
        werr(u"Test failed: " + msg + u'\n')
        werr(u"Actual: " + str2u(xrepr(result)) + u'\n')
        werr(u"Expect: " + str2u(xrepr(expected)) + u'\n')
        #return 0
    else:
        werr("PASSED! %s, expect=actual=%s\n" % (msg, xrepr(expected)))
    #return 1

def verify_list(msg, result, expected):
    verify(msg, str2u(xrepr(result)), str2u(xrepr(expected)))

def test_types():
    verify(u"Type of string",llJsonValueType(u"\"test\"",[]),JSON_STRING);
    verify(u"Type of string, unquoted",llJsonValueType(u"test",[]),JSON_INVALID);
    verify(u"Type of invalid",llJsonValueType(u"test",[]),JSON_INVALID);
    verify(u"Type of integer",llJsonValueType(u"12",[]),JSON_NUMBER);
    verify(u"Type of float",llJsonValueType(u"12.300000",[]),JSON_NUMBER);
    verify(u"Type of Inf (is unsupported by JSON standard)",llJsonValueType(u"Inf",[]),JSON_INVALID);
    verify(u"Type of NaN (is unsupported by JSON standard)",llJsonValueType(u"NaN",[]),JSON_INVALID);
    verify(u"Type of number",llJsonValueType(u"-123.4e-5",[]),JSON_NUMBER);
    verify(u"Type of object",llJsonValueType(u"{\"a\":\"b\"}",[]),JSON_OBJECT);
    # Expected to be OBJECT, since we don't do deep validation on input
    #verify(u"Type of object, invalid/unquoted key",llJsonValueType(u"{a:\"b\"}",[]),JSON_INVALID);
    \
verify(u"Type of object, invalid/unquoted key",llJsonValueType(u"{a:\"b\"}",[]),JSON_OBJECT) # check so
    # Expected to be OBJECT, since we don't do deep validation on input
    #verify(u"Type of object, invalid/unquoted value",llJsonValueType(u"{\"a\":b}",[]),JSON_INVALID);
    \
verify(u"Type of object, invalid/unquoted value",llJsonValueType(u"{\"a\":b}",[]),JSON_OBJECT) # check so
    verify(u"Type of array",llJsonValueType(u"[1,2,3]",[]),JSON_ARRAY);
    verify(u"Type of array padded front",llJsonValueType(u" [1,2,3]",[]),JSON_ARRAY);
    verify(u"Type of array padded back",llJsonValueType(u"[1,2,3] ",[]),JSON_ARRAY);
    verify(u"Type of array padded",llJsonValueType(u" [1,2,3] ",[]),JSON_ARRAY);
    verify(u"Type of true",llJsonValueType(u"true",[]),JSON_TRUE);
    verify(u"Type of false",llJsonValueType(u"false",[]),JSON_FALSE);
    verify(u"Type of null",llJsonValueType(u"null",[]),JSON_NULL);

    # test traversal of llJsonValueType
    json = u"[[1,2,3],{\"a\":3,\"b\":[true,\"test\",6],\"c\":\"true\",\"d\":false}]";
    verify(u"Type of [0]",llJsonValueType(json,[0]),JSON_ARRAY);
    verify(u"Type of [0,1]",llJsonValueType(json,[0,1]),JSON_NUMBER);
    verify(u"Type of [1]",llJsonValueType(json,[1]),JSON_OBJECT);
    verify(u"Type of [1,\"b\"]",llJsonValueType(json,[1,u"b"]),JSON_ARRAY);
    verify(u"Type of [1,\"b\",0]",llJsonValueType(json,[1,u"b",0]),JSON_TRUE);
    verify(u"Type of [1,\"b\",1]",llJsonValueType(json,[1,u"b",1]),JSON_STRING);
    verify(u"Type of [1,\"b\",2]",llJsonValueType(json,[1,u"b",2]),JSON_NUMBER);
    verify(u"Type of [1,\"c\"]",llJsonValueType(json,[1,u"c"]),JSON_STRING);
    verify(u"Type of [1,\"d\"]",llJsonValueType(json,[1,u"d"]),JSON_FALSE);
    verify(u"Type of [3] (invalid index at level 0)",llJsonValueType(json,[3]),JSON_INVALID);
    verify(u"Type of [-1] (invalid index at level 0)",llJsonValueType(json,[-1]),JSON_INVALID);
    verify(u"Type of [1,\"c\",3] (invalid index at level 1), MAINT-2670",llJsonValueType(json,[1,u"c",3]),JSON_INVALID);
    \
verify(u"Type of [1,\"c\",0] (first element of non-nested object)",llJsonValueType(json,[1,u"c",0]),JSON_INVALID);
# added by us
    verify(u"Type of [1,\"b\",3] (invalid index at level 2)",llJsonValueType(json,[1,u"b",3]),JSON_INVALID);
    verify(u"Type of [1,\"b\",2,0,1] (invalid index at level 3) MAINT-2670",llJsonValueType(json,[1,u"b",2, 0, 1]),JSON_INVALID);

    # some invalid cases where keys are uncoded
    json = u"[[1,2,3],{a:3,b:[true,\"test\",6],c:\"true\",\"d\":false}]";
    verify(u"Type of [1,\"a\"] where key is unquoted",llJsonValueType(json,[1,u"a"]),JSON_INVALID);
    verify(u"Type of [1,\"b\"] where key is unquoted",llJsonValueType(json,[1,u"b"]),JSON_INVALID);
    verify(u"Type of [1,\"c\"] where key is unquoted",llJsonValueType(json,[1,u"c"]),JSON_INVALID);


def test_get_value():
    json = u"[[1,2,3,4.0],{\"a\":3,\"b\":[true,\"test\",6]}]";
    verify(u"llJsonGetValue [0]",llJsonGetValue(json,[0]),u"[1,2,3,4.0]");
    verify(u"llJsonGetValue [0,1]",llJsonGetValue(json,[0,1]),u"2");
    verify(u"llJsonGetValue [1]",llJsonGetValue(json,[1]),u"{\"a\":3,\"b\":[true,\"test\",6]}");
    verify(u"llJsonGetValue [1,\"b\"]",llJsonGetValue(json,[1,u"b"]),u"[true,\"test\",6]");
    verify(u"llJsonGetValue [1,\"b\",0]",llJsonGetValue(json,[1,u"b",0]),JSON_TRUE);
    verify(u"llJsonGetValue [1,\"b\",1]",llJsonGetValue(json,[1,u"b",1]),u"test");
    verify(u"llJsonGetValue [1,\"b\",2]",llJsonGetValue(json,[1,u"b",2]),u"6");
    verify(u"llJsonGetValue [0,3]",llJsonGetValue(json,[0,3]), u"4.0");
    verify(u"llJsonGetValue [2] (invalid index at level 0)",llJsonGetValue(json,[2]),JSON_INVALID);
    verify(u"llJsonGetValue [-1] (invalid index at level 0)",llJsonGetValue(json,[-1]),JSON_INVALID);
    verify(u"llJsonGetValue [0,4] (invalid index within array)",llJsonGetValue(json,[0,4]),JSON_INVALID);
    verify(u"llJsonGetValue [\"f\"] (look for missing object within array, depth=0) MAINT-2671",llJsonGetValue(json,[u"f"]),JSON_INVALID);
    verify(u"llJsonGetValue [0,\"f\"] (look for missing object within array, depth=1) MAINT-2671",llJsonGetValue(json,[0,u"f"]),JSON_INVALID);
    verify(u"llJsonGetValue [1,2] (specify index within object - disallowed)",llJsonGetValue(json,[1,2]),JSON_INVALID);

    # invalid input json - missing quotes around 'a' and 'test'
    json = u"[[1,2,3,4.0],{a:3,\"b\":[true,test,6]}]";
    verify(u"llJsonGetValue [1,\"b\",1], unquoted/invalid string value",llJsonGetValue(json,[1,u"b",1]),JSON_INVALID);
    verify(u"llJsonGetValue [1,\"a\"], unquoted/invalid string for key",llJsonGetValue(json,[1,u"a"]),JSON_INVALID);

def test_set_value():
    # Test building from scratch
    json = u""
    json = llJsonSetValue(json,[0,0],u"1");
    verify(u"llJsonSetValue build json",json,u"[[1]]");
    json = llJsonSetValue(json,[0,1],u"2");
    verify(u"llJsonSetValue build json",json,u"[[1,2]]");
    json = llJsonSetValue(json,[0,2],u"3");
    verify(u"llJsonSetValue build json",json,u"[[1,2,3]]");
    json = llJsonSetValue(json,[1,u"a"],u"3");
    verify(u"llJsonSetValue build json",json,u"[[1,2,3],{\"a\":3}]");
    json = llJsonSetValue(json,[1,u"b",0],JSON_TRUE);
    verify(u"llJsonSetValue build json",json,u"[[1,2,3],{\"a\":3,\"b\":[true]}]");
    json = llJsonSetValue(json,[1,u"b",1],u"test");
    verify(u"llJsonSetValue build json",json,u"[[1,2,3],{\"a\":3,\"b\":[true,\"test\"]}]");
    json = llJsonSetValue(json,[1,u"b",2],u"6");
    verify(u"llJsonSetValue completed json",json,u"[[1,2,3],{\"a\":3,\"b\":[true,\"test\",6]}]");

    # Test replacing
    json = llJsonSetValue(json,[1,u"b",1],u"foo");
    verify(u"llJsonSetValue completed json",json,u"[[1,2,3],{\"a\":3,\"b\":[true,\"foo\",6]}]");
    json = llJsonSetValue(json,[1,u"b"],JSON_TRUE);
    verify(u"llJsonSetValue completed json, true",json,u"[[1,2,3],{\"a\":3,\"b\":true}]");
    json = llJsonSetValue(json,[1,u"b"],u"true");
    verify(u"llJsonSetValue completed json, alt true",json,u"[[1,2,3],{\"a\":3,\"b\":true}]");
    json = llJsonSetValue(json,[1,0,0],JSON_FALSE);
    verify(u"llJsonSetValue completed json",json,u"[[1,2,3],[[false]]]");

    # Test appending
    json = llJsonSetValue(u"[[1,2,3],{\"a\":3,\"b\":[true,\"test\",6]}]",[0,JSON_APPEND], u"4.0");
    verify(u"llJsonSetValue append to first array",json,u"[[1,2,3,4.0],{\"a\":3,\"b\":[true,\"test\",6]}]");
    json = llJsonSetValue(json,[1,u"b",JSON_APPEND], u"5.0");
    verify(u"llJsonSetValue append to array withhin object",json,u"[[1,2,3,4.0],{\"a\":3,\"b\":[true,\"test\",6,5.0]}]");
    json = llJsonSetValue(json,[JSON_APPEND], u"6.0");
    verify(u"llJsonSetValue append to outer array",json,u"[[1,2,3,4.0],{\"a\":3,\"b\":[true,\"test\",6,5.0]},6.0]");
    json = llJsonSetValue(u"[]",[JSON_APPEND], u"\"alone\"");
    verify(u"llJsonSetValue append to empty array (MAINT-2684)",json,u"[\"alone\"]");
    json = llJsonSetValue(u"[]",[1], u"\"alone\"");
    verify(u"llJsonSetValue append to empty array at invalid index (MAINT-2684)",json,JSON_INVALID);
    json = llJsonSetValue(u"[]",[0], u"\"alone\"");
    verify(u"llJsonSetValue append to empty array at first index (MAINT-2684)",json,u"[\"alone\"]");

    # Test deleting
    json = u"[[1,2,3],{\"a\":3,\"b\":[true,\"test\",6,null]}]";
    json = llJsonSetValue(json,[1,u"b",1],JSON_DELETE);
    verify(u"llJsonSetValue deleting string in middle of array",json,u"[[1,2,3],{\"a\":3,\"b\":[true,6,null]}]");
    json = llJsonSetValue(json,[1,u"b",2],JSON_DELETE);
    verify(u"llJsonSetValue deleting null at end of array",json,u"[[1,2,3],{\"a\":3,\"b\":[true,6]}]");
    json = llJsonSetValue(json,[1,u"b"],JSON_DELETE);
    verify(u"llJsonSetValue deleting key-value",json,u"[[1,2,3],{\"a\":3}]");
    json = llJsonSetValue(json,[1],JSON_DELETE);
    verify(u"llJsonSetValue deleting object in array",json,u"[[1,2,3]]");
    json = u"[[1,2,3],4]";
    json = llJsonSetValue(json,[0],JSON_DELETE);
    verify(u"llJsonSetValue deleting array (which is first index in array)",json,u"[4]");
    json = llJsonSetValue(json,[0],JSON_DELETE);
    verify(u"llJsonSetValue deleting last element in array",json,u"[]");
    json = u"[[1]]";
    json = llJsonSetValue(json,[0,0],JSON_DELETE);
    verify(u"llJsonSetValue deleting last element in array",json,u"[[]]");
    json = llJsonSetValue(json,[0],JSON_DELETE);
    verify(u"llJsonSetValue deleting array within array",json,u"[]");

    # Test failures in deleting
    json = u"[[1,2,3],{\"a\":3,\"b\":[true,\"test\",6,null]}]";
    verify(u"llJsonSetValue deleting undefined key-value in object",llJsonSetValue(json,[1,u"d"],JSON_DELETE),JSON_INVALID);
    verify(u"llJsonSetValue deleting out-of-range index in array",llJsonSetValue(json,[2],JSON_DELETE),JSON_INVALID);
    verify(u"llJsonSetValue deleting depth within object that doesn't exist",llJsonSetValue(json,[1,u"a",u"unicorn"],JSON_DELETE),JSON_INVALID);
    verify(u"llJsonSetValue deleting depth within array that doesn't exist",llJsonSetValue(json,[0,1,1],JSON_DELETE),JSON_INVALID);

    # this is the only failure mode that should exist.
    json = u"[[1,2,3],{\"a\":3,\"b\":[true,\"foo\",6]}]";
    json = llJsonSetValue(json,[3],JSON_FALSE);
    verify(u"llJsonSetValue fail to insert data into invalid array index (MAINT-2675)",json,JSON_INVALID);

def test_json_to_list():
    l = llJson2List(u"[[1,2,3],{\"a\":3,\"b\":[true,\"test\",6]}]");
    verify_list(u"llJson2List first",l,[u"[1,2,3]",u"{\"a\":3,\"b\":[true,\"test\",6]}"]);
    n = llJson2List(llList2String(l,0));
    verify_list(u"llJson2List l,0",n,[1,2,3]);
    n = llJson2List(llList2String(l,1));
    verify_list(u"llJson2List l,1",n,[u"a",3,u"b",u"[true,\"test\",6]"]);
    n = llJson2List(llList2String(n,3));
    verify_list(u"llJson2List n,3",n,[JSON_TRUE, u"test", 6]);
    n = llJson2List(llList2String(n,1));
    verify_list(u"llJson2List n,1",n,[u"test"]);
    n = llJson2List(u"");
    verify_list(u"Empty JSON string becomes empty list",n,[]);
    n = llJson2List(u"[]");
    verify_list(u"Empty JSON array becomes empty list (MAINT-2678)",n,[]);
    n = llJson2List(u"{}");
    verify_list(u"Empty JSON object becomes empty list (MAINT-2678)",n,[]);
    n = llJson2List(u"Non-JSON string, with comma");
    verify_list(u"llJson2List for non-JSON string is stored as a single object",n,[u"Non-JSON string, with comma"]);
    n = llJson2List(u"[malformed}");
    verify_list(u"llJson2List, malformed input",n,[u"[malformed}"]);

def test_list_to_json():
    TRUE=1
    # test objects
    json = llList2Json(JSON_OBJECT,[u"a",1,u"b",2.5,u"c",u"test",u"d",u"true",u"e",u"[1,2,3]"]);
    verify(u"llList2Json, json object",json,u"{\"a\":1,\"b\":2.500000,\"c\":\"test\",\"d\":true,\"e\":[1,2,3]}");

    # test arrays
    json = llList2Json(JSON_ARRAY,[1,2.5,u"test",u"true",u"[1,2,3]"]);
    verify(u"llList2Json, json array",json,u"[1,2.500000,\"test\",true,[1,2,3]]");

    # test arrays
    json = llList2Json(JSON_ARRAY,[1,2.5,u"test",JSON_TRUE,u"[1,2,3]"]);
    verify(u"llList2Json, json array, alternative true representation",json,u"[1,2.500000,\"test\",true,[1,2,3]]");

    # test objects, with empty input
    json = llList2Json(JSON_OBJECT,[]);
    verify(u"llList2Json, json object with empty input (MAINT-2681)",json,u"{}");

    # test arrays, with empty input
    json = llList2Json(JSON_ARRAY,[]);
    verify(u"llList2Json, json array with empty input (MAINT-2681)",json,u"[]");

    # test objects which are truncated
    json = llList2Json(JSON_OBJECT,[u"a",1,u"b",2.5,u"c",u"test",u"d",u"true",u"e"]);
    verify(u"llList2Json, json object, truncated",json,JSON_INVALID);

    # test objects which has a non-string identifier somewhere
    json = llList2Json(JSON_OBJECT,[u"a",1,TRUE,2.5,u"c",u"test",u"d",u"true",u"e"]);
    verify(u"llList2Json, json object, non-string in one of the first stride values",json,JSON_INVALID);

    # test invalid type
    json = llList2Json(u"foo",[u"a",1,u"b",2.5,u"c",u"test",u"d",u"true",u"e",u"[1,2,3]"]);
    verify(u"llList2Json, json invalid type",json,JSON_INVALID);

def test_strings_with_escaped_chars():
    escaped_pairs = [
        (u"funky\"string", u"funky\\\"string", u"quote in middle"),
        (u"funkystr\"ing", u"funkystr\\\"ing", u"quote in middle, other position"),
        # note that we have to double-up backslashes to assign them to strings..
        (u"funky\\string", u"funky\\\\string", u"backslashes in middle"),
        (u"\\funkystring", u"\\\\funkystring", u"backslashes at beginning"),
        (u"funky\nstring", u"funky\\nstring", u"newline in string"),
        (u"funky/string", u"funky\\/string", u"forward slash in string"),
        # TAB (\t) fails, because it seems that LSL automatically converts any tab into 4 consecutive spaces.
          (u"funky\tstring", u"funky\\tstring", u"tab in string"), # we enable it, with some mods
        # these cases fail; it seems that LSL doesn't support these characters, and strips the '\'
          (u"funky\x08string", u"funky\\bstring", u"backspace in middle"), # we enable it, with some mods
          (u"funky\x0Cstring", u"funky\\fstring", u"form feed in middle"), # we enable it, with some mods
          (u"funky\rstring", "funky\\rstring", u"carriage return in string"), # we enable it, with some mods
        # note that the following case can't be supported, since strings starting with \" can't be escaped
          (u"\"funkystring", u"\\\"funkystring", u"quote in beginning"), # we enable it as it's supported
        (u"vanilla string", u"vanilla string", u"nothing that needs to be escaped..")
    ];
    for funky_string, funky_string_escaped, escaped_desc1 in escaped_pairs:
        escaped_desc = u" '" + escaped_desc1 + u"'"

        verify(u"Type of string with escaped char (for MAINT-2698),"+escaped_desc,llJsonValueType(u"\""+funky_string_escaped+u"\"",[]),JSON_STRING);

        json = u"[[1,2,3,4.0],{\""+funky_string_escaped+u"\":3,\"b\":\""+funky_string_escaped+u"value\"}]";
        verify(u"llJsonGetValue [1,\""+funky_string_escaped+"\"] (for MAINT-2698),"+escaped_desc,
            llJsonGetValue(json,[1,funky_string]),u"3");
        verify(u"llJsonGetValue [1,\"b\"] (for MAINT-2698),"+escaped_desc,llJsonGetValue(json,[1,u"b"]),funky_string+u"value");

        #llSay(0, "DEBU G: '" + llEscapeURL(json) + "' is input for test " + escaped_desc);
        json = llJsonSetValue(json,[0],funky_string);
        verify(u"llJsonSetValue with escaped string as value (for MAINT-2698),"+escaped_desc,json,
            u"[\""+funky_string_escaped+u"\",{\""+funky_string_escaped+u"\":3,\"b\":\""+funky_string_escaped+u"value\"}]");

        json = llJsonSetValue(json,[0],funky_string);
        verify(u"llJsonSetValue with escaped string as value (for MAINT-2698),"+escaped_desc,json,
            u"[\""+funky_string_escaped+u"\",{\""+funky_string_escaped+u"\":3,\"b\":\""+funky_string_escaped+u"value\"}]");

        json = llJsonSetValue(json,[0,funky_string], funky_string+u"value");
        verify(u"llJsonSetValue with escaped string as key's value (for MAINT-2698),"+escaped_desc,json,
            u"[{\""+funky_string_escaped+u"\":\""+funky_string_escaped+u"value\"},{\""+funky_string_escaped+u"\":3,\"b\":\""+funky_string_escaped+u"value\"}]");

        l = llJson2List(json);
        verify_list(u"llJson2List extracting object containing escaped string (for MAINT-2698),"+escaped_desc, l,
            [u"{\""+funky_string_escaped+u"\":\""+funky_string_escaped+u"value\"}",u"{\""+funky_string_escaped+u"\":3,\"b\":\""+funky_string_escaped+u"value\"}"]);
        n = llJson2List(llList2String(l, 0));
        verify_list(u"llJson2List extracting escaped strings (for MAINT-2698),"+escaped_desc, n,
            [funky_string,funky_string+u"value"]);

        json = llList2Json(JSON_ARRAY,n);
        verify(u"llList2Json from escaped-containing string to array (for MAINT-2698),"+escaped_desc,json,
            u"[\""+funky_string_escaped+u"\",\""+funky_string_escaped+u"value\"]");

        json = llList2Json(JSON_OBJECT,n);
        verify(u"llList2Json from escaped-containing string to object (for MAINT-2698),"+escaped_desc,json,
            u"{\""+funky_string_escaped+u"\":\""+funky_string_escaped+u"value\"}");

def maint3070():
    verify(u"Set value 'messa[g]e'", llJsonSetValue(u"",[u"toast"],u"messa[g]e"), u"{\"toast\":\"messa[g]e\"}");
    verify(u"Set value 'messag[e]'", llJsonSetValue(u"",[u"toast"],u"messag[e]"), u"{\"toast\":\"messag[e]\"}");
    #verify(u"Set value 'messag\[e\]'", llJsonSetValue(u"",[u"toast"],u"messag\[e\]"), u"{\"toast\":\"messag[e]\"}");
    # ^^ BROKEN!!!!! LSL does not include the backslashes in the strings, so the above test is redundant.
    # Python does, thus making that test would break it. We include our own test tho:
    \
verify(u"Set value 'messag\\[e\\]'", llJsonSetValue(u"",[u"toast"],u"messag\\[e\\]"), u"{\"toast\":\"messag\\\\[e\\\\]\"}");

def maint4187():
    verify(u"Valid json number with + before exponent", llJsonValueType(u"1.0e+1", []), JSON_NUMBER);
    verify(u"Valid json number with - before exponent", llJsonValueType(u"1.0e-1", []), JSON_NUMBER);
    verify(u"Valid json number with - before exponent and mantissa", llJsonValueType(u"-1.0e-1", []), JSON_NUMBER);
    verify(u"Valid json number with unsigned exponent", llJsonValueType(u"1.0e1", []), JSON_NUMBER);
    verify(u"Invalid json number due to + before mantissa", llJsonValueType(u"+1.0e1", []), JSON_INVALID);

    verify(u"Invalid json number due to leading e", llJsonValueType(u"e1", []), JSON_INVALID);
    verify(u"Invalid json number due to leading 0", llJsonValueType(u"01", []), JSON_INVALID);
    verify(u"Invalid json number due to leading -0", llJsonValueType(u"-01", []), JSON_INVALID);
    verify(u"Valid json number with 0 immediately before .", llJsonValueType(u"0.01", []), JSON_NUMBER);
    verify(u"Valid json number with -0 immediately before .", llJsonValueType(u"-0.01", []), JSON_NUMBER);

def maint3053():
    jT1 = u"[1, 2]"; # A JSON array
    verify(u"llJsonSetValue(jT1,[2],\"t\")",llJsonSetValue(jT1,[2],u"t"),u"[1,2,\"t\"]");
    verify(u"llJsonSetValue(jT1,[3],\"t\")",llJsonSetValue(jT1,[3],u"t"),JSON_INVALID);
    verify(u"llJsonSetValue(jT1,[0, 0],\"t\")",llJsonSetValue(jT1,[0, 0],u"t"),u"[[\"t\"],2]");
    verify(u"llJsonSetValue(jT1,[0, 0, 2, \"t\", 75],\"t\")",llJsonSetValue(jT1,[0, 0, 2, u"t", 75],u"t"),JSON_INVALID);
    verify(u"llJsonSetValue(jT1,[0, 1],\"t\")",llJsonSetValue(jT1,[0, 1],u"t"),JSON_INVALID);
    verify(u"llJsonSetValue(jT1,[0, 1, 2, \"t\", 75],\"t\")",llJsonSetValue(jT1,[0, 1, 2, u"t", 75],u"t"),JSON_INVALID);

    jT2 = u"[ [\"A\", \"B\", \"C\"], 2]";
    verify(u"llJsonSetValue(jT2,[0, 3],\"t\")",llJsonSetValue(jT2,[0, 3],u"t"),u"[[\"A\",\"B\",\"C\",\"t\"],2]");
    verify(u"llJsonSetValue(jT2,[0, 4],\"t\")",llJsonSetValue(jT2,[0, 4],u"t"),JSON_INVALID);
    verify(u"llJsonSetValue(jT2,[0, 1, 0],\"t\")",llJsonSetValue(jT2,[0, 1, 0],u"t"),u"[[\"A\",[\"t\"],\"C\"],2]");
    verify(u"llJsonSetValue(jT2,[0, 1, 1],\"t\")",llJsonSetValue(jT2,[0, 1, 1],u"t"),JSON_INVALID);

    jT3 = u"{\"1\":2}";
    verify(u"llJsonSetValue(jT3,[\"1\"],\"t\")",llJsonSetValue(jT3,[u"1"],u"t"),u"{\"1\":\"t\"}");
    verify(u"llJsonSetValue(jT3,[\"1\",0],\"t\")",llJsonSetValue(jT3,[u"1",0],u"t"),u"{\"1\":[\"t\"]}");
    verify(u"llJsonSetValue(jT3,[\"1\",1],\"t\")",llJsonSetValue(jT3,[u"1",1],u"t"),JSON_INVALID);

    jGood = u"[null, 2]";
    verify(u"llJsonValueType(jGood, [0])",llJsonValueType(jGood, [0]),JSON_NULL);
    verify(u"llJsonValueType(jGood, [0, 0])",llJsonValueType(jGood, [0, 0]),JSON_INVALID);

    jBad = u"[, 2]";
    verify(u"llJsonValueType(jBad,[0])",llJsonValueType(jBad,[0]),JSON_INVALID);
    verify(u"llJsonValueType(jBad,[0, 0, 2, \"t\", 75])",llJsonValueType(jBad,[0, 0, 2, u"t", 75]),JSON_INVALID);
    verify(u"llJsonGetValue(jBad,[1, 0, 2, \"t\", 75])",llJsonGetValue(jBad,[1, 0, 2, u"t", 75]),JSON_INVALID);

def maint3081():
    verify(u"llJsonSetValue blank string",llJsonSetValue(u"",[u"test"],u""),u"{\"test\":\"\"}");
    verify(u"llJsonSetValue JSON_NULL",llJsonSetValue(u"",[u"test"],JSON_NULL),u"{\"test\":null}");
    verify(u"llJsonGetValue blank string",llJsonGetValue(u"{\"test\":\"\"}",[u"test"]),u"");
    verify(u"llJsonGetValue JSON_NULL",llJsonGetValue(u"{\"test\":null}",[u"test"]),JSON_NULL);
    verify(u"Identity (set->get) blank string",llJsonGetValue(llJsonSetValue(u"",[u"test"],u""),[u"test"]),u"");
    verify(u"Identity (set->get) JSON_NULL",llJsonGetValue(llJsonSetValue(u"",[u"test"],JSON_NULL),[u"test"]),JSON_NULL);

def test_jira_fixes():
    # FIXME: llJsonSetValue pending
    maint3070();
    if 6466 not in Bugs:
        maint4187();
    # FIXME: llJsonSetValue pending
    maint3053();
    # FIXME: llJsonSetValue pending
    maint3081();

def run_tests():
    # JSON tests from the wiki
    test_types();
    test_get_value();
    # FIXME: llJsonSetValue pending
    test_set_value();
    test_json_to_list();
    test_list_to_json();
    # FIXME: llJsonSetValue pending
    test_strings_with_escaped_chars();
    test_jira_fixes();
    werr("Number of tests: %d; passed: %d; failed: %d\n"
         % (tests,tests-errors,errors))
    werr("(138 failures expected)\n")

    return errors
