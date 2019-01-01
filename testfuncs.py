#    (C) Copyright 2015-2019 Sei Lisa. All rights reserved.
#
#    This file is part of LSL PyOptimizer.
#
#    LSL PyOptimizer is free software: you can redistribute it and/or
#    modify it under the terms of the GNU General Public License as
#    published by the Free Software Foundation, either version 3 of the
#    License, or (at your option) any later version.
#
#    LSL PyOptimizer is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with LSL PyOptimizer. If not, see <http://www.gnu.org/licenses/>.

# Unit Testing of lslfuncs, to ensure they match the output of LSL

from lslopt.lslcommon import *
from lslopt.lslfuncs import *
from lslopt import lslcommon
import sys
import math
from base64 import b64encode

StopAtFirstError = False

errors = 0
tests = 0
untested = 0

nan = NaN
inf = Infinity

F = float.fromhex

class ETestFailed(Exception):
    def __init__(self):
        super(self.__class__, self).__init__("Test failed")
    pass

# Not failproof because we can't redefine float.__repr__
# so floats in vectors/rotations/lists aren't handled.
class newfloat(float):
    def __repr__(n):
        if not math.isnan(n):
            return float.__repr__(n)
        return '-nan' if math.copysign(1, n) < 0 else 'nan'

def reallyequal(actual, expected, tol):
    if tol is None:
        return repr(actual) == repr(expected)

    if type(actual) != type(expected):
        return False

    # Deal with floats (edge cases, tolerance)
    if isinstance(actual, float):
        if actual == 0.0:
            return repr(actual) == repr(expected)
        if math.isnan(actual):
            # This compares the sign of NaN as well
            return math.isnan(expected) and math.copysign(1, actual) == math.copysign(1, expected)
        if math.isinf(actual) and math.isinf(expected):
            return actual == expected
        return abs(actual - expected) <= tol

    # Deal with tuples and lists (item-by-item, recursively)
    if isinstance(actual, (tuple, list)):
        return all(reallyequal(i1, i2, tol) for i1, i2 in zip(actual, expected))

    # Fall back to 'classic' equality
    return actual == expected

def test(fn, expected):
    global tests
    global errors
    global untested
    tol = 0.0  # compare to zero tolerance
    tests += 1
    if StopAtFirstError:
        if errors:
            untested += 1
            return

    actual = eval(fn)
    if isinstance(actual, float):
       actual = newfloat(actual)
    if isinstance(expected, float):
       expected = newfloat(expected)
    werr = sys.stderr.write
    if not reallyequal(actual, expected, tol):
        werr("Test failed: " + fn + '\n')
        werr("Actual: " + repr(actual) + '\n')
        werr("Expect: " + repr(expected) + '\n')
        errors += 1
        #raise ETestFailed
    else:
        pass#sys.stdout.write("PASSED! %s == %s\n" % (fn, repr(expected)))

def shouldexcept(txt, exc):
    global tests
    global errors
    global untested

    tests += 1
    if StopAtFirstError:
        if errors:
            untested += 1
            return

    try:
        eval(txt)
    except exc:
        #sys.stdout.write("PASSED! %s on %s\n" % (exc.__name__, txt))
        return

    werr = sys.stderr.write
    werr('Test failed: ' + txt + '\n')
    werr('Actual: (no exception)\n')
    werr('Expect: should raise ' + exc.__name__ + '\n')
    errors += 1

def testXB64S(s1, s2, expect):
    if type(s1) == str:
        s1 = s1.decode('utf8')
    if type(s2) == str:
        s2 = s2.decode('utf8')
    if type(expect) == str:
        expect = expect.decode('utf8')
    # llXorBase64Strings can only be executed in calculator mode
    SaveIsCalc = lslcommon.IsCalc
    lslcommon.IsCalc = True
    test('llXorBase64Strings(' + repr(s1) + ',' + repr(s2) + ')', expect)
    lslcommon.IsCalc = SaveIsCalc

def testXB64SC(s1, s2, expect):
    if type(s1) == str:
        s1 = s1.decode('utf8')
    if type(s2) == str:
        s2 = s2.decode('utf8')
    if type(expect) == str:
        expect = expect.decode('utf8')
    test('llXorBase64StringsCorrect(' + repr(s1) + ',' + repr(s2) + ')', expect)

def testXB64(s1, s2, expect, Bug3763=False):
    if type(s1) == str:
        s1 = s1.decode('utf8')
    if type(s2) == str:
        s2 = s2.decode('utf8')
    if type(expect) == str:
        expect = expect.decode('utf8')
    if Bug3763:
        Bugs.add(3763)
    test('llXorBase64(' + repr(s1) + ',' + repr(s2) + ')', expect)
    Bugs.discard(3763)

def h2b64(h):
    return unicode(b64encode(bytes(bytearray.fromhex(h))))

def testB642S(b, expected):
    assert type(b) == str
    assert type(expected) == str
    test('llEscapeURL(llBase64ToString(h2b64(' + repr(b) + ')))', unicode(expected))

# Begin JSON tests from http://wiki.secondlife.com/wiki/Json_usage_in_LSL/TestScript
def verify(msg, result, expected):
    global tests
    tests += 1
    werr = sys.stderr.write
    if expected != result:
        global errors
        errors += 1
        werr("Test failed: " + msg + '\n')
        werr("Actual: " + repr(result) + '\n')
        werr("Expect: " + repr(expected) + '\n')
        #return 0
    else:
        pass#sys.stdout.write("PASSED! %s, expect=actual=%s\n" % (msg, repr(expected)))
    #return 1

def verify_list(msg, result, expected):
    verify(msg, repr(result), repr(expected))

def test_types():
    verify("Type of string",llJsonValueType(u"\"test\"",[]),JSON_STRING);
    verify("Type of string, unquoted",llJsonValueType(u"test",[]),JSON_INVALID);
    verify("Type of invalid",llJsonValueType(u"test",[]),JSON_INVALID);
    verify("Type of integer",llJsonValueType(u"12",[]),JSON_NUMBER);
    verify("Type of float",llJsonValueType(u"12.300000",[]),JSON_NUMBER);
    verify("Type of Inf (is unsupported by JSON standard)",llJsonValueType(u"Inf",[]),JSON_INVALID);
    verify("Type of NaN (is unsupported by JSON standard)",llJsonValueType(u"NaN",[]),JSON_INVALID);
    verify("Type of number",llJsonValueType(u"-123.4e-5",[]),JSON_NUMBER);
    verify("Type of object",llJsonValueType(u"{\"a\":\"b\"}",[]),JSON_OBJECT);
    # Expected to be OBJECT, since we don't do deep validation on input
    #verify("Type of object, invalid/unquoted key",llJsonValueType(u"{a:\"b\"}",[]),JSON_INVALID);
    \
verify("Type of object, invalid/unquoted key",llJsonValueType(u"{a:\"b\"}",[]),JSON_OBJECT) # check so
    # Expected to be OBJECT, since we don't do deep validation on input
    #verify("Type of object, invalid/unquoted value",llJsonValueType(u"{\"a\":b}",[]),JSON_INVALID);
    \
verify("Type of object, invalid/unquoted value",llJsonValueType(u"{\"a\":b}",[]),JSON_OBJECT) # check so
    verify("Type of array",llJsonValueType(u"[1,2,3]",[]),JSON_ARRAY);
    verify("Type of array padded front",llJsonValueType(u" [1,2,3]",[]),JSON_ARRAY);
    verify("Type of array padded back",llJsonValueType(u"[1,2,3] ",[]),JSON_ARRAY);
    verify("Type of array padded",llJsonValueType(u" [1,2,3] ",[]),JSON_ARRAY);
    verify("Type of true",llJsonValueType(u"true",[]),JSON_TRUE);
    verify("Type of false",llJsonValueType(u"false",[]),JSON_FALSE);
    verify("Type of null",llJsonValueType(u"null",[]),JSON_NULL);

    # test traversal of llJsonValueType
    json = u"[[1,2,3],{\"a\":3,\"b\":[true,\"test\",6],\"c\":\"true\",\"d\":false}]";
    verify("Type of [0]",llJsonValueType(json,[0]),JSON_ARRAY);
    verify("Type of [0,1]",llJsonValueType(json,[0,1]),JSON_NUMBER);
    verify("Type of [1]",llJsonValueType(json,[1]),JSON_OBJECT);
    verify("Type of [1,\"b\"]",llJsonValueType(json,[1,u"b"]),JSON_ARRAY);
    verify("Type of [1,\"b\",0]",llJsonValueType(json,[1,u"b",0]),JSON_TRUE);
    verify("Type of [1,\"b\",1]",llJsonValueType(json,[1,u"b",1]),JSON_STRING);
    verify("Type of [1,\"b\",2]",llJsonValueType(json,[1,u"b",2]),JSON_NUMBER);
    verify("Type of [1,\"c\"]",llJsonValueType(json,[1,u"c"]),JSON_STRING);
    verify("Type of [1,\"d\"]",llJsonValueType(json,[1,u"d"]),JSON_FALSE);
    verify("Type of [3] (invalid index at level 0)",llJsonValueType(json,[3]),JSON_INVALID);
    verify("Type of [-1] (invalid index at level 0)",llJsonValueType(json,[-1]),JSON_INVALID);
    verify("Type of [1,\"c\",3] (invalid index at level 1), MAINT-2670",llJsonValueType(json,[1,u"c",3]),JSON_INVALID);
    \
verify("Type of [1,\"c\",0] (first element of non-nested object)",llJsonValueType(json,[1,u"c",0]),JSON_INVALID);
# added by us
    verify("Type of [1,\"b\",3] (invalid index at level 2)",llJsonValueType(json,[1,u"b",3]),JSON_INVALID);
    verify("Type of [1,\"b\",2,0,1] (invalid index at level 3) MAINT-2670",llJsonValueType(json,[1,u"b",2, 0, 1]),JSON_INVALID);

    # some invalid cases where keys are uncoded
    json = u"[[1,2,3],{a:3,b:[true,\"test\",6],c:\"true\",\"d\":false}]";
    verify("Type of [1,\"a\"] where key is unquoted",llJsonValueType(json,[1,u"a"]),JSON_INVALID);
    verify("Type of [1,\"b\"] where key is unquoted",llJsonValueType(json,[1,u"b"]),JSON_INVALID);
    verify("Type of [1,\"c\"] where key is unquoted",llJsonValueType(json,[1,u"c"]),JSON_INVALID);


def test_get_value():
    json = u"[[1,2,3,4.0],{\"a\":3,\"b\":[true,\"test\",6]}]";
    verify("llJsonGetValue [0]",llJsonGetValue(json,[0]),u"[1,2,3,4.0]");
    verify("llJsonGetValue [0,1]",llJsonGetValue(json,[0,1]),u"2");
    verify("llJsonGetValue [1]",llJsonGetValue(json,[1]),u"{\"a\":3,\"b\":[true,\"test\",6]}");
    verify("llJsonGetValue [1,\"b\"]",llJsonGetValue(json,[1,u"b"]),u"[true,\"test\",6]");
    verify("llJsonGetValue [1,\"b\",0]",llJsonGetValue(json,[1,u"b",0]),JSON_TRUE);
    verify("llJsonGetValue [1,\"b\",1]",llJsonGetValue(json,[1,u"b",1]),u"test");
    verify("llJsonGetValue [1,\"b\",2]",llJsonGetValue(json,[1,u"b",2]),u"6");
    verify("llJsonGetValue [0,3]",llJsonGetValue(json,[0,3]), u"4.0");
    verify("llJsonGetValue [2] (invalid index at level 0)",llJsonGetValue(json,[2]),JSON_INVALID);
    verify("llJsonGetValue [-1] (invalid index at level 0)",llJsonGetValue(json,[-1]),JSON_INVALID);
    verify("llJsonGetValue [0,4] (invalid index within array)",llJsonGetValue(json,[0,4]),JSON_INVALID);
    verify("llJsonGetValue [\"f\"] (look for missing object within array, depth=0) MAINT-2671",llJsonGetValue(json,[u"f"]),JSON_INVALID);
    verify("llJsonGetValue [0,\"f\"] (look for missing object within array, depth=1) MAINT-2671",llJsonGetValue(json,[0,u"f"]),JSON_INVALID);
    verify("llJsonGetValue [1,2] (specify index within object - disallowed)",llJsonGetValue(json,[1,2]),JSON_INVALID);

    # invalid input json - missing quotes around 'a' and 'test'
    json = u"[[1,2,3,4.0],{a:3,\"b\":[true,test,6]}]";
    verify("llJsonGetValue [1,\"b\",1], unquoted/invalid string value",llJsonGetValue(json,[1,u"b",1]),JSON_INVALID);
    verify("llJsonGetValue [1,\"a\"], unquoted/invalid string for key",llJsonGetValue(json,[1,u"a"]),JSON_INVALID);

def test_set_value():
    # Test building from scratch
    json = u""
    json = llJsonSetValue(json,[0,0],u"1");
    verify("llJsonSetValue build json",json,u"[[1]]");
    json = llJsonSetValue(json,[0,1],u"2");
    verify("llJsonSetValue build json",json,u"[[1,2]]");
    json = llJsonSetValue(json,[0,2],u"3");
    verify("llJsonSetValue build json",json,u"[[1,2,3]]");
    json = llJsonSetValue(json,[1,u"a"],u"3");
    verify("llJsonSetValue build json",json,u"[[1,2,3],{\"a\":3}]");
    json = llJsonSetValue(json,[1,u"b",0],JSON_TRUE);
    verify("llJsonSetValue build json",json,u"[[1,2,3],{\"a\":3,\"b\":[true]}]");
    json = llJsonSetValue(json,[1,u"b",1],u"test");
    verify("llJsonSetValue build json",json,u"[[1,2,3],{\"a\":3,\"b\":[true,\"test\"]}]");
    json = llJsonSetValue(json,[1,u"b",2],u"6");
    verify("llJsonSetValue completed json",json,u"[[1,2,3],{\"a\":3,\"b\":[true,\"test\",6]}]");

    # Test replacing
    json = llJsonSetValue(json,[1,u"b",1],u"foo");
    verify("llJsonSetValue completed json",json,u"[[1,2,3],{\"a\":3,\"b\":[true,\"foo\",6]}]");
    json = llJsonSetValue(json,[1,u"b"],JSON_TRUE);
    verify("llJsonSetValue completed json, true",json,u"[[1,2,3],{\"a\":3,\"b\":true}]");
    json = llJsonSetValue(json,[1,u"b"],u"true");
    verify("llJsonSetValue completed json, alt true",json,u"[[1,2,3],{\"a\":3,\"b\":true}]");
    json = llJsonSetValue(json,[1,0,0],JSON_FALSE);
    verify("llJsonSetValue completed json",json,u"[[1,2,3],[[false]]]");

    # Test appending
    json = llJsonSetValue(u"[[1,2,3],{\"a\":3,\"b\":[true,\"test\",6]}]",[0,JSON_APPEND], u"4.0");
    verify("llJsonSetValue append to first array",json,u"[[1,2,3,4.0],{\"a\":3,\"b\":[true,\"test\",6]}]");
    json = llJsonSetValue(json,[1,u"b",JSON_APPEND], u"5.0");
    verify("llJsonSetValue append to array withhin object",json,u"[[1,2,3,4.0],{\"a\":3,\"b\":[true,\"test\",6,5.0]}]");
    json = llJsonSetValue(json,[JSON_APPEND], u"6.0");
    verify("llJsonSetValue append to outer array",json,u"[[1,2,3,4.0],{\"a\":3,\"b\":[true,\"test\",6,5.0]},6.0]");
    json = llJsonSetValue(u"[]",[JSON_APPEND], u"\"alone\"");
    verify("llJsonSetValue append to empty array (MAINT-2684)",json,u"[\"alone\"]");
    json = llJsonSetValue(u"[]",[1], u"\"alone\"");
    verify("llJsonSetValue append to empty array at invalid index (MAINT-2684)",json,JSON_INVALID);
    json = llJsonSetValue(u"[]",[0], u"\"alone\"");
    verify("llJsonSetValue append to empty array at first index (MAINT-2684)",json,u"[\"alone\"]");

    # Test deleting
    json = u"[[1,2,3],{\"a\":3,\"b\":[true,\"test\",6,null]}]";
    json = llJsonSetValue(json,[1,u"b",1],JSON_DELETE);
    verify("llJsonSetValue deleting string in middle of array",json,u"[[1,2,3],{\"a\":3,\"b\":[true,6,null]}]");
    json = llJsonSetValue(json,[1,u"b",2],JSON_DELETE);
    verify("llJsonSetValue deleting null at end of array",json,u"[[1,2,3],{\"a\":3,\"b\":[true,6]}]");
    json = llJsonSetValue(json,[1,u"b"],JSON_DELETE);
    verify("llJsonSetValue deleting key-value",json,u"[[1,2,3],{\"a\":3}]");
    json = llJsonSetValue(json,[1],JSON_DELETE);
    verify("llJsonSetValue deleting object in array",json,u"[[1,2,3]]");
    json = u"[[1,2,3],4]";
    json = llJsonSetValue(json,[0],JSON_DELETE);
    verify("llJsonSetValue deleting array (which is first index in array)",json,u"[4]");
    json = llJsonSetValue(json,[0],JSON_DELETE);
    verify("llJsonSetValue deleting last element in array",json,u"[]");
    json = u"[[1]]";
    json = llJsonSetValue(json,[0,0],JSON_DELETE);
    verify("llJsonSetValue deleting last element in array",json,u"[[]]");
    json = llJsonSetValue(json,[0],JSON_DELETE);
    verify("llJsonSetValue deleting array within array",json,u"[]");

    # Test failures in deleting
    json = u"[[1,2,3],{\"a\":3,\"b\":[true,\"test\",6,null]}]";
    verify("llJsonSetValue deleting undefined key-value in object",llJsonSetValue(json,[1,u"d"],JSON_DELETE),JSON_INVALID);
    verify("llJsonSetValue deleting out-of-range index in array",llJsonSetValue(json,[2],JSON_DELETE),JSON_INVALID);
    verify("llJsonSetValue deleting depth within object that doesn't exist",llJsonSetValue(json,[1,u"a",u"unicorn"],JSON_DELETE),JSON_INVALID);
    verify("llJsonSetValue deleting depth within array that doesn't exist",llJsonSetValue(json,[0,1,1],JSON_DELETE),JSON_INVALID);

    # this is the only failure mode that should exist.
    json = u"[[1,2,3],{\"a\":3,\"b\":[true,\"foo\",6]}]";
    json = llJsonSetValue(json,[3],JSON_FALSE);
    verify("llJsonSetValue fail to insert data into invalid array index (MAINT-2675)",json,JSON_INVALID);

def test_json_to_list():
    l = llJson2List(u"[[1,2,3],{\"a\":3,\"b\":[true,\"test\",6]}]");
    verify_list("llJson2List first",l,[u"[1,2,3]",u"{\"a\":3,\"b\":[true,\"test\",6]}"]);
    n = llJson2List(llList2String(l,0));
    verify_list("llJson2List l,0",n,[1,2,3]);
    n = llJson2List(llList2String(l,1));
    verify_list("llJson2List l,1",n,[u"a",3,u"b",u"[true,\"test\",6]"]);
    n = llJson2List(llList2String(n,3));
    verify_list("llJson2List n,3",n,[JSON_TRUE, u"test", 6]);
    n = llJson2List(llList2String(n,1));
    verify_list("llJson2List n,1",n,[u"test"]);
    n = llJson2List(u"");
    verify_list("Empty JSON string becomes empty list",n,[]);
    n = llJson2List(u"[]");
    verify_list("Empty JSON array becomes empty list (MAINT-2678)",n,[]);
    n = llJson2List(u"{}");
    verify_list("Empty JSON object becomes empty list (MAINT-2678)",n,[]);
    n = llJson2List(u"Non-JSON string, with comma");
    verify_list("llJson2List for non-JSON string is stored as a single object",n,[u"Non-JSON string, with comma"]);
    n = llJson2List(u"[malformed}");
    verify_list("llJson2List, malformed input",n,[u"[malformed}"]);

def test_list_to_json():
    TRUE=1
    # test objects
    json = llList2Json(JSON_OBJECT,[u"a",1,u"b",2.5,u"c",u"test",u"d",u"true",u"e",u"[1,2,3]"]);
    verify("llList2Json, json object",json,u"{\"a\":1,\"b\":2.500000,\"c\":\"test\",\"d\":true,\"e\":[1,2,3]}");

    # test arrays
    json = llList2Json(JSON_ARRAY,[1,2.5,u"test",u"true",u"[1,2,3]"]);
    verify("llList2Json, json array",json,u"[1,2.500000,\"test\",true,[1,2,3]]");

    # test arrays
    json = llList2Json(JSON_ARRAY,[1,2.5,u"test",JSON_TRUE,u"[1,2,3]"]);
    verify("llList2Json, json array, alternative true representation",json,u"[1,2.500000,\"test\",true,[1,2,3]]");

    # test objects, with empty input
    json = llList2Json(JSON_OBJECT,[]);
    verify("llList2Json, json object with empty input (MAINT-2681)",json,u"{}");

    # test arrays, with empty input
    json = llList2Json(JSON_ARRAY,[]);
    verify("llList2Json, json array with empty input (MAINT-2681)",json,u"[]");

    # test objects which are truncated
    json = llList2Json(JSON_OBJECT,[u"a",1,u"b",2.5,u"c",u"test",u"d",u"true",u"e"]);
    verify("llList2Json, json object, truncated",json,JSON_INVALID);

    # test objects which has a non-string identifier somewhere
    json = llList2Json(JSON_OBJECT,[u"a",1,TRUE,2.5,u"c",u"test",u"d",u"true",u"e"]);
    verify("llList2Json, json object, non-string in one of the first stride values",json,JSON_INVALID);

    # test invalid type
    json = llList2Json(u"foo",[u"a",1,u"b",2.5,u"c",u"test",u"d",u"true",u"e",u"[1,2,3]"]);
    verify("llList2Json, json invalid type",json,JSON_INVALID);

def test_strings_with_escaped_chars():
    escaped_pairs = [
        (u"funky\"string", u"funky\\\"string", "quote in middle"),
        (u"funkystr\"ing", u"funkystr\\\"ing", "quote in middle, other position"),
        # note that we have to double-up backslashes to assign them to strings..
        (u"funky\\string", u"funky\\\\string", "backslashes in middle"),
        (u"\\funkystring", u"\\\\funkystring", "backslashes at beginning"),
        (u"funky\nstring", u"funky\\nstring", "newline in string"),
        (u"funky/string", u"funky\\/string", "forward slash in string"),
        # TAB (\t) fails, because it seems that LSL automatically converts any tab into 4 consecutive spaces.
          (u"funky\tstring", u"funky\\tstring", "tab in string"), # we enable it, with some mods
        # these cases fail; it seems that LSL doesn't support these characters, and strips the '\'
          (u"funky\x08string", u"funky\\bstring", "backspace in middle"), # we enable it, with some mods
          (u"funky\x0Cstring", u"funky\\fstring", "form feed in middle"), # we enable it, with some mods
          (u"funky\rstring", "funky\\rstring", "carriage return in string"), # we enable it, with some mods
        # note that the following case can't be supported, since strings starting with \" can't be escaped
          (u"\"funkystring", u"\\\"funkystring", "quote in beginning"), # we enable it as it's supported
        (u"vanilla string", u"vanilla string", "nothing that needs to be escaped..")
    ];
    for funky_string, funky_string_escaped, escaped_desc1 in escaped_pairs:
        escaped_desc = " '" + escaped_desc1 + "'"

        verify("Type of string with escaped char (for MAINT-2698),"+escaped_desc,llJsonValueType(u"\""+funky_string_escaped+u"\"",[]),JSON_STRING);

        json = u"[[1,2,3,4.0],{\""+funky_string_escaped+u"\":3,\"b\":\""+funky_string_escaped+u"value\"}]";
        verify("llJsonGetValue [1,\""+str(funky_string_escaped)+"\"] (for MAINT-2698),"+escaped_desc,
            llJsonGetValue(json,[1,funky_string]),"3");
        verify("llJsonGetValue [1,\"b\"] (for MAINT-2698),"+escaped_desc,llJsonGetValue(json,[1,u"b"]),funky_string+u"value");

        #llSay(0, "DEBU G: '" + llEscapeURL(json) + "' is input for test " + escaped_desc);
        json = llJsonSetValue(json,[0],funky_string);
        verify("llJsonSetValue with escaped string as value (for MAINT-2698),"+escaped_desc,json,
            u"[\""+funky_string_escaped+u"\",{\""+funky_string_escaped+u"\":3,\"b\":\""+funky_string_escaped+u"value\"}]");

        json = llJsonSetValue(json,[0],funky_string);
        verify("llJsonSetValue with escaped string as value (for MAINT-2698),"+escaped_desc,json,
            u"[\""+funky_string_escaped+u"\",{\""+funky_string_escaped+u"\":3,\"b\":\""+funky_string_escaped+u"value\"}]");

        json = llJsonSetValue(json,[0,funky_string], funky_string+u"value");
        verify("llJsonSetValue with escaped string as key's value (for MAINT-2698),"+escaped_desc,json,
            u"[{\""+funky_string_escaped+u"\":\""+funky_string_escaped+u"value\"},{\""+funky_string_escaped+u"\":3,\"b\":\""+funky_string_escaped+u"value\"}]");

        l = llJson2List(json);
        verify_list("llJson2List extracting object containing escaped string (for MAINT-2698),"+escaped_desc, l,
            [u"{\""+funky_string_escaped+u"\":\""+funky_string_escaped+u"value\"}",u"{\""+funky_string_escaped+u"\":3,\"b\":\""+funky_string_escaped+u"value\"}"]);
        n = llJson2List(llList2String(l, 0));
        verify_list("llJson2List extracting escaped strings (for MAINT-2698),"+escaped_desc, n,
            [funky_string,funky_string+u"value"]);

        json = llList2Json(JSON_ARRAY,n);
        verify("llList2Json from escaped-containing string to array (for MAINT-2698),"+escaped_desc,json,
            u"[\""+funky_string_escaped+u"\",\""+funky_string_escaped+u"value\"]");

        json = llList2Json(JSON_OBJECT,n);
        verify("llList2Json from escaped-containing string to object (for MAINT-2698),"+escaped_desc,json,
            u"{\""+funky_string_escaped+u"\":\""+funky_string_escaped+u"value\"}");

def maint3070():
    verify("Set value 'messa[g]e'", llJsonSetValue(u"",[u"toast"],u"messa[g]e"), u"{\"toast\":\"messa[g]e\"}");
    verify("Set value 'messag[e]'", llJsonSetValue(u"",[u"toast"],u"messag[e]"), u"{\"toast\":\"messag[e]\"}");
    #verify("Set value 'messag\[e\]'", llJsonSetValue(u"",[u"toast"],u"messag\[e\]"), u"{\"toast\":\"messag[e]\"}");
    # ^^ BROKEN!!!!! LSL does not include the backslashes in the strings, so the above test is redundant.
    # Python does, thus making that test would break it. We include our own test tho:
    \
verify("Set value 'messag\\[e\\]'", llJsonSetValue(u"",[u"toast"],u"messag\\[e\\]"), u"{\"toast\":\"messag\\\\[e\\\\]\"}");

def maint4187():
    verify("Valid json number with + before exponent", llJsonValueType(u"1.0e+1", []), JSON_NUMBER);
    verify("Valid json number with - before exponent", llJsonValueType(u"1.0e-1", []), JSON_NUMBER);
    verify("Valid json number with - before exponent and mantissa", llJsonValueType(u"-1.0e-1", []), JSON_NUMBER);
    verify("Valid json number with unsigned exponent", llJsonValueType(u"1.0e1", []), JSON_NUMBER);
    verify("Invalid json number due to + before mantissa", llJsonValueType(u"+1.0e1", []), JSON_INVALID);

    verify("Invalid json number due to leading e", llJsonValueType(u"e1", []), JSON_INVALID);
    verify("Invalid json number due to leading 0", llJsonValueType(u"01", []), JSON_INVALID);
    verify("Invalid json number due to leading -0", llJsonValueType(u"-01", []), JSON_INVALID);
    verify("Valid json number with 0 immediately before .", llJsonValueType(u"0.01", []), JSON_NUMBER);
    verify("Valid json number with -0 immediately before .", llJsonValueType(u"-0.01", []), JSON_NUMBER);

def maint3053():
    jT1 = u"[1, 2]"; # A JSON array
    verify("llJsonSetValue(jT1,[2],\"t\")",llJsonSetValue(jT1,[2],u"t"),u"[1,2,\"t\"]");
    verify("llJsonSetValue(jT1,[3],\"t\")",llJsonSetValue(jT1,[3],u"t"),JSON_INVALID);
    verify("llJsonSetValue(jT1,[0, 0],\"t\")",llJsonSetValue(jT1,[0, 0],u"t"),u"[[\"t\"],2]");
    verify("llJsonSetValue(jT1,[0, 0, 2, \"t\", 75],\"t\")",llJsonSetValue(jT1,[0, 0, 2, u"t", 75],u"t"),JSON_INVALID);
    verify("llJsonSetValue(jT1,[0, 1],\"t\")",llJsonSetValue(jT1,[0, 1],u"t"),JSON_INVALID);
    verify("llJsonSetValue(jT1,[0, 1, 2, \"t\", 75],\"t\")",llJsonSetValue(jT1,[0, 1, 2, u"t", 75],u"t"),JSON_INVALID);

    jT2 = u"[ [\"A\", \"B\", \"C\"], 2]";
    verify("llJsonSetValue(jT2,[0, 3],\"t\")",llJsonSetValue(jT2,[0, 3],u"t"),u"[[\"A\",\"B\",\"C\",\"t\"],2]");
    verify("llJsonSetValue(jT2,[0, 4],\"t\")",llJsonSetValue(jT2,[0, 4],u"t"),JSON_INVALID);
    verify("llJsonSetValue(jT2,[0, 1, 0],\"t\")",llJsonSetValue(jT2,[0, 1, 0],u"t"),u"[[\"A\",[\"t\"],\"C\"],2]");
    verify("llJsonSetValue(jT2,[0, 1, 1],\"t\")",llJsonSetValue(jT2,[0, 1, 1],u"t"),JSON_INVALID);

    jT3 = u"{\"1\":2}";
    verify("llJsonSetValue(jT3,[\"1\"],\"t\")",llJsonSetValue(jT3,[u"1"],u"t"),u"{\"1\":\"t\"}");
    verify("llJsonSetValue(jT3,[\"1\",0],\"t\")",llJsonSetValue(jT3,[u"1",0],u"t"),u"{\"1\":[\"t\"]}");
    verify("llJsonSetValue(jT3,[\"1\",1],\"t\")",llJsonSetValue(jT3,[u"1",1],u"t"),JSON_INVALID);

    jGood = u"[null, 2]";
    verify("llJsonValueType(jGood, [0])",llJsonValueType(jGood, [0]),JSON_NULL);
    verify("llJsonValueType(jGood, [0, 0])",llJsonValueType(jGood, [0, 0]),JSON_INVALID);

    jBad = u"[, 2]";
    verify("llJsonValueType(jBad,[0])",llJsonValueType(jBad,[0]),JSON_INVALID);
    verify("llJsonValueType(jBad,[0, 0, 2, \"t\", 75])",llJsonValueType(jBad,[0, 0, 2, u"t", 75]),JSON_INVALID);
    verify("llJsonGetValue(jBad,[1, 0, 2, \"t\", 75])",llJsonGetValue(jBad,[1, 0, 2, u"t", 75]),JSON_INVALID);

def maint3081():
    verify("llJsonSetValue blank string",llJsonSetValue(u"",[u"test"],u""),u"{\"test\":\"\"}");
    verify("llJsonSetValue JSON_NULL",llJsonSetValue(u"",[u"test"],JSON_NULL),u"{\"test\":null}");
    verify("llJsonGetValue blank string",llJsonGetValue(u"{\"test\":\"\"}",[u"test"]),u"");
    verify("llJsonGetValue JSON_NULL",llJsonGetValue(u"{\"test\":null}",[u"test"]),JSON_NULL);
    verify("Identity (set->get) blank string",llJsonGetValue(llJsonSetValue(u"",[u"test"],u""),[u"test"]),u"");
    verify("Identity (set->get) JSON_NULL",llJsonGetValue(llJsonSetValue(u"",[u"test"],JSON_NULL),[u"test"]),JSON_NULL);

def test_jira_fixes():
    # FIXME: llJsonSetValue pending
    #maint3070();
    if 6466 not in Bugs:
        maint4187();
    # FIXME: llJsonSetValue pending
    #maint3053();
    # FIXME: llJsonSetValue pending
    #maint3081();

# End JSON tests from wiki

def do_tests():
    # Test our own test function for NaNs
    test('reallyequal(NaN, Indet, 0.0)', False)
    test('reallyequal(-NaN, Indet, 0.0)', True)
    test('reallyequal(NaN, -Indet, 0.0)', True)
    test('reallyequal(NaN, -(Infinity*0), 0.0)', True)
    test('reallyequal(Infinity*0, Indet, 0.0)', True)
    # Check that it properly distinguishes -0.0 and 0.0
    test('reallyequal(-0.0, 0.0, 0.0)', False)
    test('reallyequal(0.0, -0.0, 0.0)', False)
    test('reallyequal(0.0, 0.0, 0.0)', True)
    test('reallyequal(-0.0, -0.0, 0.0)', True)

    shouldexcept('div(1.0, 0.0)', ELSLMathError)
    shouldexcept('div(1, 0)', ELSLMathError)
    shouldexcept('div(NaN, 1)', ELSLMathError)
    shouldexcept('div(1, NaN)', ELSLMathError)
    shouldexcept('div(F32(1e40), F32(1e40))', ELSLMathError)
    shouldexcept('zstr(b"blah")', ELSLInvalidType)
    test('div(1, 9)', 0)
    test('div(8, 9)', 0)
    test('div(9, 9)', 1)
    test('div(-1,9)', 0)
    test('div(-8,9)', 0)
    test('div(-9,9)', -1)
    test('div(1,-9)', 0)
    test('div(8,-9)', 0)
    test('div(9,-9)', -1)
    test('less(Infinity, Infinity)', 0)
    test('less(-Infinity, Infinity)', 1)
    test(r'zstr(Key(u"xy\0zzy"))', Key(u'xy'))
    test('typecast(Infinity, unicode)', u'Infinity')
    test('typecast(NaN, unicode)', u'NaN')
    test('typecast(Vector((NaN,-Infinity,-0.)), unicode)', u'<NaN, -Infinity, -0.00000>')
    test('typecast(Vector((NaN,-Infinity,-0.)), unicode, True, True)', u'<NaN, -Infinity, -0.000000>')
    test('typecast(Vector((NaN,2.,3.)), list)', [Vector((NaN,2.,3.))])
    test('typecast([Vector((NaN,2.,-Infinity))], list)', [Vector((NaN,2.,-Infinity))])

    shouldexcept('typecast(1.2, Vector)', ELSLTypeMismatch)
    shouldexcept('typecast(1, Vector)', ELSLTypeMismatch)
    shouldexcept('typecast(Vector((1.,2.,3.)), Key)', ELSLTypeMismatch)
    shouldexcept('typecast(Quaternion((1.,2.,3.,0.)), Key)', ELSLTypeMismatch)
    shouldexcept('typecast(Key(u""), Vector)', ELSLTypeMismatch)
    test('typecast(Key(u"xyz"), unicode)', u'xyz')
    test('typecast(u"xyz", Key)', Key(u'xyz'))
    test('typecast(u"3.14e+0a", int)', 3)
    test('typecast(u"a3.14e+0a", int)', 0)
    test('typecast(u"0XA3.14e+0a", int)', 0xA3)
    test('typecast(u"3333333333333", int)', -1)
    test('typecast(u"4124567890", int)', -170399406)
    test('typecast(u"-4124567890", int)', 170399406)
    test('typecast(u"3.14e+0a", float)', F32(3.14))
    test('typecast(u"++3.14e+0a", float)', F32(0.))
    test('typecast(u"0x3.14p+0a", float)', F32(float.fromhex('0x3.14p0')))
    test('typecast(u"<5.31,7.13,0x9.99", Vector)', F32(Vector((5.31,7.13,float.fromhex('0x9.99')))))
    test('typecast(u"<5.31, 7.13, 0x9.99>", Vector)', F32(Vector((5.31,7.13,float.fromhex('0x9.99')))))
    test('typecast(u"<5.31 , 7.13 , 0x9.99>", Vector)', ZERO_VECTOR)
    test('typecast(u"5.31, 7.13, 0x9.99>", Vector)', ZERO_VECTOR)
    test('typecast(u"<5.31, a7.13, 0x9.99>", Vector)', ZERO_VECTOR)
    test('typecast(u"<1,1,2+", Vector)', Vector((1.,1.,2.)))
    test('typecast(u"<1,1,2a", Vector)', Vector((1.,1.,2.)))
    test('typecast(u"<1,1,inf", Vector)', Vector((1.,1.,Infinity)))
    test('typecast(u"<1,1,infi", Vector)', ZERO_VECTOR)
    test('typecast(u"<1,1,infix", Vector)', ZERO_VECTOR)
    test('typecast(u"<1,1,infinity", Vector)', Vector((1.,1.,Infinity)))
    test('neg(-2147483648)',-2147483648)
    test('neg(2147483647)',-2147483647)
    test('neg(Quaternion((-.5,.5,.4,-.4)))',Quaternion((.5,-.5,-.4,.4)))
    shouldexcept('neg(u"3")', ELSLTypeMismatch)
    test('add(-2147483648,-1)', 2147483647)
    test('add(-2147483648.,-1.)', -2147483648.)
    test('add(-2147483648.,-1)', -2147483648.)
    test('add(-2147483648,-1.)', -2147483648.)
    test('add(Vector((1.,2.,3.)),Vector((2.,4.,6.)))', Vector((3.,6.,9.)))
    test('add(Quaternion((1.,2.,3.,4.)),Quaternion((2.,4.,6.,8.)))', Quaternion((3.,6.,9.,12.)))
    test('add([u"1"],[u"2"])', [u'1',u'2'])
    test('add(u"1",[u"2"])', [u'1',u'2'])
    test('add([u"1"],u"2")', [u'1',u'2'])
    test('add(u"1",u"2")', u'12')
    test('sub(1,2)', -1)
    test('sub(1.,2)', -1.)
    test('sub(1,2.)', -1.)
    test('sub(1.,2.)', -1.)
    test('sub(-2147483647,2)', 2147483647)
    test('sub(Vector((1.,2.,3.)),Vector((2.,4.,6.)))', Vector((-1.,-2.,-3.)))
    test('sub(Quaternion((1.,2.,3.,4.)),Quaternion((2.,4.,6.,8.)))', Quaternion((-1.,-2.,-3.,-4.)))
    test('mul(2.,Vector((1.,2.,3.)))', Vector((2.,4.,6.)))
    test('mul(Quaternion((.5,.5,.5,.5)),Quaternion((1.,0.,0.,0.)))', Quaternion((.5,-.5,.5,-.5)))
    test('mul(1,2)', 2)
    test('mul(1.,2)', 2.)
    test('mul(1,2.)', 2.)
    test('mul(1.,2.)', 2.)
    test('mul(Vector((3.,4.,5.)),Vector((3.,4.,5.)))', 50.)
    test('mul(Vector((3.,4.,5.)),1.)', Vector((3.,4.,5.)))
    test('mul(Vector((3.,4.,5.)),ZERO_ROTATION)', Vector((3.,4.,5.)))
    test('mul(Vector((3.,4.,5.)),Quaternion((.5,.5,.5,.5)))', Vector((5.,3.,4.)))
    shouldexcept('add(Key(u"1"),Key(u"2"))', ELSLTypeMismatch)
    shouldexcept('sub(u"1",u"2")', ELSLTypeMismatch)
    shouldexcept('mul(u"1",u"2")', ELSLTypeMismatch)
    shouldexcept('mul(1.,Quaternion((1.,2.,3.,4.)))', ELSLTypeMismatch)
    shouldexcept('mul(Quaternion((1.,2.,3.,4.)),1.)', ELSLTypeMismatch)
    shouldexcept('mul(Quaternion((1.,2.,3.,4.)),Vector((1.,2.,3.)))', ELSLTypeMismatch)
    shouldexcept('mul(b"a",3)', ELSLInvalidType)
    shouldexcept('mul(Vector((3.,4.,5.)),b"a")', ELSLInvalidType)
    shouldexcept('typecast([1,F32(3.14),Key(u"blah"),Quaternion((1.,0.,0.,0.))], Vector)',
        ELSLTypeMismatch)
    shouldexcept('typecast(b"", unicode)', ELSLInvalidType)


    test('''llListSort(
        [Quaternion((1.,2.,3.,4.)),Quaternion((2.,3.,4.,5.)),Quaternion((5.,4.,3.,2.)),
        Quaternion((4.,3.,2.,1.)),Quaternion((3.,2.,1.,0.))]
        ,1,0)''',
        [Quaternion((3.,2.,1.,0.)),Quaternion((4.,3.,2.,1.)),Quaternion((5.,4.,3.,2.)),
        Quaternion((2.,3.,4.,5.)),Quaternion((1.,2.,3.,4.))]
    )

    test('''llListSort(
        [Quaternion((1.,2.,3.,4.)),Quaternion((2.,3.,4.,5.)),Quaternion((5.,4.,3.,2.)),
        Quaternion((4.,3.,2.,1.)),Quaternion((3.,2.,1.,0.))]
        ,1,1)''',
        [Quaternion((1.,2.,3.,4.)),Quaternion((2.,3.,4.,5.)),Quaternion((5.,4.,3.,2.)),
        Quaternion((4.,3.,2.,1.)),Quaternion((3.,2.,1.,0.))]
    )

    test('''llListSort(
        [Vector((1.,0.,0.)),Vector((0.,3.,0.)),Vector((0.,0.,1.)),Vector((3.,0.,0.))]
        ,1,1)''',
        [Vector((1.0, 0.0, 0.0)), Vector((0.0, 0.0, 1.0)), Vector((0.0, 3.0, 0.0)), Vector((3.0, 0.0, 0.0))]
    )

    test('''llListSort([2,0,1,1,2,2,2,3,2,4,1,5,2,6], 2, 1)''',
        [1, 1, 1, 5, 2, 2, 2, 3, 2, 4, 2, 0, 2, 6])
    test('''llListSort([2,0,1,1,2,2,2,3,2,4,1,5,2,6], 2, 0)''',
        [2, 6, 2, 4, 2, 3, 2, 2, 2, 0, 1, 5, 1, 1])
    test('''llListSort([2,0,1,1,2,2,2,3,2,4,1,5,2,6,3], 2, 1)''',
        [2, 0, 1, 1, 2, 2, 2, 3, 2, 4, 1, 5, 2, 6, 3])

    # NaN in sort behaves strangely. Also when inside vectors.
    test('llListSort([-1., 9., 3., 2., NaN, 5., 1.], 1, 1)', [1., 5., NaN, -1., 2., 3., 9.])
    test('llListSort([Vector((2.,0.,0.)),Vector((1.,NaN,0.))],1,1)', [Vector((1.,NaN,0.)),Vector((2.,0.,0.))])
    test('llListSort([Vector((1.,NaN,0.)),Vector((2.,0.,0.))],1,1)', [Vector((2.,0.,0.)),Vector((1.,NaN,0.))])
    test('llListSort([Vector((2.,0.,0.)),Vector((1.,NaN,0.))],1,0)', [Vector((2.,0.,0.)),Vector((1.,NaN,0.))])
    test('llListSort([Vector((1.,NaN,0.)),Vector((2.,0.,0.))],1,0)', [Vector((1.,NaN,0.)),Vector((2.,0.,0.))])
    # This proves that it does not sort by UTF-16 words, but by code points.
    # Otherwise u"\U0001d41a" would be before u"\ufb01" (because the UTF-16
    # of u"\U0001d41a" is 0xD835 0xDC1A)
    test(r'llListSort([u"\ufb01",u"\xe1", u"\U0001d41a", u"a"], 1, 1)', [u'a',u'\xe1',u'\ufb01',u'\U0001d41a'])

    test('llLog(NaN)', 0.)
    test('llLog(-Infinity)', 0.)
    test('llLog(Infinity)', Infinity)
    test('llLog(-1.)', 0.)
    test('llLog(-0.)', 0.)
    test('llLog(1.)', 0.)
    test('llLog(F32(math.e))', F('0x1.FFFFFEp-1'))
    test('llLog10(NaN)', 0.)
    test('llLog10(-Infinity)', 0.)
    test('llLog10(Infinity)', Infinity)
    test('llLog10(-1.)', 0.)
    test('llLog10(-0.)', 0.)
    test('llLog10(1.)', 0.)
    test('llLog10(100.)', 2.)
    test('llLog(Infinity)', Infinity)

    test('F32(1e38)', 99999996802856924650656260769173209088.)
    test('''llList2CSV([F32(1e0),F32(1e1),F32(1e2),F32(1e3),F32(1e4),F32(1e5),F32(1e6),
            F32(1e7),F32(1e8),F32(1e9),F32(1e10),F32(1e11),F32(1e12),F32(1e13),
            F32(1e14),F32(1e15),F32(1e16),F32(1e17),F32(1e18),F32(1e19),F32(1e20),
            F32(1e21),F32(1e22),F32(1e23),F32(1e24),F32(1e25),F32(1e26),F32(1e27),
            F32(1e28),F32(1e29),F32(1e30),F32(1e31),F32(1e32),F32(1e33),F32(1e34),
            F32(1e35),F32(1e36),F32(1e37),F32(1e38)])''',
        u"1.000000, 10.000000, 100.000000, 1000.000000, 10000.000000, 100000.000000, "
        u"1000000.000000, 10000000.000000, 100000000.000000, 1000000000.000000, "
        u"10000000000.000000, 99999997952.000000, 999999995904.000000, "
        u"9999999827968.000000, 100000000376832.000000, 999999986991104.000000, "
        u"10000000272564224.000000, 99999998430674944.000000, "
        u"999999984306749440.000000, 9999999980506447872.000000, "
        u"100000002004087734272.000000, 1000000020040877342720.000000, "
        u"9999999778196308361216.000000, 99999997781963083612160.000000, "
        u"1000000013848427855085568.000000, 9999999562023526247432192.000000, "
        u"100000002537764290115403776.000000, 999999988484154753734934528.000000, "
        u"9999999442119689768320106496.000000, "
        u"100000001504746621987668885504.000000, "
        u"1000000015047466219876688855040.000000, "
        u"9999999848243207295109594873856.000000, "
        u"100000003318135351409612647563264.000000, "
        u"999999994495727286427992885035008.000000, "
        u"9999999790214767953607394487959552.000000, "
        u"100000004091847875962975319375216640.000000, "
        u"999999961690316245365415600208216064.000000, "
        u"9999999933815812510711506376257961984.000000, "
        u"99999996802856924650656260769173209088.000000")

    test('llDumpList2String([F32(1e11)], u"/")', u'100000000000.000000')

    # Inputs
    inp = [F32(i) for i in [1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10, 1e11, 1e12, 1e13, 1e14,
            1e15, 1e16, 1e17, 1e18, 1e19, 1e20, 1e21, 1e22, 1e23, 1e24, 1e25, 1e26, 1e27, 1e28, 1e29, 1e30,
            1e31, 1e32, 1e33, 1e34, 1e35, 1e36, 1e37, 1e38]]

    # Expected
    exp = [
        u'1.000000',u'10.000000',u'100.000000',u'1000.000000',u'10000.000000',u'100000.000000',
        u'1000000.000000',u'10000000.000000',u'100000000.000000',u'1000000000.000000',u'10000000000.000000',
        u'100000000000.000000',u'1000000000000.000000',u'10000000000000.000000',u'100000000000000.000000',
        u'1000000000000000.000000',u'10000000000000000.000000',u'100000000000000000.000000',
        u'1000000000000000000.000000',u'10000000000000000000.000000',u'100000000000000000000.000000',
        u'1000000000000000000000.000000',u'10000000000000000000000.000000',
        u'100000000000000000000000.000000',u'1000000000000000000000000.000000',
        u'10000000000000000000000000.000000',u'100000000000000000000000000.000000',
        u'1000000000000000000000000000.000000',u'9999999000000000000000000000.000000',
        u'100000000000000000000000000000.000000',u'1000000000000000000000000000000.000000',
        u'10000000000000000000000000000000.000000',u'100000000000000000000000000000000.000000',
        u'1000000000000000000000000000000000.000000',u'10000000000000000000000000000000000.000000',
        u'100000000000000000000000000000000000.000000',u'1000000000000000000000000000000000000.000000',
        u'10000000000000000000000000000000000000.000000',u'100000000000000000000000000000000000000.000000'
        ]

    for i in range(len(inp)):
        test('typecast('+repr(inp[i])+',unicode)', exp[i])

    # Mono mode
    test('f2s(F32(-.5e-6), 6)', u'-0.000001')
    test('f2s(F("-0x1.0C6F7Ap-21"), 6)', u'-0.000001') # = F32(-.5e-6) so this test is redundant
    test('f2s(F("0x1.0C6F7Ap-21"), 6)', u'0.000001')
    test('f2s(F("-0x1.0C6F78p-21"), 6)', u'0.000000')
    test('f2s(F("0x1.0C6F78p-21"), 6)', u'0.000000')
    test('f2s(F32(.5e-5), 5)', u'0.00001')
    test('f2s(F("-0x1.4F8B58p-18"), 5)', u'-0.00001') # = F32(-.5e-5) so this test is redundant
    test('f2s(F("0x1.4F8B58p-18"), 5)', u'0.00001')
    test('f2s(F("-0x1.4F8B56p-18"), 5)', u'0.00000')
    test('f2s(F("0x1.4F8B56p-18"), 5)', u'0.00000')

    test('f2s(F32(-123456789.), 6)', u'-123456800.000000')
    test('f2s(F32(-123456784.), 6)', u'-123456800.000000')

    test('f2s(F32(-123456740.), 6)', u'-123456700.000000')
    test('f2s(F32(-12345.674), 6)', u'-12345.670000')
    test('f2s(F32(-1.2345674), 6)', u'-1.234567')
    test('f2s(F32(-1.2345675), 6)', u'-1.234568')
    test('f2s(F32(-123456740.), 5)', u'-123456700.00000')
    test('f2s(F32(-12345.674), 5)', u'-12345.67000')
    test('f2s(F32(-1.2345674), 5)', u'-1.23457')
    test('f2s(F32(-1.234564), 5)', u'-1.23456')

    test('f2s(F32(-123456750.), 6)', u'-123456800.000000')
    test('f2s(F32(-12345.675), 6)', u'-12345.670000') # I kid you not - that's from LSL
    test('f2s(F32(-12345.676), 6)', u'-12345.680000')
    test('f2s(F32(-123456750.), 5)', u'-123456800.00000')
    test('f2s(F32(-12345.675), 5)', u'-12345.67000') # I kid you not - that's from LSL
    test('f2s(F32(-12345.676), 5)', u'-12345.68000')
    test('f2s(F32(-12.345675), 5)', u'-12.34568')
    test('f2s(F32(1234567.5), 5)', u'1234568.00000')
    test('f2s(F32(1234567.5), 6)', u'1234568.000000')
    test('f2s(F32(1234567.4), 5)', u'1234567.00000')
    test('f2s(F32(1234567.4), 6)', u'1234567.000000')
    test('f2s(F32(123456.75), 5)', u'123456.80000')
    test('f2s(F32(123456.74), 5)', u'123456.70000')
    test('f2s(F32(12345675.), 5)', u'12345680.00000')
    test('f2s(F32(12345674.), 5)', u'12345670.00000')
    test('f2s(F32(9.999999), 5)', u'10.00000')

    # LSO mode
    lslcommon.LSO = True
    test('f2s(F32(-.5e-6), 6)', u'-0.000000')
    test('f2s(F32(-.51e-6), 6)', u'-0.000001')
    test('f2s(F("-0x1.0C6F7Ap-21"), 6)', u'-0.000000') # = F32(-.5e-6) so this test is redundant
    test('f2s(F("0x1.0C6F7Ap-21"), 6)', u'0.000000')
    test('f2s(F("-0x1.0C6F7Cp-21"), 6)', u'-0.000001')
    test('f2s(F("0x1.0C6F7Cp-21"), 6)', u'0.000001')
    test('f2s(F32(.5e-5), 5)', u'0.00000')
    test('f2s(F("-0x1.4F8B58p-18"), 5)', u'-0.00000') # = F32(-.5e-5) so this test is redundant
    test('f2s(F("0x1.4F8B58p-18"), 5)', u'0.00000')
    test('f2s(F("-0x1.4F8B5Ap-18"), 5)', u'-0.00001')
    test('f2s(F("0x1.4F8B5Ap-18"), 5)', u'0.00001')

    test('f2s(F32(-123456789.), 6)', u'-123456792.000000')
    test('f2s(F32(-123456784.), 6)', u'-123456784.000000')

    test('f2s(F32(-123456740.), 6)', u'-123456736.000000')
    test('f2s(F32(-12345.674), 6)', u'-12345.673828')
    test('f2s(F32(-1.2345674), 6)', u'-1.234567')
    test('f2s(F32(-1.2345675), 6)', u'-1.234568')
    test('f2s(F32(-123456740.), 5)', u'-123456736.00000')
    test('f2s(F32(-12345.674), 5)', u'-12345.67383')
    test('f2s(F32(-1.2345674), 5)', u'-1.23457')
    test('f2s(F32(-1.234564), 5)', u'-1.23456')

    test('f2s(F32(-123456750.), 6)', u'-123456752.000000')
    test('f2s(F32(-12345.675), 6)', u'-12345.674805')
    test('f2s(F32(-12345.676), 6)', u'-12345.675781')
    test('f2s(F32(-123456750.), 5)', u'-123456752.00000')
    test('f2s(F32(-12345.675), 5)', u'-12345.67480')
    test('f2s(F32(-12345.676), 5)', u'-12345.67578')
    test('f2s(F32(-12.345675), 5)', u'-12.34568')
    test('f2s(F32(1234567.5), 5)', u'1234567.50000')
    test('f2s(F32(1234567.5), 6)', u'1234567.500000')
    test('f2s(F32(1234567.4), 5)', u'1234567.37500')
    test('f2s(F32(1234567.4), 6)', u'1234567.375000')
    test('f2s(F32(123456.75), 5)', u'123456.75000')
    test('f2s(F32(123456.74), 5)', u'123456.74219')
    test('f2s(F32(123456.74), 6)', u'123456.742188')
    test('f2s(F32(12345675.), 5)', u'12345675.00000')
    test('f2s(F32(12345674.), 5)', u'12345674.00000')

    lslcommon.LSO = False

    # llPow() input matrix
    in1 = in2 = [NaN,-Infinity,F32(-2.1),-2.,-1.,F32(-.1),-.0,.0,F32(.1),1.,2.,F32(2.1),Infinity,Indet]

    # Expected results table
    exp = [
        NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,
        NaN,0.,0.,0.,0.,0.,1.,1.,Infinity,-Infinity,Infinity,Infinity,Infinity,NaN,
        NaN,0.,Indet,F('0x1.D0662Ep-3'),F('-0x1.E79E7Cp-2'),Indet,1.,1.,Indet,F('-0x1.0CCCCCp+1'),F('0x1.1A3D6Ep+2'),Indet,Infinity,NaN,
        NaN,0.,Indet,.25,-0.5,Indet,1.,1.,Indet,-2.,4.,Indet,Infinity,NaN,
        NaN,NaN,Indet,1.,-1.,Indet,1.,1.,Indet,-1.,1.,Indet,NaN,NaN,
        NaN,Infinity,Indet,100.,-10.,Indet,1.,1.,Indet,F('-0x1.99999Ap-4'),F('0x1.47AE16p-7'),Indet,0.,NaN,
        NaN,Infinity,Infinity,Infinity,-Infinity,Infinity,1.,1.,0.,0.,0.,0.,0.,NaN,
        NaN,Infinity,Infinity,Infinity,Infinity,Infinity,1.,1.,0.,0.,0.,0.,0.,NaN,
        NaN,Infinity,F('0x1.F791EEp+6'),100.,10.,F('0x1.4248F0p0'),1.,1.,F('0x1.96B230p-1'),F('0x1.99999Ap-4'),F('0x1.47AE16p-7'),F('0x1.04491Ap-7'),0.,NaN,
        NaN,NaN,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,NaN,NaN,
        NaN,0.,F('0x1.DDB682p-3'),.25,.5,F('0x1.DDB680p-1'),1.,1.,F('0x1.125FBEp0'),2.,4.,F('0x1.125FBEp+2'),Infinity,NaN,
        NaN,0.,F('0x1.AF30DAp-3'),F('0x1.D0662Ep-3'),F('0x1.E79E7Cp-2'),F('0x1.DB6346p-1'),1.,1.,F('0x1.13B748p0'),F('0x1.0CCCCCp+1'),F('0x1.1A3D6Ep+2'),F('0x1.2FFA0Ep+2'),Infinity,NaN,
        NaN,0.,0.,0.,0.,0.,1.,1.,Infinity,Infinity,Infinity,Infinity,Infinity,NaN,
        NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,
        ]

    idx = 0
    for i in in1:
        for j in in2:
            test('llPow(' + repr(i) + ',' + repr(j) + ')', exp[idx])
            idx += 1

    test('llCos(NaN)', NaN)
    test('llCos(Infinity)', Indet)
    test('llCos(-Infinity)', Indet)
    test('llCos(math.pi)', -1.)
    test('llCos(1000.)', F('0x1.1FF026p-1'))
    test('llCos(1000000.)', F('0x1.DF9DFAp-1'))
    test('llCos(1000000000.)', F('0x1.ACFF8Cp-1'))
    test('llCos(-1000000000.)', F('0x1.ACFF8Cp-1'))
    test('llCos(F("0x1.FFFFFEp+62"))', F('-0x1.F4E122p-1'))
    test('llCos(F("-0x1.FFFFFEp+62"))', F('-0x1.F4E122p-1'))
    test('llCos(F("0x1p63"))', F('0x1p63'))
    test('llCos(F("-0x1p63"))', F('-0x1p63'))
    test('llSin(NaN)', NaN)
    test('llSin(Infinity)', Indet)
    test('llSin(-Infinity)', Indet)
    test('llSin(F32(math.pi))', F('-0x1.777A5Cp-24'))
    test('llSin(1000.)', F('0x1.A75CC2p-1'))
    test('llSin(1000000.)', F('-0x1.6664B2p-2'))
    test('llSin(1000000000.)', F('0x1.1778CAp-1'))
    test('llSin(-1000000000.)', F('-0x1.1778CAp-1'))
    test('llSin(F32(1e38))', F32(1e38))
    test('llSin(F("0x1.FFFFFEp+62"))', F('0x1.A8862Cp-3'))
    test('llSin(F("-0x1.FFFFFEp+62"))', F('-0x1.A8862Cp-3'))
    test('llSin(F("0x1p63"))', F('0x1p63'))
    test('llSin(F("-0x1p63"))', F('-0x1p63'))
    test('llTan(F32(1e38))', F32(1e38))
    test('llTan(F32(4e38))', Indet)
    test('llTan(F32(math.pi))', F('0x1.777A5Cp-24'))
    test('llTan(F32(math.pi*.5))', -22877332.)
    test('llTan(F("0x1.921FB4p0"))', 13245402.)
    test('llTan(F("0x1.FFFFFEp62"))', F('-0x1.B1F30Ep-3'))
    test('llTan(F("-0x1.FFFFFEp62"))', F('0x1.B1F30Ep-3'))
    test('llTan(F("0x1p63"))', F('0x1p63'))
    test('llTan(F("-0x1p63"))', F('-0x1p63'))
    test('llAsin(2.0)', NaN)
    test('llAcos(2.0)', NaN)
    test('llAtan2(0.0, 0.0)', 0.0)
    test('llAtan2(-0.0, -1.0)', F32(-math.pi))
    test('llAtan2(0.0, -1.0)', F32(math.pi))
    test('llAtan2(-Infinity, -1.0)', F32(-math.pi/2))
    test('llAtan2(Infinity, -1.0)', F32(math.pi/2))
    test('llAtan2(NaN, -1.0)', NaN)
    test('llAtan2(NaN, -0.0)', NaN)
    test('llAtan2(NaN,  0.0)', NaN)
    test('llAtan2(NaN,  1.0)', NaN)
    test('llAtan2(-NaN, -1.0)', -NaN)
    test('llAtan2(-NaN, -0.0)', -NaN)
    test('llAtan2(-NaN,  0.0)', -NaN)
    test('llAtan2(-NaN,  1.0)', -NaN)
    test('llAtan2(-1.0, NaN)', NaN)
    test('llAtan2(-0.0, NaN)', NaN)
    test('llAtan2( 0.0, NaN)', NaN)
    test('llAtan2( 1.0, NaN)', NaN)
    test('llAtan2(-1.0, -NaN)', -NaN)
    test('llAtan2(-0.0, -NaN)', -NaN)
    test('llAtan2( 0.0, -NaN)', -NaN)
    test('llAtan2( 1.0, -NaN)', -NaN)
    test('llAtan2(-NaN, -NaN)', -NaN)
    test('llAtan2(-NaN, NaN)', NaN)
    test('llAtan2(NaN, -NaN)', NaN)
    test('llAtan2(NaN, NaN)', NaN)

    # nan and -nan in llList2CSV
    test('llList2CSV([llSin(F32(4e38))])', u'-nan')
    test('llList2CSV([llCos(F32(4e38))])', u'-nan')
    test('llList2CSV([llTan(F32(4e38))])', u'-nan')
    test('llList2CSV([llSqrt(F32(-1))])', u'-nan')
    test('llList2CSV([llPow(-1.0,F32(1.3))])', u'-nan')
    test('llList2CSV([llPow(nan,F32(1.3))])', u'nan')
    test('llList2CSV([Vector((-nan,nan,-inf))])', u'<-nan, nan, -inf>')

    test('llFrand(0.0)', 0.0)
    test('llFrand(-0.0)', 0.0)
    test('llFrand(Infinity)', 0.0)
    test('llFrand(-Infinity)', 0.0)
    test('llFrand(-NaN)', -NaN)
    test('llFrand(NaN)', NaN)
    test('llFrand(F32(1.4e-45))', 0.0)
    test('llFrand(F32(1.1754942106924411e-38))', 0.0)

    test('llRot2Fwd(Quaternion((1.,0.,0.,0.)))', Vector((1.,0.,0.)))
    test('llRot2Fwd(Quaternion((0.,1.,0.,0.)))', Vector((-1.,0.,0.)))
    test('llRot2Fwd(Quaternion((0.,0.,1.,0.)))', Vector((-1.,0.,0.)))
    test('llRot2Fwd(Quaternion((0.,0.,0.,1.)))', Vector((1.,0.,0.)))
    test('llRot2Fwd(Quaternion((0.,0.,0.,0.)))', Vector((1.,0.,0.)))
    test('llRot2Left(Quaternion((1.,0.,0.,0.)))', Vector((0.,-1.,0.)))
    test('llRot2Left(Quaternion((0.,1.,0.,0.)))', Vector((0.,1.,0.)))
    test('llRot2Left(Quaternion((0.,0.,1.,0.)))', Vector((0.,-1.,0.)))
    test('llRot2Left(Quaternion((0.,0.,0.,1.)))', Vector((0.,1.,0.)))
    test('llRot2Left(Quaternion((0.,0.,0.,0.)))', Vector((0.,1.,0.)))
    test('llRot2Up(Quaternion((1.,0.,0.,0.)))', Vector((0.,0.,-1.)))
    test('llRot2Up(Quaternion((0.,1.,0.,0.)))', Vector((0.,0.,-1.)))
    test('llRot2Up(Quaternion((0.,0.,1.,0.)))', Vector((0.,0.,1.)))
    test('llRot2Up(Quaternion((0.,0.,0.,1.)))', Vector((0.,0.,1.)))
    test('llRot2Up(Quaternion((0.,0.,0.,0.)))', Vector((0.,0.,1.)))

    lslcommon.IsCalc = True # llGenerateKey() only works in calculator mode
    test('cond(llGenerateKey())', True)
    lslcommon.IsCalc = False
    shouldexcept('llGenerateKey()', ELSLCantCompute)

    testXB64S("", "", "")
    testXB64S(u"Hello, World!", u"", u"Hello, World!")
    testXB64S("AAAAA==AAAAA=", "_X", "/X/X/==X/X/X=")
    testXB64S("AAAAAA......AAAAAAA", "BCDEFG=====", "BCDEFG======CDEFGBC")
    testXB64S("AAAAA===AAAAAAAAAAA", "BCDEF", "BCDEF===EFBCDEFBCDE")
    testXB64S("Hello, World!", "A", "Hello==World=")
    testXB64S("Hello, World!", "A=", "Hello==World=")
    testXB64S("Hello, World!", "A?", "Hello==World=")
    testXB64S("Hello, World!", "A?A", "Hello==World=")
    testXB64S("Hello, World!", "+", "5gbbW==oWVbj=")
    testXB64S("Hello, World!", "++", "5gbbW==oWVbj=")
    testXB64S("Hello, World!", "=", "5gbbW==oWVbj=")
    testXB64S("Hello, World!", "+=", "5gbbW==oWVbj=")
    testXB64S("Hello, World!", "+?", "5gbbW==oWVbj=")
    testXB64S("Hello, World!", "+??", "5gbbW==oWVbj=")
    testXB64S("Hello, World!", "+???", "5gbbW==oWVbj=")
    testXB64S("Hello, World!", "+????", "5gbbW==oWVbj=")
    testXB64S("Hello, World!", "+?????", "5gbbW==oWVbj=")
    testXB64S("Hello, World!", "+???????", "5gbbW==oWVbj=")
    testXB64S("Hello, World!", "/", "4haaX==pXUai=")
    testXB64S("Hello, World!", "//", "4haaX==pXUai=")
    testXB64S("Hello, World!", "^", "4haaX==pXUai=")
    testXB64S("Hello, World!", ".", "4haaX==pXUai=")
    testXB64S("Hello, World!", "_", "4haaX==pXUai=")
    testXB64S("Hello, World!", "_XX", "4Jya/==B/UyK=")
    testXB64S("Hello, World!", "XYZ", "QG8yw==Ox89E=")
    testXB64S("Hello, World!", "XYZ?", "QG8yw==BwyyF=")
    testXB64S("Hello, World!", "XYZXYZ", "QG8yw==Ox89E=")
    testXB64S("Hello, World!", "XYZXYZ==", "QG8yw==BwyyF=")
    testXB64S("AAAAA===AAAAAAAAAAA", "BCDEF", "BCDEF===EFBCDEFBCDE")
    testXB64S("AAAAA===AAAAAAAAAAA", "BCDEF=", "BCDEF===DEFBCDEFBCD")
    testXB64S("AAAAA===AAAAAAAAAAA", "BCDEF==", "BCDEF===CDEFBCDEFBC")
    testXB64S("AAAAA===AAAAAAAAAAA", "BCDEF===", "BCDEF===BCDEFBCDEFB")
    testXB64S("AA_AAA______AAAAAAAAAAAAA", "BC=EFG==",
                 "BC=EFG======FGBCBCBCBCBCB")
    testXB64S("AA_AAA______AAAAAA=AAAAAA", "BC=EFG==",
                 "BC=EFG======FGBCBC=EFGBCB")
    testXB64S("AAAAA==AAAAA", "_XXXXXXX", "/XXXX==X/XXX")
    testXB64S("ABCDABCDABCDABCDABCDABCDABCD", "ABCD",
                 "AAAAAAAAAAAAAAAAAAAAAAAAAAAA")
    testXB64S("BCDABCDABCDABCDABCDABCDABCDA", "BCDA",
                 "AAAAAAAAAAAAAAAAAAAAAAAAAAAA")
    testXB64S("AA_AAA______AAAAAAAAAAAAA", "=5gbbW==oWVbj=", "+5=bbW======j+5gbbW+5gbbW")

    testXB64SC(u"\U00012345", "", u"\U00012345")
    testXB64SC("AABA", "1234", "1224")
    testXB64SC("1234", "AABA", "1234")
    testXB64SC("BAAA", "1234", "0234")
    testXB64SC("1234", "BAAA", "02n8")
    testXB64SC("AABA", "AABA", "AABA")
    testXB64SC("AABA", "AABC", "AABA")
    testXB64SC("AABC", "AABA", "AABC")
    testXB64SC("Hello, World!", "XYZXYZ", "QG8y")
    testXB64SC("QG8y", "XYZXYZ", "Hell")
    testXB64SC("ABCDABCDABCDABCDABCDABCDABCD", "ABCD",
                  "AAAAAAAAAAAAAAAAAAAAAAAAAAAA")
    testXB64SC("BCDABCDABCDABCDABCDABCDABCDA", "BCDA",
                  "AAAAAAAAAAAAAAAAAAAAAAAAAAAA")
    testXB64SC("ABCD", "ABCD", "AAAA")
    testXB64SC("ABCDABCDABCD", "ABCD", "AAAAAAAAAAAA")
    testXB64SC("AACD", "AACD", "AACD")
    testXB64SC("AQCD", "AQCD", "AAGC")
    testXB64SC("AQCDAQCD", "AQC=", "AAGCAAGC")
    testXB64SC("AQCDAQCD", "AQCD", "AAGCAAGC")
    testXB64SC("ACCD", "AC==", "ACCD")
    testXB64SC("ABCD", "AB==", "ABCD")
    testXB64SC("ABCD", "ABC=", "AACD")
    testXB64SC("APCD", "APC=", "AACD")
    testXB64SC("AQCD", "AQC=", "AAGC")
    testXB64SC("ACCD", "ABC=", "ADCD")
    testXB64SC("ARCD", "ARC=", "AACC")
    testXB64SC("ABCDABCDABCDABCDABCDABCDABCD", "AB==",
                  "ABCDABCDABCDABCDABCDABCDABCD")
    testXB64SC("ABCDABCDABCDABCDABCDABCDABCD", "AQ==",
                  "ARGCARGCARGCARGCARGCARGCARGC")
    testXB64SC("ABCDABCDABCDABCDABCDABCDABCD", "ABCDAP//",
                  "AAAAAAAAAAAAAAAAAAAAAAAAAAAA")

    testXB64(u"\U00012345", "", u"\U00012345")
    testXB64("AABA", "1234", "1224")
    testXB64("1234", "AABA", "1224")
    testXB64("BAAA", "1234", "0234")
    testXB64("1234", "BAAA", "0234")
    testXB64("AABA", "AABA", "AAAA")
    testXB64("AABA", "AABC", "AAAC")
    testXB64("AABC", "AABA", "AAAC")
    testXB64("Hello, World!", "XYZXYZ", "QG8y")
    testXB64("QG8y", "XYZXYZ", "Hell")
    testXB64("ABCDABCDABCD", "ABCD", "AAAAAAAAABCT", True)
    testXB64("ABCDABCDABCDABCDABCDABCDABCD", "ABCD",
                "AAAAAAAAABCTgxCDEJODAAAAABCT", True)
    testXB64("ABCDABCDABCD", "ABCD", "AAAAAAAAAAAA")
    testXB64("ABCDABCDABCDABCDABCDABCDABCD", "ABCD",
                "AAAAAAAAAAAAAAAAAAAAAAAAAAAA")
    testXB64S ("ABCDABCDABCD", "ABC=", "AAADBDCCCBDB")
    testXB64SC("ABCDABCDABCD", "ABC=", "AACDEBCTAACD")
    testXB64  ("ABCDABCDABCD", "ABC=", "AACDEBCDEBCD", True)
    testXB64  ("ABCDABCDABCD", "ABC=", "AACDEBCTAACD")

    testXB64("Stuffs not b64 <^.^>!", "AA==", "Stuffg==")

    # Four different results for the same input strings...
    testXB64S ("AQCDAQCD", "AQC=", "AAADQSCT")
    testXB64SC("AQCDAQCD", "AQC=", "AAGCAAGC")
    testXB64  ("AQCDAQCD", "AQC=", "AACCAQCC", True)
    testXB64  ("AQCDAQCD", "AQC=", "AACCAQGD") # the only correct one


    lslcommon.IsCalc = True # llModPow can only be computed in calculator mode
    test('llModPow(65535, 3, 0)', 0)
    test('llModPow(65535, 3, 41)', 34)
    test('llModPow(65535, 3, -2147483648)', 196607)
    test('llModPow(65535, 3, -2147483647)', 131071)
    test('llModPow(65533, 3, -2147483648)', 1769445)
    test('llModPow(65533, 3, -2147483645)', 1572843)
    test('llModPow(65533, 3, 2147483645)', 1966047)
    test('llModPow(65533, 3, 555)', 142)
    test('llModPow(65533, 3, 1073741823)', 1966045)
    test('llModPow(65533, 3, 1073741824)', 1769445)
    test('llModPow(65533, 3, 1073741825)', 1572845)
    test('llModPow(32767, 3, 1073741825)', 98302)
    test('llModPow(32767, 3, 107374182)', 216275)
    test('llModPow(32767, 3, 10737418)', 876887)
    test('llModPow(32767, 3, 1073741)', 230066)
    test('llModPow(32767, 3, 107374)', 54345)
    test('llModPow(32767, 3, 507374)', 161343)
    test('llModPow(32767, 3, 907374)', 346875)
    test('llModPow(32767, 3, 707374)', 690307)
    test('llModPow(32767, 3, 607374)', 139309)
    test('llModPow(32767, 3, 600374)', 146813)
    test('llModPow(32767, 3, 550374)', 389875)
    test('llModPow(32767, 3, 520374)', 301047)
    test('llModPow(32767, 3, 510374)', 36839)
    test('llModPow(32767, 3, 500374)', 115989)
    test('llModPow(32767, 3, 300374)', 83681)
    test('llModPow(32767, 3, 100374)', 23425)
    test('llModPow(32767, 3, 130374)', 64819)
    test('llModPow(32767, 3, 132374)', 66641)
    test('llModPow(32767, 3, 142374)', 93049)
    test('llModPow(32767, 3, 172374)', 59569)
    test('llModPow(32767, 3, 192374)', 66591)
    test('llModPow(32767, 3, 199374)', 112231)
    test('llModPow(32767, 3, 209374)', 54343)
    test('llModPow(32767, 3, 259374)', 84733)
    test('llModPow(32767, 3, 269374)', 49913)
    test('llModPow(32767, 3, 261374)', 85865)
    test('llModPow(32767, 3, 260374)', 2379)
    test('llModPow(32767, 3, 250374)', 78307)
    test('llModPow(32767, 3, 259375)', 99163)
    test('llModPow(32767, 3, 260000)', 254367)
    test('llModPow(32767, 3, 259999)', 90487)
    test('llModPow(32767, 3, 259500)', 19663)
    test('llModPow(32767, 3, 259750)', 29663)
    test('llModPow(32767, 3, 259850)', 49367)
    test('llModPow(32767, 3, 259800)', 164967)
    test('llModPow(32767, 3, 259790)', 137017)
    test('llModPow(32767, 3, 259770)', 64183)
    test('llModPow(32767, 3, 259780)', 237863)
    test('llModPow(32767, 3, 259785)', 162132)
    test('llModPow(32767, 3, 259782)', 85797)
    test('llModPow(32767, 3, 259781)', 157054)
    test('llModPow(32767, 2, 259781)', 1416)
    test('llModPow(32767, 2, 259782)', 257065)
    test('llModPow(32767, 3, 259782)', 85797)
    test('llModPow(-1, 3, 259782)', 251271)
    test('llModPow(-1, -3, 259782)', 251271)
    test('llModPow(0, 0, 0)', 0)
    test('llModPow(1, 0, 0)', 0)
    test('llModPow(1, 0, 1)', 0)
    test('llModPow(1, 0, 2)', 1)
    test('llModPow(1, 0, 3)', 1)
    test('llModPow(1, 0, 4)', 1)
    test('llModPow(1, 1, 1)', 0)
    test('llModPow(5, 1, 1)', 0)
    test('llModPow(5, 25, 7)', 5)
    test('llModPow(5, 25, 13)', 5)
    test('llModPow(5, 25, 17)', 12)
    test('llModPow(41, 1, 17)', 7)
    lslcommon.IsCalc = False

    lslcommon.LSO = True
    test('llListFindList([],[])', -1)
    test('llListFindList([3],[])', 0)
    lslcommon.LSO = False
    test('llListFindList([],[])', 0)
    test('llListFindList([3],[])', 0)
    test('llListFindList([3],[3,4,5])', -1)
    test('llListFindList([NaN], [NaN])', 0) # I swear.
    test('llListFindList([NaN, Indet], [Indet, NaN])', 0) # Indeed.
    test('llListFindList([-0.], [0.])', 0) # Really.
    test('llListFindList([0.], [-0.])', 0) # Yes.
    test('llListFindList([1, NaN, 1., NaN], [1., NaN])', 2)
    test('llListFindList([1, NaN, 1., NaN], [2.])', -1)
    test('llListFindList([Vector((NaN,0.,0.))], [Vector((NaN,0.,0.))])', -1) # Yes, really
    test('llListFindList([Vector((0.,0.,0.))], [Vector((0.,0.,0.))])', 0)
    test('llListFindList([Quaternion((0.,0.,0.,NaN))], [Quaternion((0.,0.,0.,NaN))])', -1) # Python == fails here
    test('llListFindList([Quaternion((0.,0.,0.,Indet))], [Quaternion((0.,0.,0.,Indet))])', -1)
    test('''llListFindList([Key(u"12345678-ABCD-5678-1234-123456781234")],
            [Key(u"12345678-abcd-5678-1234-123456781234")])''', -1)
    test('''llListFindList([Key(u"12345678-abcd-5678-1234-123456781234")],
                           [Key(u"12345678-abcd-5678-1234-123456781234")])''', 0)
    test('''llListFindList([u"12345678-abcd-5678-1234-123456781234",
                            Key(u"12345678-abcd-5678-1234-123456781234")],
            [Key(u"12345678-abcd-5678-1234-123456781234")])''', 1)

    test('llIntegerToBase64(-680658713)', u'12345w==')
    test('llIntegerToBase64(-1)', u'/////w==')
    test('llIntegerToBase64(0)', u'AAAAAA==')
    test('llIntegerToBase64(1)', u'AAAAAQ==')
    test('llIntegerToBase64(2)', u'AAAAAg==')
    test('llIntegerToBase64(3)', u'AAAAAw==')
    test('llIntegerToBase64(-2147483648)', u'gAAAAA==')
    test('llIntegerToBase64(2147483647)', u'f////w==')

    test('llBase64ToInteger(u"123456789")', 0)
    test('llBase64ToInteger(u"12345A===")', 0)
    test('llBase64ToInteger(u"12345678")', -680658713)
    test('llBase64ToInteger(u"123456")', -680658713)
    test('llBase64ToInteger(u"123456==")', -680658713)
    test('llBase64ToInteger(u"12345w==")', -680658713)
    test('llBase64ToInteger(u"12345.")', -680658944)
    test('llBase64ToInteger(u"gAAAAA")', -2147483648)
    test('llBase64ToInteger(u"/////w")', -1)

    s = u''.join(unichr(i) for i in xrange(1,257))
    test('llEscapeURL(' + repr(s) + ')',
        u'%01%02%03%04%05%06%07%08%09%0A%0B%0C%0D%0E%0F%10%11%12%13%14%15%16%17%18%19%1A%1B%1C%1D%1E%1F'
        u'%20%21%22%23%24%25%26%27%28%29%2A%2B%2C%2D%2E%2F0123456789%3A%3B%3C%3D%3E%3F%40ABCDEFGHIJKLMN'
        u'OPQRSTUVWXYZ%5B%5C%5D%5E%5F%60abcdefghijklmnopqrstuvwxyz%7B%7C%7D%7E%7F%C2%80%C2%81%C2%82%C2%'
        u'83%C2%84%C2%85%C2%86%C2%87%C2%88%C2%89%C2%8A%C2%8B%C2%8C%C2%8D%C2%8E%C2%8F%C2%90%C2%91%C2%92%'
        u'C2%93%C2%94%C2%95%C2%96%C2%97%C2%98%C2%99%C2%9A%C2%9B%C2%9C%C2%9D%C2%9E%C2%9F%C2%A0%C2%A1%C2%'
        u'A2%C2%A3%C2%A4%C2%A5%C2%A6%C2%A7%C2%A8%C2%A9%C2%AA%C2%AB%C2%AC%C2%AD%C2%AE%C2%AF%C2%B0%C2%B1%'
        u'C2%B2%C2%B3%C2%B4%C2%B5%C2%B6%C2%B7%C2%B8%C2%B9%C2%BA%C2%BB%C2%BC%C2%BD%C2%BE%C2%BF%C3%80%C3%'
        u'81%C3%82%C3%83%C3%84%C3%85%C3%86%C3%87%C3%88%C3%89%C3%8A%C3%8B%C3%8C%C3%8D%C3%8E%C3%8F%C3%90%'
        u'C3%91%C3%92%C3%93%C3%94%C3%95%C3%96%C3%97%C3%98%C3%99%C3%9A%C3%9B%C3%9C%C3%9D%C3%9E%C3%9F%C3%'
        u'A0%C3%A1%C3%A2%C3%A3%C3%A4%C3%A5%C3%A6%C3%A7%C3%A8%C3%A9%C3%AA%C3%AB%C3%AC%C3%AD%C3%AE%C3%AF%'
        u'C3%B0%C3%B1%C3%B2%C3%B3%C3%B4%C3%B5%C3%B6%C3%B7%C3%B8%C3%B9%C3%BA%C3%BB%C3%BC%C3%BD%C3%BE%C3%'
        u'BF%C4%80')

    test(r'llStringToBase64(u"\U0001D11E\xC1a\xF1# +")', u'8J2EnsOBYcOxIyAr')
    test('llBase64ToString(u"8J2EnsOBYcOxIyAr")', u'\U0001D11E\xC1a\xF1# +')
    test('llBase64ToString(u"")', u'')
    test('llBase64ToString(u"1")', u'')
    test('llBase64ToString(u"12")', u'?')
    test('llBase64ToString(u"14A")', u'\u05C0')
    # llUnescapeURL and llBase64ToString behave differently. We need to test
    # both thoroughly.

    # Embedded and trailing NUL tests:
    test('llBase64ToString(u"QUJDAERFRg")', u'ABC?DEF') # 'ABC\x00DEF'
    test('llBase64ToString(u"AEEAQgBD")', u'?A?B?C') # '\x00A\x00B\x00C'
    test('llBase64ToString(u"AEEAQgBDAA")', u'?A?B?C') # '\x00A\x00B\x00C\x00'
    test('llBase64ToString(u"AEEAQgBDAAA=")', u'?A?B?C?') # '\x00A\x00B\x00C\x00'

    # Some assorted tests:
    test('llBase64ToString(u"gIAA")', u'??')
    test('llBase64ToString(u"gAA")', u'?')
    test('llBase64ToString(u"44AA")', u'?')
    test('llBase64ToString(u"4IAh")', u'?!')
    test('llBase64ToString(u"gICAgGE")', u'????a')
    test('llBase64ToString(u"QQA")', u'A')
    test('llBase64ToString(u"AEE=")', u'?A')
    test('llBase64ToString(u"wKE")', u'?') # C080
    test('llBase64ToString(u"9ICA")', u'?') # F48080
    test('llBase64ToString(u"94CAgICA")', u'??????')
    test('llBase64ToString(u"4ICA")', u'?') # E08080
    test('llBase64ToString(u"4IA")', u'?') # E080
    test('llUnescapeURL(u"%E0%80")', u'??') # Compare with the above

    testB642S("C38180E381C380414243D3", "%C3%81%3F%3F%C3%80ABC%3F")
    testB642S("C38180E381C38041424300D3", "%C3%81%3F%3F%C3%80ABC%3F%3F")
    testB642S("E0808080808080E381C38041424300D3", "%3F%3F%3F%3F%3F%3F%C3%80ABC%3F%3F")
    # test UTF-8 valid ranges
    testB642S("7F78", "%7Fx") # range up to U+007F
    testB642S("808078", "%3F%3Fx") # invalid range begin
    testB642S("BFBF78", "%3F%3Fx") # invalid range end
    testB642S("C08078", "%3Fx") # aliased range begin (U+0000)
    testB642S("C1BF78", "%3Fx") # aliased range end   (U+007F)
    testB642S("C28078", "%C2%80x") # U+0080 (2-byte range start)
    testB642S("DFBF78", "%DF%BFx") # U+07FF (2-byte range end)
    testB642S("E0808078", "%3Fx") # aliased range begin (U+0000)
    testB642S("E09FBF78", "%3Fx") # aliased range end   (U+07FF)
    testB642S("E0A08078", "%E0%A0%80x") # U+0800 (3-byte range start)
    testB642S("ED9FBF78", "%ED%9F%BFx") # U+D7FF (right before first UTF-16 high surrogate)
    testB642S("EE808078", "%EE%80%80x") # U+E000 (right after last UTF-16 low surrogate)
    testB642S("EFBFBF78", "%EF%BF%BFx") # U+FFFF (3-byte range end)
    testB642S("F080808078", "%3Fx") # aliased range begin (U+0000)
    testB642S("F08FBFBF78", "%3Fx") # aliased range end   (U+FFFF)
    testB642S("F090808078", "%F0%90%80%80x") # U+10000 (4-byte range start)
    testB642S("F48FBFBF78", "%F4%8F%BF%BFx") # U+10FFFF (valid 4-byte range end)
    # excluded because they are used for UTF-16 surrogates, not valid characters
    testB642S("EDA08078", "%3F%3F%3Fx") # D800 - first high surrogate
    testB642S("EDAFBF78", "%3F%3F%3Fx") # DBFF - last high surrogate
    testB642S("EDB08078", "%3F%3F%3Fx") # DC00 - first low surrogate
    testB642S("EDBFBF78", "%3F%3F%3Fx") # DFFF - last low  surrogate
    # excluded because of truncation to U+10FFFF
    testB642S("F490808078", "%3F%3F%3F%3Fx") # U+110000 (invalid 4-byte range start)
    testB642S("F7BFBFBF78", "%3F%3F%3F%3Fx") # U+1FFFFF (invalid 4-byte range end)
    testB642S("F88080808078", "%3Fx") # aliased range begin (U+0000)
    testB642S("F887BFBFBF78", "%3Fx") # aliased range end (U+1FFFFF)
    testB642S("F88880808078", "%3F%3F%3F%3F%3Fx") # U+200000 (invalid 5-byte range start)
    testB642S("FBBFBFBFBF78", "%3F%3F%3F%3F%3Fx") # U+3FFFFFF (invalid 5-byte range end)
    testB642S("FC808080808078", "%3Fx") # aliased range begin (U+0000)
    testB642S("FC83BFBFBFBF78", "%3Fx") # aliased range end (U+3FFFFFF)
    testB642S("FC848080808078", "%3F%3F%3F%3F%3F%3Fx") # U+4000000 (invalid 6-byte range start)
    testB642S("FDBFBFBFBFBF78", "%3F%3F%3F%3F%3F%3Fx") # U+7FFFFFFF (invalid 6-byte range end)
    # not actually valid either way (these are actually used to distinguish the
    # input as UTF-16 BOM and are invalid in UTF-8)
    testB642S("FEB080808080808078", "%3F%3F%3F%3F%3F%3F%3F%3Fx")
    testB642S("FFBFBFBFBFBFBFBF78", "%3F%3F%3F%3F%3F%3F%3F%3Fx")
    # short or invalid sequences
    testB642S("80", "%3F")
    testB642S("BF", "%3F")
    testB642S("C2", "%3F")
    testB642S("E1", "%3F")
    testB642S("E180", "%3F")
    testB642S("F1", "%3F")
    testB642S("F180", "%3F")
    testB642S("F18080", "%3F")
    testB642S("F8808080", "%3F")
    testB642S("F8888080", "%3F")
    testB642S("FC80808080", "%3F")
    testB642S("FC84808080", "%3F")

    # Test that U+FFFD is preserved even with invalid characters
    testB642S("EFBFBD90", "%EF%BF%BD%3F")


    test('typecast([1,F32(3.14),Key(u"blah"),Quaternion((1.,0.,0.,0.))], unicode)',
        u'13.140000blah<1.000000, 0.000000, 0.000000, 0.000000>')

    test('''llDumpList2String(llCSV2List(u'a,<<1,2>,3,4,">5,6, "1,3",7<<>,8,9'), u"|")''',
        u'a|<<1,2>,3,4,">5|6|"1|3"|7<<>,8,9')

    test('llInsertString(u"", -3, u"abc")', u'abc')
    test('llInsertString(u"", -1, u"abc")', u'abc')
    test('llInsertString(u"", 0, u"abc")', u'abc')
    test('llInsertString(u"", 1, u"abc")', u'abc')
    test('llInsertString(u"", 3, u"abc")', u'abc')

    test('llInsertString(u"xy", -3, u"abc")', u'abcxy')
    test('llInsertString(u"xy", -1, u"abc")', u'abcxy')
    test('llInsertString(u"xy", 0, u"abc")', u'abcxy')
    test('llInsertString(u"xy", 1, u"abc")', u'xabcy')
    test('llInsertString(u"xy", 2, u"abc")', u'xyabc')
    test('llInsertString(u"xy", 3, u"abc")', u'xyabc')

    test('llUnescapeURL(u"%")', u'')
    test('llUnescapeURL(u"%%")', u'')
    test('llUnescapeURL(u"%4%252Fabc")', u'\x40252Fabc')
    test('llUnescapeURL(u"%%4%252Fabc")', u'\x04\x252Fabc')
    test('llEscapeURL(llUnescapeURL(u"%.44%25%%2Fa\u2190c"))', u'%044%25%02Fa%E2%86%90c')
    test('llEscapeURL(llUnescapeURL(u"%.44%25%%2Fa\u2190c%"))', u'%044%25%02Fa%E2%86%90c')
    test('llEscapeURL(llUnescapeURL(u"%.44%25%%2Fa\u2190c%2"))', u'%044%25%02Fa%E2%86%90c')
    test('llEscapeURL(llUnescapeURL(u"%.44%25%%2Fa\u2190c%%"))', u'%044%25%02Fa%E2%86%90c')
    test('llEscapeURL(llUnescapeURL(u"%.44%25%%2Fa\u2190c%%2"))', u'%044%25%02Fa%E2%86%90c%02')
    test('llEscapeURL(llUnescapeURL(u"%.44%25%%2Fa\u2190c%%%2346"))', u'%044%25%02Fa%E2%86%90c')
    test('llEscapeURL(llUnescapeURL(u"%4.%25"))', u'%40%25')

    # test UTF-8 validity
    test('llEscapeURL(llUnescapeURL(u"%C3%81%80%E3%81%C3%80ABC%D3"))', u'%C3%81%3F%3F%3F%C3%80ABC%3F')
    test('llEscapeURL(llUnescapeURL(u"%C3%81%80%E3%81%C3%80ABC%00%D3"))', u'%C3%81%3F%3F%3F%C3%80ABC')
    test('llEscapeURL(llUnescapeURL(u"%E0%80%80%80%80%80%80%E3%81%C3%80ABC%00%D3"))', u'%3F%3F%3F%3F%3F%3F%3F%3F%3F%C3%80ABC')
    # test UTF-8 valid ranges
    test('llEscapeURL(llUnescapeURL(u"%7Fx"))', u'%7Fx') # range up to U+007F
    test('llEscapeURL(llUnescapeURL(u"%80%80x"))', u'%3F%3Fx') # invalid range begin
    test('llEscapeURL(llUnescapeURL(u"%BF%BFx"))', u'%3F%3Fx') # invalid range end
    test('llEscapeURL(llUnescapeURL(u"%C0%80x"))', u'%3F%3Fx') # aliased range begin (U+0000)
    test('llEscapeURL(llUnescapeURL(u"%C1%BFx"))', u'%3F%3Fx') # aliased range end   (U+007F)
    test('llEscapeURL(llUnescapeURL(u"%C2%80x"))', u'%C2%80x') # U+0080 (2-byte range start)
    test('llEscapeURL(llUnescapeURL(u"%DF%BFx"))', u'%DF%BFx') # U+07FF (2-byte range end)
    test('llEscapeURL(llUnescapeURL(u"%E0%80%80x"))', u'%3F%3F%3Fx') # aliased range begin (U+0000)
    test('llEscapeURL(llUnescapeURL(u"%E0%9F%BFx"))', u'%3F%3F%3Fx') # aliased range end   (U+07FF)
    test('llEscapeURL(llUnescapeURL(u"%E0%A0%80x"))', u'%E0%A0%80x') # U+0800 (3-byte range start)
    test('llEscapeURL(llUnescapeURL(u"%ED%9F%BFx"))', u'%ED%9F%BFx') # U+D7FF (right before first UTF-16 high surrogate)
    # excluded because they are used for UTF-16 surrogates, not valid characters
    test('llEscapeURL(llUnescapeURL(u"%ED%A0%80x"))', u'%3F%3F%3Fx') # D800 - first high surrogate
    test('llEscapeURL(llUnescapeURL(u"%ED%AF%BFx"))', u'%3F%3F%3Fx') # DBFF - last high surrogate
    test('llEscapeURL(llUnescapeURL(u"%ED%B0%80x"))', u'%3F%3F%3Fx') # DC00 - first low surrogate
    test('llEscapeURL(llUnescapeURL(u"%ED%BF%BFx"))', u'%3F%3F%3Fx') # DFFF - last low  surrogate
    test('llEscapeURL(llUnescapeURL(u"%EE%80%80x"))', u'%EE%80%80x') # U+E000 (right after last UTF-16 low surrogate)
    test('llEscapeURL(llUnescapeURL(u"%EF%BF%BFx"))', u'%EF%BF%BFx') # U+FFFF (3-byte range end)
    test('llEscapeURL(llUnescapeURL(u"%F0%80%80%80x"))', u'%3F%3F%3F%3Fx') # aliased range begin (U+0000)
    test('llEscapeURL(llUnescapeURL(u"%F0%8F%BF%BFx"))', u'%3F%3F%3F%3Fx') # aliased range end   (U+FFFF)
    test('llEscapeURL(llUnescapeURL(u"%F0%90%80%80x"))', u'%F0%90%80%80x') # U+10000 (4-byte range start)
    test('llEscapeURL(llUnescapeURL(u"%F4%8F%BF%BFx"))', u'%F4%8F%BF%BFx') # U+10FFFF (valid 4-byte range end)
    # excluded because of truncation to U+10FFFF
    test('llEscapeURL(llUnescapeURL(u"%F4%90%80%80x"))', u'%3F%3F%3F%3Fx') # U+110000 (invalid 4-byte range start)
    test('llEscapeURL(llUnescapeURL(u"%F7%BF%BF%BFx"))', u'%3F%3F%3F%3Fx') # U+1FFFFF (invalid 4-byte range end)
    test('llEscapeURL(llUnescapeURL(u"%F8%80%80%80%80x"))', u'%3F%3F%3F%3F%3Fx') # aliased range begin (U+0000)
    test('llEscapeURL(llUnescapeURL(u"%F8%87%BF%BF%BFx"))', u'%3F%3F%3F%3F%3Fx') # aliased range end (U+1FFFFF)
    test('llEscapeURL(llUnescapeURL(u"%F8%88%80%80%80x"))', u'%3F%3F%3F%3F%3Fx') # U+200000 (5-byte range start)
    test('llEscapeURL(llUnescapeURL(u"%FB%BF%BF%BF%BFx"))', u'%3F%3F%3F%3F%3Fx') # U+3FFFFFF (5-byte range end)
    test('llEscapeURL(llUnescapeURL(u"%FC%80%80%80%80%80x"))', u'%3F%3F%3F%3F%3F%3Fx') # aliased range begin (U+0000)
    test('llEscapeURL(llUnescapeURL(u"%FC%83%BF%BF%BF%BFx"))', u'%3F%3F%3F%3F%3F%3Fx') # aliased range end (U+3FFFFFF)
    test('llEscapeURL(llUnescapeURL(u"%FC%84%80%80%80%80x"))', u'%3F%3F%3F%3F%3F%3Fx') # U+4000000 (6-byte range start)
    test('llEscapeURL(llUnescapeURL(u"%FD%BF%BF%BF%BF%BFx"))', u'%3F%3F%3F%3F%3F%3Fx') # U+7FFFFFFF (6-byte range end)
    # not actually valid either way (these are actually used to distinguish the input as UTF-16 BOM)
    test('llEscapeURL(llUnescapeURL(u"%FE%B0%80%80%80%80%80%80x"))', u'%3F%3F%3F%3F%3F%3F%3F%3Fx')
    test('llEscapeURL(llUnescapeURL(u"%FF%BF%BF%BF%BF%BF%BF%BFx"))', u'%3F%3F%3F%3F%3F%3F%3F%3Fx')
    # short or invalid sequences
    test('llEscapeURL(llUnescapeURL(u"%80"))', u'%3F')
    test('llEscapeURL(llUnescapeURL(u"%BF"))', u'%3F')
    test('llEscapeURL(llUnescapeURL(u"%C2"))', u'%3F')
    test('llEscapeURL(llUnescapeURL(u"%E1"))', u'%3F')
    test('llEscapeURL(llUnescapeURL(u"%E1%80"))', u'%3F%3F')
    test('llEscapeURL(llUnescapeURL(u"%F1"))', u'%3F')
    test('llEscapeURL(llUnescapeURL(u"%F1%80"))', u'%3F%3F')
    test('llEscapeURL(llUnescapeURL(u"%F1%80%80"))', u'%3F%3F%3F')

    # Test that U+FFFD is preserved even with invalid characters
    test('llEscapeURL(llUnescapeURL(u"%EF%BF%BD%90"))', u'%EF%BF%BD%3F')

    test('llParseString2List(u"[ 1 ]2|3|4|5", [u"|"], [u"[ ", u" ]"])',
                 [u"[ ", u"1", u" ]", u"2", u"3", u"4", u"5"])
    test('llParseString2List(u"[ 1 ]2|3|4|5", [u"|"], [u"|", u"|"])',
                 [u"[ 1 ]2", u"3", u"4", u"5"])
    test('llList2CSV(llParseString2List(u"1abc2ab3abc4",[u"ab",u"abc"],[]))',
                u'1, c2, 3, c4')
    test('llList2CSV(llParseString2List(u"1abc2ab3abc4",[u"abc",u"ab"],[]))',
                u'1, 2, 3, 4')
    test('llList2CSV(llParseStringKeepNulls(u"1abc2ab3abc4",[u"ab"],[u"abc"]))',
                u'1, c2, 3, c4')
    test('llList2CSV(llParseStringKeepNulls(u"1abc2ab3abc4",[u"ab"],[u"a"]))',
                u'1, c2, 3, c4')
    test('llList2CSV(llParseStringKeepNulls(u"1abc2ab3abc4",[u"ab"],[u"ab"]))',
                u'1, c2, 3, c4')
    test('llParseStringKeepNulls(u"",[],[])', [u""])
    shouldexcept('llParseStringKeepNulls(u"",[],[""])', ELSLInvalidType)
    test('llParseStringKeepNulls(u"",[],[u""])', [u""])
    test('llParseStringKeepNulls(u"",[u""],[])', [u""])
    test('llParseStringKeepNulls(u"",[u""],[u""])', [u""])
    test('llParseString2List(u"",[],[])', [])
    test('llParseString2List(u"",[],[u""])', [])
    test('llParseString2List(u"",[u""],[])', [])
    test('llParseString2List(u"",[u""],[u""])', [])
    test('llParseStringKeepNulls(u"a",[u""],[])', [u"a"])


    test(r'llToUpper(u"\u03c4\u03ac\u03c7\u03b9\u03c3\u03c4\u03b7 \u03b1\u03bb'
         r'\u03ce\u03c0\u03b7\u03be \u03b2\u03b1\u03c6\u03ae\u03c2 \u03c8\u03b7'
         r'\u03bc\u03ad\u03bd\u03b7 \u03b3\u03b7, \u03b4\u03c1\u03b1\u03c3'
         r'\u03ba\u03b5\u03bb\u03af\u03b6\u03b5\u03b9 \u03c5\u03c0\u03ad\u03c1 '
         r'\u03bd\u03c9\u03b8\u03c1\u03bf\u03cd \u03ba\u03c5\u03bd\u03cc\u03c2")',
         u'\u03a4\u0386\u03a7\u0399\u03a3\u03a4\u0397 \u0391\u039b\u038f\u03a0'
         u'\u0397\u039e \u0392\u0391\u03a6\u0389\u03a3 \u03a8\u0397\u039c\u0388'
         u'\u039d\u0397 \u0393\u0397, \u0394\u03a1\u0391\u03a3\u039a\u0395'
         u'\u039b\u038a\u0396\u0395\u0399 \u03a5\u03a0\u0388\u03a1 \u039d\u03a9'
         u'\u0398\u03a1\u039f\u038e \u039a\u03a5\u039d\u038c\u03a3')

    test(r'llToLower(u"\u03a4\u0386\u03a7\u0399\u03a3\u03a4\u0397 \u0391\u039b'
         r'\u038f\u03a0\u0397\u039e \u0392\u0391\u03a6\u0389\u03a3 \u03a8\u0397'
         r'\u039c\u0388\u039d\u0397 \u0393\u0397, \u0394\u03a1\u0391\u03a3'
         r'\u039a\u0395\u039b\u038a\u0396\u0395\u0399 \u03a5\u03a0\u0388\u03a1 '
         r'\u039d\u03a9\u0398\u03a1\u039f\u038e \u039a\u03a5\u039d\u038c\u03a3")',
         u'\u03c4\u03ac\u03c7\u03b9\u03c3\u03c4\u03b7 \u03b1\u03bb\u03ce\u03c0'
         u'\u03b7\u03be \u03b2\u03b1\u03c6\u03ae\u03c3 \u03c8\u03b7\u03bc\u03ad'
         u'\u03bd\u03b7 \u03b3\u03b7, \u03b4\u03c1\u03b1\u03c3\u03ba\u03b5'
         u'\u03bb\u03af\u03b6\u03b5\u03b9 \u03c5\u03c0\u03ad\u03c1 \u03bd\u03c9'
         u'\u03b8\u03c1\u03bf\u03cd \u03ba\u03c5\u03bd\u03cc\u03c3')

    test('llListStatistics(1, [])', 0.)
    test('llListStatistics(0, [1.,5.,2,3,9,-1])', 10.)
    test('llListStatistics(1, [1.,5.,-2,3,9,-1])', -2.)
    test('llListStatistics(2, [1.,5.,2,3,9,-1])', 9.)
    test('llListStatistics(3, [1.,5.,2,3,9,-1])', F('0x1.955556p+1'))
    test('llListStatistics(4, [1.,5.,2,3,9,-1])', 2.5)
    test('llListStatistics(4, [1.,5.,NaN,2,3,9,-1])', -1.)
    test('llListStatistics(5, [1.,5.,2,3,9,-1])', F32(3.488075))
    test('llListStatistics(6, [1.,5.,2,3,9,-1])', 19.)
    test('llListStatistics(7, [1.,5.,2,3,9,-1])', 121.)
    test('llListStatistics(8, [1.,5.,2,3,9,-1])', 6.)
    test('llListStatistics(9, [1.,5.,2.,3.,NaN,9.])', NaN)
    test('llListStatistics(9, [1.,5.,2.,3.,NaN,-9.])', 0.)
    test('llListStatistics(9, [1.,5.,2.,3.,1.,9.])', F('0x1.456A2Ep+1'))
    test('llListStatistics(10, [1.,5.,2,3,9,-1])', 0.)
    test('llList2ListStrided([1,2,3,4,5],0,-1,2)', [1,3,5])
    test('llList2ListStrided([1,2,3,4,5],1,-1,2)', [3,5])
    test('llList2ListStrided([1,2,3,4,5],0,-2,2)', [1,3])
    test('llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],0,-4,3)', [1, 4, 7])
    test('llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],0,-3,3)', [1, 4, 7, 10])
    test('llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],0,-1,3)', [1, 4, 7, 10])
    test('llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],-3,3,3)', [1, 4, 7, 10])
    test('llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],-2,-3,3)', [1, 4, 7, 10])
    test('llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],-2,-2,3)', [])
    test('llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],-2,-1,3)', [])
    test('llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],4,3,3)', [1,4,7,10])
    test('llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],4,4,3)', [])
    test('llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],4,5,3)', [])
    test('llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],5,5,3)', [])
    test('llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],5,6,3)', [7])
    test('llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],6,5,3)', [1,4,7,10])
    test('llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],6,6,3)', [7])
    test('llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],6,2,-3)', [1,4,7,10])
    test('llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],6,2,-2)', [1,3,5,7,9,11])
    test('llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],6,2,-1)', [1,2,3,4,5,6,7,8,9,10,11,12])
    test('llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],6,2,0)', [1,2,3,4,5,6,7,8,9,10,11,12])
    test('llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],6,2,1)', [1,2,3,4,5,6,7,8,9,10,11,12])
    test('llList2ListStrided([1,2,3,4,5,6,7,8,9,10,11,12],6,20,3)', [7,10])

    test('llListReplaceList([0,1,2,3,4,5],[6,7],2,3)', [0,1,6,7,4,5])
    test('llListReplaceList([0,1,2,3,4,5],[],2,3)', [0,1,4,5])
    test('llListReplaceList([0,1,2,3,4,5],[6,7],2,3)', [0,1,6,7,4,5])
    test('llListReplaceList([0,1,2,3,4,5],[6,7],2,-1)', [0,1,6,7])
    test('llListReplaceList([0,1,2,3,4,5],[],4,1)', [2,3])
    test('llListReplaceList([0,1,2,3,4,5],[6,7,8],4,1)', [2,3,6,7,8])
    test('llListReplaceList([0,1,2,3,4,5],[6,7,8],6,6)', [0,1,2,3,4,5,6,7,8])
    test('llListReplaceList([0,1,2,3,4,5],[6,7,8],7,6)', [6,7,8])
    test('llListReplaceList([0,1,2,3,4,5],[6,7,8],7,8)', [0,1,2,3,4,5,6,7,8])

    test('llListInsertList([], [1], 0)', [1])
    test('llListInsertList([], [1], 3)', [1])
    test('llListInsertList([], [1], -1)', [1])
    test('llListInsertList([1,2,3],[4,5],-1)', [1,2,4,5,3])
    test('llListInsertList([1,2,3],[4,5],-5)', [4,5,1,2,3])
    test('llListInsertList([1,2,3,4,5],[9],-3)', [1,2,9,3,4,5])

    test('llMD5String(u"", 0)', u'1a9d5db22c73a993ff0b42f64b396873')

    test('llGetEnv(u"")', u'')
    test('llGetEnv(u"yadda")', u'')
    shouldexcept('llGetEnv(u"agent_limit")', ELSLCantCompute)




    # JSON tests - Here be dragons.
    print "9 errors expected past here " + "-" * 50
    test('''llEscapeURL(llList2Json(JSON_OBJECT, [llUnescapeURL(
            u"%01%02%03%04%05%06%07%08%09%0A%0B%0C%0D%0E%0F"
            u"%10%11%12%13%14%15%16%17%18%19%1A%1B%1C%1D%1E%1F"
            u"%20%21%22%23%24%25%26%27%28%29%2A%2B%2C%2D%2E%2F"
            u"%30%39%3A%3B%3C%3D%3E%3F%40%41%5A%5B%5C%5D%5E%5F"
            u"%60%61%7A%7B%7C%7D%7E%7F%C2%80%C2%81"),Key(u"")]))''',

            u'%7B%22%01%02%03%04%05%06%07%5Cb%5Ct%5Cn%0B%5Cf%5Cr%0E%0F'
            u'%10%11%12%13%14%15%16%17%18%19%1A%1B%1C%1D%1E%1F'
            u'%20%21%5C%22%23%24%25%26%27%28%29%2A%2B%2C%2D%2E%5C%2F'
            u'09%3A%3B%3C%3D%3E%3F%40AZ%5B%5C%5C%5D%5E%5F%60az%7B%7C%7D%7E%7F'
            u'%C2%80%C2%81%22%3A%22%22%7D')

    test(r'llEscapeURL(llList2Json(JSON_OBJECT, [u"blah",F32(Quaternion((1000000000000.0,-Infinity,NaN,-0.))), u"x", u"{\"}}"]))',
        u'%7B%22blah%22%3A%22%3C999999995904%2E000000%2C%20%2Dinf%2C%20nan%2C%20'
        u'%2D0%2E000000%3E%22%2C%22x%22%3A%7B%22%7D%7D%7D')

    test(r'llList2Json(JSON_OBJECT, [u"true",u"true"])', u'{"true":true}')
    test(r'llList2Json(JSON_OBJECT, [u"true",u"tr\nue"])', ur'{"true":"tr\nue"}')

    # ... yeah
    test(r'llList2Json(JSON_ARRAY, [u" "+JSON_INVALID+u" ",u" "+JSON_DELETE+u" ",'
         r'u" "+JSON_STRING+u" ",JSON_NUMBER,JSON_OBJECT,JSON_ARRAY])',
         u'[,"\ufdd8","\ufdd4","\ufdd3","\ufdd1","\ufdd2"]')


    Bugs.add(6466)
    test(r'llJson2List(u"  -e11111.193147483650  ")', [0.0])
    test(r'llJson2List(u"  12345678901234  ")', [2147483647])
    test(r'llJson2List(u"  12345678901234e  ")', [12345679020032.])
    test(r'llJson2List(u"  1.0e+1  ")', [u'1.0e+1'])
    test(r'llJson2List(u"  5e-1.2  ")', [0.5])
    test(r'llJson2List(u"  0e0  ")', [0.])
    test(r'llJson2List(u"  .  ")', [0.])
    test(r'llJson2List(u"  .5  ")', [0.5])
    test(r'llJson2List(u"  .5e  ")', [0.5])
    test(r'llJson2List(u"  .e1  ")', [0.])
    test(r'llJson2List(u"  0.e1  ")', [0.])
    Bugs.discard(6466)
    test(r'llJson2List(u"  -e11111.193147483650  ")', [u'-e11111.193147483650'])
    test(r'llJson2List(u"  12345678901234  ")', [2147483647])
    test(r'llJson2List(u"  12345678901234e  ")', [12345679020032.])
    test(r'llJson2List(u"  1.0e+1  ")', [10.0])
    test(r'llJson2List(u"  5e-1.2  ")', [0.5])
    test(r'llJson2List(u"  0e0  ")', [u'0e0'])
    test(r'llJson2List(u"  .  ")', [0.])
    test(r'llJson2List(u"  .5  ")', [0.5])
    test(r'llJson2List(u"  .5e  ")', [0.5])
    test(r'llJson2List(u"  .e1  ")', [u'.e1'])
    test(r'llJson2List(u"  0.e1  ")', [0.])
    test(r'llJson2List(u"  -12345678901234  ")', [-2147483648])
    test(r'llJson2List(u"  -123456  ")', [-123456])
    test(r'llJson2List(u"  true  ")', [JSON_TRUE])
    test(r'llJson2List(u"  \"blah\\\"  ")', [u'blah\\'])
    test(r'llJson2List(u"  True  ")', [u'True'])
    test(r'llJson2List(u"  \"  ")', [u'"'])

    # Here's where the fun begins. Solve this puzzle.
    test(r'llJson2List(u"  \"ab,c\"3{,\"de\\1234\"  ")', [u'ab,c"3{,"de1234'])
    # INCOMPAT: Too crazy to make it work compatibly. All these tests fail.
    #test(r'llJson2List(u" { \"ab,c\"3{,\"de\\1234\" } ")', [u'', u'"ab,c"3'])
    #test(r'llJson2List(u" { x: \"ab,c\"3{,\"de\\1234\" } ")', [JSON_INVALID])
    #test(r'llJson2List(u" { \"x\": \"ab,c\"3{,\"de\\1234\" } ")', [u'x', u'"ab,c"3'])
    #test(r'llJson2List(u" [ \"x\": \"ab,c\"3{,\"de\\1234\" ] ")', [u'x', u'"ab,c"3'])
    #test(r'llJson2List(u" [ ab,c\"3{,\"de\\1234} ] ")', [u'ab', ur'"3,de\1234'])
    #test(r'llJson2List(u" [ ab, tes{t3\"de\\1234\" ] ")', [u'ab', u'tes'])
    #test(r'llJson2List(u" [ ab, tes{t3\"de\\1234}\" ] ")', [u'ab', u'"'])
    #test(r'llJson2List(u" [ ab, tes{t3\"de\\1234}\"x\" ] ")', [u'ab', u'x'])
    #test(r'llJson2List(u" [ ab, tes{t3\"de\\1234}x ] ")', [u'ab', ur'tes{t3"de\1234}x'])
    #test(r'llJson2List(u" [ ab, test\"3{,\"de\\1234} ] ")', [u'ab', ur'"3{,"de\1234}'])
    test(r'llJson2List(u"[,,3]")', [u'', u'', 3])
    test(r'llJson2List(u"[:,2,3]")', [JSON_INVALID])
    test(r'llJson2List(u"[;,,3]")', [u';', u'', 3])
    test(r'llJson2List(u"[],,3]")', [u']', u'', 3]) # nice, huh?
    # INCOMPAT:
    #test(r'llJson2List(u"[[,,3]")', [u'']) # Even crazier. Not going to try to make that work.
    test(r'llJson2List(u"[,],3]")', [u'', u']', 3])
    test(r'llJson2List(u"{1}")', [u'', 1])
    test(r'llJson2List(u"{\"a\"}")', [u'', u'a'])
    test(r'llJson2List(u"{1,2}")', [JSON_INVALID])
    test(r'llJson2List(u"{\"\":1,\"a\":2}")', [JSON_INVALID]) # Empty names are only allowed at the end.
    test(r'llJson2List(u"{\"a\":1,\"\":2}")', [u'a',1,u'',2])
    test(r'llJson2List(u"{\"a\":1,2}")', [u'a',1,u'',2])
    test(r'llJson2List(u"{\"a\":1,}")', [u'a',1,u'',u''])
    test(r'llJson2List(u"{\"a\":},}}")', [u'a', u'}', u'', u'}'])

    test(r'llJsonGetValue(u" { ab, tes{t3\"de\\1234}x  ", [])', JSON_INVALID)
    Bugs.add(6466)
    test(r'llJsonGetValue(u"x", [])', JSON_INVALID)
    test(r'llJsonGetValue(u"e", [])', u'e')
    Bugs.discard(6466)
    test(r'llJsonGetValue(u"x", [])', JSON_INVALID)
    test(r'llJsonGetValue(u"e", [])', JSON_INVALID)
    test(r'llJsonGetValue(u"", [])', JSON_INVALID)
    test(r'llJsonGetValue(u"}", [])', JSON_INVALID)
    test(r'llJsonGetValue(u"{", [])', JSON_INVALID)
    test(r'llJsonGetValue(u" ", [])', JSON_INVALID)
    test(r'llJsonGetValue(u" , ", [])', JSON_INVALID)
    test(r'llJsonGetValue(u" null ", [])', JSON_NULL)
    test(r'llJsonGetValue(u" false ", [])', JSON_FALSE)
    test(r'llJsonGetValue(u" true ", [])', JSON_TRUE)
    test(r'llJsonGetValue(u"true,", [])', JSON_INVALID)
    test(r'llJsonGetValue(u"true,false", [])', JSON_INVALID)
    test(r'llJsonGetValue(u"{x}", [])', u'{x}')
    test(r'llJsonGetValue(u"[x]", [])', u'[x]')
    test(r'llJsonGetValue(u" \"blah\\\\\" ", [])', u'blah\\')
    test(r'llJsonGetValue(u" \"blah\\\" ", [])', u'blah\\')
    test(r'llJsonGetValue(u" [[[]]]]]]][] ", [])', u'[[[]]]]]]][]')

    test(r'llJsonGetValue(u"{\"a\":[1],\"a\":[2],\"a\":[3]}", [u"a",0])', u'3')
    test(r'llJsonGetValue(u"{\"a\":[2,3,[4]],\"a\":1}", [u"a", 2, 0])', JSON_INVALID)

    test(r'llJsonGetValue(u"{\"a\":1,}",[u"a"])', JSON_INVALID)


    test(r'llJsonGetValue(u"[3,4,5,[6,7,8],:9,10]", [1])', JSON_INVALID) # *ing crap! ALL entries are tested before the result is returned...
    test(r'llJsonGetValue(u"[3,4,5,[6,7,:8]]", [1])', u'4') # ... But only at the current nesting level. Crazy.
    test(r'llJsonGetValue(u"[3,4,5,{:6,7,8}]", [1])', u'4')
    test(r'llJsonGetValue(u"[[[[1,2,[3],4],5],6],7]", [0,0,0,2])', u'[3]')
    test(r'llJsonGetValue(u"[[[[1,2,[3],4],5],6],7]", [0,0,0,2,0])', u'3')
    # This suggests recursivity is used, once per nesting level, and that deeper levels remain unchecked:
    test(r'llJsonGetValue(u"[[[[1,2,[:3],4],5],6],7]", [0,0,0,2,0])', JSON_INVALID)
    test(r'llJsonGetValue(u"[[[[1,2,[:3],4],5],6],7]", [0,0,0,2])', u'[:3]')
    test(r'llJsonGetValue(u"[[[[1,2,[3],:4],5],6],7]", [0,0,0,2])', JSON_INVALID)
    test(r'llJsonGetValue(u"[[[[1,2,[3],4],:5],6],7]", [0,0,0,2])', JSON_INVALID)
    test(r'llJsonGetValue(u"[[[[1,2,[3],4],5],:6],7]", [0,0,0,2])', JSON_INVALID)
    test(r'llJsonGetValue(u"[[[[1,2,[3],4],5],6],:7]", [0,0,0,2])', JSON_INVALID)
    # same with value type
    test(r'llJsonValueType(u"[[[[1,2,[3],4],5],6],7]", [0,0,0,2,0])', JSON_NUMBER)
    test(r'llJsonValueType(u"[[[[1,2,[:3],4],5],6],7]", [0,0,0,2,0])', JSON_INVALID)
    test(r'llJsonValueType(u"[[[[1,2,[:3],4],5],6],7]", [0,0,0,2])', JSON_ARRAY)
    test(r'llJsonValueType(u"[[[[1,2,[3],:4],5],6],7]", [0,0,0,2])', JSON_INVALID)
    test(r'llJsonValueType(u"[[[[1,2,[3],4],:5],6],7]", [0,0,0,2])', JSON_INVALID)
    test(r'llJsonValueType(u"[[[[1,2,[3],4],5],:6],7]", [0,0,0,2])', JSON_INVALID)
    test(r'llJsonValueType(u"[[[[1,2,[3],4],5],6],:7]", [0,0,0,2])', JSON_INVALID)

    if 6495 in Bugs:
        # OH-MY-FLYINGSPAGHETTIMONSTER!!!!!! A nested string can't contain "]"!!!!
        # INCOMPAT: Too crazy. We're not emulating that.
        #test(r'llJsonGetValue(u"[[[[1,2,[\"],4]\",5],6],7]]]", [0,0,0,2])', JSON_INVALID)
        # Simpler test case:
        # INCOMPAT
        #test(r'llJsonGetValue(u"[[\"[\"]]", [0])', JSON_INVALID)
        #test(r'llJsonGetValue(u"[[\"]\"]]", [0])', JSON_INVALID)
        #test(r'llJsonGetValue(u"[{\"{\":0}]", [0])', JSON_INVALID)
        #test(r'llJsonGetValue(u"[{\"}\":0}]", [0])', JSON_INVALID)
        test(r'llJsonGetValue(u"[[\"{\"]]", [0])', u'["{"]')
        test(r'llJsonGetValue(u"[[\"}\"]]", [0])', u'["}"]')
        test(r'llJsonGetValue(u"[{\"[\":0}]", [0])', u'{"[":0}')
        test(r'llJsonGetValue(u"[{\"]\":0}]", [0])', u'{"]":0}')
        # And proper balance removes the problem.
        test(r'llJsonGetValue(u"[[\"[]\"]]", [0])', u'["[]"]')
        # Even if not too proper.
        test(r'llJsonGetValue(u"[[\"][\"]]", [0])', u'["]["]')
        test(r'llJsonGetValue(u"[[\"][\"]]", [0,0])', u'][')
        #INCOMPAT
        #test(r'llJsonGetValue(u"[[][]]", [0])', u'[][]')
        #test(r'llJsonValueType(u"[[][]]", [0])', JSON_ARRAY)
        test(r'llJsonGetValue(u"[[][]]", [0,0])', JSON_INVALID)
        # Depth zero is special, no error there:
        test(r'llJsonGetValue(u"[\"[\"]", [])', u'["["]')
        test(r'llJsonGetValue(u"[\"[\"]", [0])', u'[')

        # INCOMPAT
        #test(r'llJsonGetValue(u"[\"[\\\"\\\\\"]", [0])', JSON_INVALID) # File this under WTF.
        #test(r'llJsonGetValue(u"[\"[\\\"\\\\\"", [0])', u'"[\\') # Ditto.
        #test(r'llJsonGetValue(u"[\"\\\\\"]", [0])', JSON_INVALID) # This seems to be the essence.
        #test(r'llJsonGetValue(u"[\"\\\\\"", [0])', u'\\')
    else:
        test(r'llJsonGetValue(u"[[[[1,2,[\"],4]\",5],6],7]]]", [0,0,0,2])', u'["],4]",5]')
        # INCOMPAT
        #test(r'llJsonGetValue(u"[[\"[\"]]", [0])', u'["["]')
        #test(r'llJsonGetValue(u"[[\"]\"]]", [0])', u'["]"]')
        #test(r'llJsonGetValue(u"[{\"{\":0}]", [0])', u'{"{":0}')
        #test(r'llJsonGetValue(u"[{\"}\":0}]", [0])', u'{"}":0}')
        test(r'llJsonGetValue(u"[[\"{\"]]", [0])', u'["{"]')
        test(r'llJsonGetValue(u"[[\"}\"]]", [0])', u'["}"]')
        test(r'llJsonGetValue(u"[{\"[\":0}]", [0])', u'{"[":0}')
        test(r'llJsonGetValue(u"[{\"]\":0}]", [0])', u'{"]":0}')
        test(r'llJsonGetValue(u"[[\"[]\"]]", [0])', u'["[]"]')
        test(r'llJsonGetValue(u"[[\"][\"]]", [0])', u'["]["]')
        test(r'llJsonGetValue(u"[[\"][\"]]", [0,0])', u'][')
        # INCOMPAT
        #test(r'llJsonGetValue(u"[[][]]", [0])', u'[][]')
        #test(r'llJsonValueType(u"[[][]]", [0])', JSON_ARRAY)
        test(r'llJsonGetValue(u"[[][]]", [0,0])', JSON_INVALID)
        test(r'llJsonGetValue(u"[\"[\"]", [])', u'["["]')
        test(r'llJsonGetValue(u"[\"[\"]", [0])', u'[')

        # INCOMPAT
        #test(r'llJsonGetValue(u"[\"[\\\"\\\\\"]", [0])', u'["\\')
        #test(r'llJsonGetValue(u"[\"[\\\"\\\\\"", [0])', JSON_INVALID)
        #test(r'llJsonGetValue(u"[\"\\\\\"]", [0])', u'\\')
        #test(r'llJsonGetValue(u"[\"\\\\\"", [0])', JSON_INVALID)

    # This proves that it converts to integer when traversing an array.
    # MAINT-2671 introduced a check for the first character, which is reflected
    # in the tests. Apparently the test is just Firstchar in [0-9].
    test(r'llJsonGetValue(u"[[1,2],{\"a\":0}]",[u"0.9999999999"])', u"[1,2]")
    # This test mimicks the test for MAINT-2671 in the wiki tests.
    test(r'llJsonGetValue(u"[[1,2],{\"a\":0}]",[u"f"])', JSON_INVALID)
    # FIXME: llJsonSetValue pending
    '''test(r'llJsonSetValue(u"", [u"0",1], u"1")', JSON_INVALID)
    test(r'llJsonSetValue(u"", [u"0",0], u"1")', u'{"0":[1]}')
    test(r'llJsonSetValue(u"", [u"0",0.0], u"1")', u'{"0":}')
    test(r'llJsonSetValue(u"", [u"0",0.0], u"1")', u'{"0":}')
    test(r'llJsonSetValue(u"", [u"0",u"0"], u"1")', u'{"0":{u"0":1}}')
    test(r'llJsonSetValue(u"", [u"0",Key(u"0")], u"1")', u'{"0":{u"0":1}}')
    test(r'llJsonSetValue(u"", [u"0",ZERO_VECTOR], u"1")', u'{"0":}')
    test(r'llJsonSetValue(u"", [ZERO_VECTOR], u"1")', u'')
    test(r'llJsonSetValue(u" [ 1 , 2 , [ 3 , 4 ] ] ", [0], u"1")', u'[1,2,[ 3 , 4 ]]')

    test(r'llJsonSetValue(u" { \"a\" : [ 1 ] , \"a\" : 2 } ",[u"a"], u"3")', u'{"a":3}')
    test(r'llJsonSetValue(u" { \"a\" : [ 1 ] , \"a\" : 2 } ",[u"a",0], u"3")', u'{"a":[3]}')
    test(r'llJsonSetValue(u" { \"a\" : [ 1 ] , \"a\" : 2 } ",[u"a",0], u"3")', u'{"a":3}')
    test(r'llJsonSetValue(u" { \"a\" : [ 1 , 2 ] , \"a\" : 3 } ",[u"a",0], u"4")', u'{"a":[4]}')
    # This proves the path must be found in the *last* match of an object:
    test(r'llJsonSetValue(u" { \"a\" : [ 1 , 2 ] , \"a\" : 3 } ",[u"a",1], u"4")', JSON_INVALID)
    test(r'llJsonSetValue(u" { \"a\" : 3 , \"a\" : [ 1 , 2 ] } ",[u"a",0], u"4")', u'{"a":[4,2]}')
    test(r'llJsonSetValue(u" { \"a\" : 3 , \"a\" : [ 1 , 2 ] } ",[u"a",1], u"4")', u'{"a":[1,4]}')
    test(r'llJsonSetValue(u" { \"a\" : 3 , \"b\" : 6, \"a\" : [ 1 , 2 ] } ",[u"a",1], u"4")', u'{"a":[1,4],"b":6}')
    test(r'llJsonSetValue(u" { \"b\" : 3 , \"a\" : 6, \"b\" : [ 1 , 2 ] } ",[u"b",1], u"4")', u'{"a":6,"b":[1,4]}')
    # This proves llJsonSetValue sorts the items it recurses into and keeps
    # just one copy of them.
    # No "a13" in original but some dupes:
    test(r'llJsonSetValue(u"{\"b\":{\"a9\":15,\"a0\":16,\"a7\":27,\"a20\":28,'
    r'\"a7\":21,\"a21\":22,\"a8\":7,\"a24\":8,\"a9\":13,\"a0\":14,\"a26\":29,'
    r'\"a20\":30,\"a0\":11,\"a26\":12,\"a14\":19,\"a10\":20,\"a11\":1,'
    r'\"a12\":2,\"a9\":23,\"a4\":24,\"a20\":5,\"a17\":6,\"a21\":17,\"a3\":18,'
    r'\"a4\":9,\"a1\":10,\"a10\":3,\"a26\":4,\"a21\":25,\"a17\":26},'
    r'\"a\":{\"a9\":15,\"a0\":16,\"a7\":27,\"a20\":28,\"a7\":21,\"a21\":22,'
    r'\"a8\":7,\"a24\":8,\"a9\":13,\"a0\":14,\"a26\":29,\"a20\":30,\"a0\":11,'
    r'\"a26\":12,\"a14\":19,\"a10\":20,\"a11\":1,\"a12\":2,\"a9\":23,\"a4\":24'
    r',\"a20\":5,\"a17\":6,\"a21\":17,\"a3\":18,\"a4\":9,\"a1\":10,\"a10\":3'
    r',\"a26\":4,\"a21\":25,\"a17\":26}}", [u"b",u"a13"], u"\"*********\"")',
    u'{"a":{"a9":15,"a0":16,"a7":27,"a20":28,"a7":21,"a21":22,"a8":7,'
    u'"a24":8,"a9":13,"a0":14,"a26":29,"a20":30,"a0":11,"a26":12,"a14":19,'
    u'"a10":20,"a11":1,"a12":2,"a9":23,"a4":24,"a20":5,"a17":6,"a21":17,'
    u'"a3":18,"a4":9,"a1":10,"a10":3,"a26":4,"a21":25,"a17":26},'
    u'"b":{"a0":11,"a1":10,"a10":3,"a11":1,"a12":2,"a13":"*********","a14":19,'
    u'"a17":26,"a20":5,"a21":25,"a24":8,"a26":4,"a3":18,"a4":9,"a7":21,"a8":7,'
    u'"a9":23}}')
    # Two "a13" in original (and some other dups such as "a9" which appears three times):
    test(r'llJsonSetValue(u"{\"b\":{\"a12\":19,\"a23\":20,\"a9\":7,\"a7\":8,'
    r'\"a28\":21,\"a15\":22,\"a9\":17,\"a22\":18,\"a1\":13,\"a26\":14,'
    r'\"a7\":23,\"a13\":24,' # <-- 1st instance of "a13"
    r'\"a9\":3,\"a26\":4,\"a14\":9,\"a24\":10,\"a12\":29,'
    r'\"a16\":30,\"a25\":5,\"a23\":6,\"a2\":11,\"a13\":12,' # <-- 2nd instance
    r'\"a11\":1,\"a12\":2,'
    r'\"a15\":25,\"a25\":26,\"a25\":27,\"a18\":28,\"a22\":15,\"a27\":16},'
    r'\"a\":{\"a12\":19,\"a23\":20,\"a9\":7,\"a7\":8,\"a28\":21,\"a15\":22,'
    r'\"a9\":17,\"a22\":18,\"a1\":13,\"a26\":14,\"a7\":23,\"a13\":24,\"a9\":3,'
    r'\"a26\":4,\"a14\":9,\"a24\":10,\"a12\":29,\"a16\":30,\"a25\":5,'
    r'\"a23\":6,\"a2\":11,\"a13\":12,\"a11\":1,\"a12\":2,\"a15\":25,'
    r'\"a25\":26,\"a25\":27,\"a18\":28,\"a22\":15,\"a27\":16}}",'
    r' [u"b",u"a13"],u"\"*********\"")',
    # "a" is respected but put *before* "b".
    u'{"a":{"a12":19,"a23":20,"a9":7,"a7":8,"a28":21,"a15":22,"a9":17,'
    u'"a22":18,"a1":13,"a26":14,"a7":23,"a13":24,"a9":3,"a26":4,"a14":9,'
    u'"a24":10,"a12":29,"a16":30,"a25":5,"a23":6,"a2":11,"a13":12,"a11":1,'
    u'"a12":2,"a15":25,"a25":26,"a25":27,"a18":28,"a22":15,"a27":16},'
    # "b" has only one of each including "a13", and has its elements sorted.
    # Only the last element of each repetition is kept.
    u'"b":{"a1":13,"a11":1,"a12":2,"a13":"*********","a14":9,"a15":25,'
    u'"a16":30,"a18":28,"a2":11,"a22":15,"a23":6,"a24":10,"a25":27,"a26":4,'
    u'"a27":16,"a28":21,"a7":23,"a9":3}}')

    # Incoming facepalm series.
    test(r'llJsonSetValue(u"{\"a\":3,\"b\":4}", [], u" "+JSON_DELETE+u" ")', JSON_DELETE)
    test(r'llJsonSetValue(u"{\"a\":3,\"b\":4}", [], u" "+JSON_INVALID+u" ")', u'')
    test(r'llJsonSetValue(u"{\"a\":3,\"b\":4}", [], u" "+JSON_STRING+u" ")', u'"' + JSON_STRING + u'"')
    test(r'llJsonSetValue(u"{\"a\":3,\"b\":4}", [], JSON_NUMBER)', u'"' + JSON_NUMBER + u'"')
    test(r'llJsonSetValue(u"{\"a\":3,\"b\":4}", [], JSON_OBJECT)', u'"' + JSON_OBJECT + u'"')
    test(r'llJsonSetValue(u"{\"a\":3,\"b\":4}", [], JSON_ARRAY)', u'"' + JSON_ARRAY + u'"')
    test(r'llJsonSetValue(u"[1,2,3]", [0,0,0], JSON_INVALID)', u'[[[]],2,3]')
    test(r'llJsonSetValue(u"[1,2,3]", [0,0,0], JSON_DELETE)', JSON_INVALID)

    # These are sane-ish or at least they make sense.
    test(r'llJsonSetValue(u"[1,2,3]", [0,0,0], u"")', u'[[[""]],2,3]')
    test(r'llJsonSetValue(u"{\"a\":3,\"b\":4}", [], u" "+JSON_TRUE+u" ")', u'true')
    test(r'llJsonSetValue(u"{\"a\":3,\"b\":4}", [], JSON_FALSE)', u'false')
    test(r'llJsonSetValue(u"{\"a\":3,\"b\":4}", [], JSON_NULL)', u'null')

    # Elements are completely replaced if the type doesn't match.
    # E.g. an object turns into a zero-length array if necessary.
    # And there's no numeric evaluation of strings.
    test(r'llJsonSetValue(u"{\"a\":3}", [u"a"], u"")', u'{"a":""}')
    test(r'llJsonSetValue(u"{\"a\":3}", [u"0"], u"")', u'{"0":"","a":3}')
    test(r'llJsonSetValue(u"[\"a\",3]", [u"0"], u"")', u'{"0":""}')
    test(r'llJsonSetValue(u"[\"a\",3]", [1, u"0"], u"")', u'["a",{"0":""}]')
    test(r'llJsonSetValue(u"{\"a\":3}", [1, u"0"], u"")', JSON_INVALID)
    test(r'llJsonSetValue(u"{\"a\":3}", [0, u"0"], u"")', u'[{"0":""}]')
    test(r'llJsonSetValue(u"{\"a\":3,\"b\":4}", ["a"], u" "+JSON_DELETE+u" ")', u'{"b":4}')
'''
    # JSON tests from the wiki
    test_types();
    test_get_value();
    # FIXME: llJsonSetValue pending
    #test_set_value();
    test_json_to_list();
    test_list_to_json();
    # FIXME: llJsonSetValue pending
    #test_strings_with_escaped_chars();
    test_jira_fixes();

do_tests()

print
print tests, "tests in suite"
print tests-untested, "tests run"
print errors, "errors found"
print tests-untested-errors, "tests passed"

# Disassemble
#import dis
#dis.disco(sub.__code__)

#from deadcode import finddeadcode
#
#dead = finddeadcode(InternalUTF8toString.__code__)
#
#if dead:
#    for line in dead:
#        print "Line %d is unreachable" % line
#elif dead == []:
#    print "No unreachable lines found."
#else:
#    print "No line info in code?!"
