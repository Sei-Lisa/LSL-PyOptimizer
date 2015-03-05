#    (C) Copyright 2015 Sei Lisa. All rights reserved.
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

# JSON functions

import re
import math
from lslcommon import *
from lslbasefuncs import llStringTrim, isstring, islist, InternalTypecast

# INCOMPATIBILITY NOTE: The JSON functions in SL have very weird behaviour
# in corner cases. Despite our best efforts, that behaviour is not replicated
# here, as doing so proved to be too difficult to investigate and implement.
# The functions in here behave somewhat more sanely in these corner cases than
# in SL, and may therefore fail to reproduce the same results as SL does.

# If you wish to maintain compatibility, you can disable the JSON functions
# by commenting out the 'from lsljson import *' line in lslfuncs.py.

JSON_INVALID = u'\uFDD0'
JSON_OBJECT  = u'\uFDD1'
JSON_ARRAY   = u'\uFDD2'
JSON_NUMBER  = u'\uFDD3'
JSON_STRING  = u'\uFDD4'
JSON_NULL    = u'\uFDD5'
JSON_TRUE    = u'\uFDD6'
JSON_FALSE   = u'\uFDD7'
JSON_DELETE  = u'\uFDD8'

JSON_APPEND  = -1

jsonesc_re = re.compile(u'[\x08\x09\x0A\x0C\x0D"/\\\\]')
jsonesc_dict = {u'\x08':ur'\b', u'\x09':ur'\t', u'\x0A':ur'\n', u'\x0C':ur'\f',
                u'\x0D':ur'\r', u'"':ur'\"', u'/':ur'\/', u'\\':ur'\\'}
jsonunesc_dict = {u'b':u'\x08', u't':u'\x09', u'n':u'\x0A', u'f':u'\x0C', u'r':u'\x0D'}

# LSL JSON numbers differ from standard JSON numbers in many respects:
#      Numbers starting with 0 are allowed, e.g. 01.3e4, 00042
#      .5 is allowed.
#      1e+0 is NOT allowed (the + after the e, to be precise). BUG-6466.
#      . is allowed, as is -.e-0 etc.
#      1E is allowed.
#      E.2 is allowed.
#      E is allowed.
#      1E-1.2 is allowed.
# In general, the rule seems to be: at most one 'E' (optionally followed by a
# '-') and one '.', with optional digits interspersed and an optional initial
# minus sign.
#
# Our RE below checks for the two possible orders of '.' and 'E'. One branch
# must have a mandatory 'E'; in the other everything is optional but it must
# have at least 1 character (done by the lookahead assertion).
#
# The capturing groups serve to check whether the first variant was taken, and
# whether there is something after the digits in the second variant. If both
# are empty, then the match is just one or more digits preceded by an optional
# minus sign (i.e. an integer). That's used by llJson2List to return integer
# elements when appropriate.

# Real JSON number parser:
#jsonnum_re = re.compile(ur'-?(?:[1-9][0-9]*|0)(?:\.[0-9]+)?(?:[Ee][+-]?[0-9]+)?')

# BUG-6466 active:
jsonnumbug_re = re.compile(ur'-?(?:[0-9]*([Ee])-?[0-9]*\.?[0-9]*|(?=[0-9Ee.])[0-9]*(\.?[0-9]*(?:[Ee]-?)?[0-9]*))')
# BUG-6466 fixed:
# The new RE is just a modified version of the crap, allowing + exponents and
# disallowing zeros, sometimes even when legal (e.g. 0e0)
#jsonnum_re = re.compile(ur'-?(?:(?=[1-9]|\.(?:[^e]|$)|0(?:[^0-9e]|$))[0-9]*([Ee])[+-]?[0-9]*\.?[0-9]*|(?=[1-9]|\.(?:[^e]|$)|0(?:[^0-9e]|$))[0-9]*(\.?[0-9]*(?:[Ee][+-]?)?[0-9]*))')
# They've fixed BUG-6657 by bringing BUG-6466 back to life.
jsonnum_re = re.compile(ur'-?(?:[0-9]*([Ee])-?[0-9]*\.?[0-9]*|(?=[0-9Ee.])[0-9]*(\.?[0-9]*(?:[Ee]-?)?[0-9]*))')

jsonstring_re = re.compile(ur'"(?:[^"\\]|\\.)*"')

# This might need some explanation. The ] and - are included in the first
# set, the ] in the first after the ^ and the - in the last positions of
# the set as required by RE syntax. The [ is part of it and isn't special,
# though it confuses things. The set comprises any character not in
# -{}[],:"0123456789
# The second set comprises zero or more characters not in ,:]}
#word_re = re.compile(ur'[^][{}0-9",:-][^]},:]*')
# Screw that, we're using just a fallback.
jsoncatchall_re = re.compile(u'(.*?)[\x09\x0A\x0B\x0C\x0D ]*(?:[]},]|$)')

digits_re = re.compile(u'[0-9]{1,9}')


class EInternalJsonInvalid(Exception):
    """Used to force return of JSON_INVALID from child functions"""
    pass

def InternalJsonQuote(s):
    return u'"' + jsonesc_re.sub(lambda x: jsonesc_dict[x.group()], s) + u'"'

def InternalJsonUnquote(s):
    """Relaxed unquote with LSL rules. Assumes string starts and ends in ",
    may contain " and may end in \" too (i.e. malformed). E.g. "a"b\" is a
    valid string for this function and the result is a"b\
    """
    assert s != u''
    assert s[0] == s[-1] == u'"' and s[1:2]

    ret = u''
    esc = False
    for c in s[1:-1]:
        if esc:
            try:
                ret += jsonunesc_dict[c]
            except KeyError:
                ret += c
            esc = False
        else:
            if c == u'\\':
                esc = True
            else:
                ret += c
    if esc:
        return ret + u'\\'
    return ret

def InternalJsonUnquoteX(s):
    """Rigorous unquote; checks for quotes at the beginning and end only."""
    esc = last = False
    first = True

    ret = u''
    for c in s:
        if last:
            break
        if esc:
            try:
                ret += jsonunesc_dict[c]
            except:
                ret += c
            esc = False
            first = False
        elif first:
            if c != u'"': break
            first = False
        elif c == u'"':
            last = True
            first = False
        elif c == u'\\':
            esc = True
        else:
            ret += c
    else:
        if not first and last:
            return ret
    return s # malformed string, return the original

def InternalJsonF2S(f):
    if math.isnan(f):
        return u'nan'
    if math.isinf(f):
        return u'inf' if f > 0 else u'-inf'
    return u'%.6f' % f

def InternalJsonScanMatching(json, idx):
    """Shortcut: scan for a matching pair of {} or [] with proper nesting
    and string handling, with no validity check other than well-formedness,
    meaning all {} or [] must match.
    """
    matching = json[idx]
    matching += '}' if json[idx] == '{' else ']'
    level = 1
    str = False
    esc = False
    for i in xrange(idx+1, len(json)):
        c = json[i]
        if str:
            if esc:
                esc = False
            elif c == u'\\':
                esc = True
            elif c == u'"':
                str = False
        elif c == u'"':
            str = True
        elif c in matching:
            if c == matching[0]:
                level += 1
            else:
                level -= 1
            if not level:
                return i+1
    return None

def InternalElement2Json(elem, ParseNumbers = True):
    telem = type(elem)
    if telem == unicode:
        elem = llStringTrim(elem, 3) # STRING_TRIM
        if elem == u'':
            return u'""'
        # Yes, these are checked after trimming. Don't facepalm too hard.
        if elem == JSON_NULL:
            return u'null'
        if elem == JSON_TRUE:
            return u'true'
        if elem == JSON_FALSE:
            return u'false'
        if elem[0] == elem[-1] == u'"' and elem[1:2] or elem in ('null','false','true') \
                or elem[0] == u'[' and elem[-1] == u']' \
                or elem[0] == u'{' and elem[-1] == u'}':
            return elem

        if ParseNumbers:
            match = (jsonnumbug_re if 6466 in Bugs else jsonnum_re).match(elem)
            if match and match.end() == len(elem):
                return elem

        if elem == JSON_INVALID:
            return u''

        return InternalJsonQuote(elem)

    if telem == Key:
        return u'"' + unicode(elem) + u'"'
    if telem in (Vector, Quaternion):
        return u'"<' + u', '.join([InternalJsonF2S(x) for x in elem]) + u'>"'
    if telem == float:
        return InternalJsonF2S(elem)
    # Integer
    return unicode(elem)

def InternalJsonGetToken(json, idx):

    #start = idx
    num_re = jsonnumbug_re if 6466 in Bugs else jsonnum_re

    L = len(json)
    while idx < L:
        c = json[idx]
        if c not in u'\x09\x0A\x0B\x0C\x0D ':
            break
        idx += 1

    if idx >= L:
        return (idx, idx, None)

    c = json[idx]
    if c in u',:{}[]':
        return (idx, idx+1, c)

    match = jsonstring_re.match(json, idx)
    if match:
        return (idx, match.end(), JSON_STRING)

    match = num_re.match(json, idx)
    if match:
        return (idx, match.end(), JSON_NUMBER)

    match = jsoncatchall_re.match(json, idx) # matches always, even if empty string
    s = match.group(1)
    if s in (u'null', u'true', u'false'):
        return (idx, match.end(1),
            JSON_NULL if s == u'null' else JSON_TRUE if s == u'true' else JSON_FALSE)
    return (idx, match.end(1), JSON_INVALID)

def InternalJsonGetTokenFull(json, idx):
    ret = InternalJsonGetToken(json, idx)
    if ret[2] in (u'{', u'['):
        match = InternalJsonScanMatching(json, ret[0])
        if match is not None:
            return (ret[0], match, JSON_OBJECT if ret[2] == u'{' else JSON_ARRAY)
    return ret

def InternalJsonPathMatches(key, pathelem):
    if type(key) == type(pathelem) == int or type(key) == unicode and isinstance(pathelem, unicode):
        return key == pathelem
    if type(key) == unicode and type(pathelem) == int:
        raise EInternalJsonInvalid
    # one combo remains - key is numeric and pathelem is unicode or Key
    match = digits_re.match(pathelem)
    if not match:
        raise EInternalJsonInvalid
    return key == int(match.group())

def InternalJsonFindValue(json, tgtpath, ReturnsToken, SetRules = False):

    # Building a function that meets the strange requisites of LL's json is not easy.
    # These requisites include syntax-checking of all items at the current level,
    # but not of items at a deeper nesting level.

    # Making it one-pass iterative O(len) instead of recursive O(depth*len) is even
    # more of a challenge, especially with these constraints.

    token = InternalJsonGetToken(json, 0)

    if tgtpath == []:
        # No nesting receives special treatment.
        if token[2] in (JSON_NUMBER, JSON_STRING, JSON_NULL, JSON_TRUE, JSON_FALSE, JSON_INVALID):
            if InternalJsonGetToken(json, token[1])[2] is None:
                if ReturnsToken:
                    return token
                if token[2] == JSON_NUMBER:
                    return json[token[0]:token[1]]
                if token[2] == JSON_STRING:
                    return InternalJsonUnquote(json[token[0]:token[1]])
                if token[2] == JSON_INVALID:
                    # Accept malformed strings if they start and end in quotes
                    s = json[token[0]:token[1]]
                    if s[1:2] and s[0] == s[-1] == u'"':
                        return InternalJsonUnquote(s)
                return token[2]
            return JSON_INVALID
        if token[2] not in (u'{', u'['):
            return JSON_INVALID

        json = llStringTrim(json, 2) # STRING_TRIM_RIGHT
        if json[-1] == u'}' and token[2] == u'{':
            if ReturnsToken:
                return (token[0], len(json), JSON_OBJECT)
            return json[token[0]:]
        if json[-1] == u']' and token[2] == u'[':
            if ReturnsToken:
                return (token[0], len(json), JSON_ARRAY)
            return json[token[0]:]
        return JSON_INVALID

        # This would be the code if there was proper scanning.
        #match = InternalJsonScanMatching(json, token[0])
        #if match is None or InternalJsonGetToken(json, match)[2] is not None:
        #    return JSON_INVALID
        #if ReturnsType: # this has been changed tho' - review if ever used
        #    return JSON_OBJECT if token[2] == u'{' else JSON_ARRAY
        #return json[token[0]:match]

    if token[2] not in (u'{', u'['):
        return JSON_INVALID

    # Follow the path
    L = len(tgtpath)
    # For the current position, matchlvl keeps track of how many levels are
    # matched. When matchlvl == L, we are at the item of interest.
    # For example: if we're at the ! in [1.0, "y", true, [1, ![6], {"a":5}]]
    # and the path is [3, 2, "a"], matchlvl will be 1 (meaning the first level
    # of the path, i.e. position 3, is matched, but we're not in sub-position
    # 2 yet).
    matchlvl = 0
    ret = None # the target token, if found, or None if not

    # Keeps track of what we have opened so far.
    stk = [token[2]]

    # This tracks the current key within an array or object. Here we assume
    # it's an array; if it's an object, the item key will replace it anyway.
    curkey = 0

    just_open = True
    just_closed = False

    # Load next token
    token = InternalJsonGetToken(json, token[1])

    try:
        while True:
            # Process value if it can be present
            kind = token[2]
            if not (just_closed or
                    just_open and kind in (u'}', u']')):
                # Item processing.
                # Not entering here immediately after a } or ] (just_closed)
                # or after a { or [ followed by } or ] (just_open...)
                just_open = False
                if kind in u':,]}' or kind == JSON_INVALID:
                    return JSON_INVALID
                if stk[-1] == u'{':
                    # Read the current key
                    if kind != JSON_STRING:
                        return JSON_INVALID
                    colon = InternalJsonGetToken(json, token[1])
                    if colon[2] != u':':
                        return JSON_INVALID
                    curkey = InternalJsonUnquote(json[token[0]:token[1]])
                    token = InternalJsonGetToken(json, colon[1])
                    kind = token[2]
                    del colon
                if matchlvl < L and InternalJsonPathMatches(curkey, tgtpath[matchlvl]):
                    # Descend to this level
                    matchlvl += 1
                    ret = None # because e.g. llJsonGetValue("{\"a\":[1],\"a\":2}",["a",0])==JSON_INVALID
                    if matchlvl == L:
                        if kind in u'{[':
                            match = InternalJsonScanMatching(json, token[0])
                            if match is None:
                                return JSON_INVALID
                            token = (token[0], match, JSON_OBJECT if token[2] == u'{' else JSON_ARRAY)
                        ret = token
                        matchlvl -= 1
                    elif kind in u'{[':
                        stk.append(token[2])
                        curkey = 0
                        just_open = True
                        token = InternalJsonGetToken(json, token[1])
                        continue
                else:
                    # We're skipping the element
                    if kind in u'[{':
                        match = InternalJsonScanMatching(json, token[0])
                        if match is None:
                            return JSON_INVALID
                        token = (None, match) # HACK: shortcut to: (token[0], match, JSON_OBJECT if kind == u'{' else JSON_ARRAY)
                        just_closed = True

                token = InternalJsonGetToken(json, token[1]) # prepare next token
                kind = token[2]

            just_closed = False
            # Process coma if it can be present
            if not just_open:
                if kind == u',':
                    token = InternalJsonGetToken(json, token[1]) # load next token
                    if stk[-1] == u'[':
                        curkey += 1
                    continue

            if kind == u'}' and stk[-1] == u'{' or kind == u']' and stk[-1] == u'[':
                stk = stk[:-1]
                matchlvl -= 1
                if stk == []:
                    if InternalJsonGetToken(json, token[1])[2] is None:
                        break # Yay! end of job!
                    return JSON_INVALID # No yay - something at end of string
                just_closed = True
                token = InternalJsonGetToken(json, token[1])
                continue

            return JSON_INVALID


    except EInternalJsonInvalid:
        return JSON_INVALID

    if ret is None:
        return JSON_INVALID
    if ReturnsToken:
        return ret
    if ret[2] == JSON_STRING:
        return InternalJsonUnquote(json[ret[0]:ret[1]])
    if ret[2] in (JSON_NUMBER, JSON_OBJECT, JSON_ARRAY):
        return json[ret[0]:ret[1]]
    return ret[2] # JSON_TRUE, JSON_FALSE, JSON_NULL

def InternalJson2Elem(json):
    if json == u'': # checking this now lets us check for json[0] and json[-1] later
        return u''

    if json == u'null':
        return JSON_NULL

    if json == u'false':
        return JSON_FALSE

    if json == u'true':
        return JSON_TRUE

    match = (jsonnumbug_re if 6466 in Bugs else jsonnum_re).match(json)
    if match and match.end() == len(json):
        # HACK: Use our RE to know if the number is an integer
        if not match.group(1) and not match.group(2):
            # we have just digits with optional minus sign, i.e. an integer
            if len(json) > 11: # surely overflown
                if json[0] == u'-':
                    return -2147483648
                return 2147483647
            # a bit harder to test; we could check in ASCII to avoid conversion
            # to long in 32 bit systems, but it's probably not worth the effort
            elem = int(json)
            if elem > 2147483647:
                return 2147483647
            if elem < -2147483648:
                return -2147483648
            return elem
        return InternalTypecast(json, float, InList=False, f32=True)

    # Malformed strings are valid, e.g. "a\" (final \" is converted into a \)
    if json[0] == json[-1] == u'"' and json[1:2]: # the latter check ensures len(json) > 1
        return InternalJsonUnquote(json)

    return json

def llJson2List(json):
    assert isstring(json)
    json = llStringTrim(json, 3) # STRING_TRIM

    if json == u'':
        return []

    if json[0] == u'[' and json[-1] == u']':
        # Array can of worms. Not all LSL quirks are implemented.
        ret = []
        token = InternalJsonGetTokenFull(json, 1)
        if token[2] == u']' and token[1] == len(json):
            return ret
        if token[2] == u':':
            return [JSON_INVALID]
        if token[2] == u',':
            ret.append(u'')
        else:
            ret.append(InternalJson2Elem(json[token[0]:token[1]]))
            token = InternalJsonGetTokenFull(json, token[1])
        while True:
            if token[2] == u']' and token[1] == len(json):
                break
            elif token[2] != u',':
                return [JSON_INVALID]
            token = InternalJsonGetTokenFull(json, token[1])
            if token[2] == u',' or token[2] == u']' and token[1] == len(json):
                ret.append(u'')
            else:
                if token[2] == u':':
                    return JSON_INVALID
                ret.append(InternalJson2Elem(json[token[0]:token[1]]))
                token = InternalJsonGetTokenFull(json, token[1])
        return ret

    if json[0] == u'{' and json[-1] == u'}':
        # Object can of worms. Worse than array. Not all LSL quirks are implemented.

        # Parse this grammar:
        # object: '{' complete_list incomplete_element '}' $
        # complete_list: <empty> | complete_list complete_element ','
        # complete_element: nonempty_string ':' value
        # incomplete_element: <empty> | value | string ':' value
        # string: '"' '"' | nonempty_string
        #
        # That allows:
        # {"a":1,"b":2,} # incomplete_element is empty
        # {"a":1,"b":2} # "b" is an incomplete_element
        # {2} # complete_list empty
        # {} # both empty
        # etc.

        ret = []
        token = InternalJsonGetTokenFull(json, 1)
        if token[2] == u'}' and token[1] == len(json):
            return ret
        if token[2] in (u':', u','):
            return [JSON_INVALID]

        while True:
            k = u''
            if token[2] == u'}' and token[1] == len(json):
                ret.append(k)
                ret.append(k)
                return ret
            if token[2] == JSON_STRING:
                colon = InternalJsonGetTokenFull(json, token[1])
                if colon[2] == u':':
                    k = InternalJsonUnquote(json[token[0]:token[1]])
                    token = InternalJsonGetTokenFull(json, colon[1])
            if token[2] in (u',', u':'):
                return [JSON_INVALID]
            ret.append(k)
            ret.append(InternalJson2Elem(json[token[0]:token[1]]))
            token = InternalJsonGetTokenFull(json, token[1])
            if token[2] == u'}' and token[1] == len(json):
                return ret
            if token[2] != u',' or k == u'':
                return [JSON_INVALID]
            token = InternalJsonGetTokenFull(json, token[1])

    return [InternalJson2Elem(json)]

def llJsonGetValue(json, lst):
    assert isstring(json)
    assert islist(lst)
    return InternalJsonFindValue(json, lst, ReturnsToken=False)

# llJsonSetValue was finally not implemented. This is a failed attempt
# at tackling it in the way that LSL does it.
'''def InternalJsonRecuriveSetValue(json, lst, val):
    # We give up and make it recursive

    if lst == []:
        if val == JSON_DELETE:
            return val
        return InternalElement2Json(val, ParseNumbers=True)

    ret = None
    lst0 = lst[0]
    tlst0 = type(lst0)
    if tlst0 == Key:
        tlst0 = unicode

    if val != JSON_DELETE:

        json = llStringTrim(json, 3) # STRING_TRIM
        if tlst0 == int and json[0:1] == u'[' and json[-1:] == u']':
            ret = []
            close = u']'
        if tlst0 == unicode and json[0:1] == u'{' and json[-1:] == u'}':
            ret = {}
            close = u'}'

    if ret is not None:
        if close: pass



def llJsonSetValue(json, lst, val):
    assert isstring(json)
    assert islist(lst)
    assert isstring(val)
    if lst == []:
        # [] replaces the entire string no matter if it was invalid
        if val == JSON_DELETE:
            return val # this is a special case for SetValue with []
        return InternalElement2Json(val, ParseNumbers=True)
    # Needs to cope with JSON_APPEND, JSON_DELETE, lastindex+1.
    # Needs to do deep assignment.

    # Recursive works best here
    return InternalJsonRecursiveSetValue(json, lst, val)

    return u"----unimplemented----"
'''

def llJsonValueType(json, lst):
    assert isstring(json)
    assert islist(lst)
    ret = InternalJsonFindValue(json, lst, ReturnsToken=True)
    if ret == JSON_INVALID:
        return ret
    return ret[2]

def llList2Json(kind, lst):
    assert isstring(kind)
    assert islist(lst)

    if kind == JSON_OBJECT:
        ret = u'{'
        if len(lst) & 1:
            return JSON_INVALID
        for i in xrange(0, len(lst), 2):
            if ret != u'{':
                ret += u','
            ret += InternalJsonQuote(lst[i]) + u':' + InternalElement2Json(lst[i+1], ParseNumbers=False)

        ret += u'}'

    elif kind == JSON_ARRAY:
        ret = u'['
        if lst:
            ret += InternalElement2Json(lst[0], ParseNumbers=False)
            del lst[0]
            for elem in lst:
                ret += u',' + InternalElement2Json(elem, ParseNumbers=False)
        ret += u']'

    else:
        ret = JSON_INVALID

    return ret
