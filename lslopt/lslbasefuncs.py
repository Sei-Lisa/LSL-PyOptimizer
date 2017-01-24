#    (C) Copyright 2015-2016 Sei Lisa. All rights reserved.
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

# This module is used by the optimizer for resolving constant values.
#
# The functions it implements are all functions that always return the same
# result when given the same input, and that have no side effects.
#
# For example, llAbs() is here, but llGetPos() is not, because it doesn't
# always return the same result.
#
# This implies that functions present in this module can be precomputed if
# their arguments are constants.
#
# In some instances, the result can't be computed; in these cases the function
# raises a LSLCantCompute exception that is caught by the optimizer to leave
# the expression unchanged. For example, llBase64ToInteger("AA") returns
# unpredictable garbage in the low bytes in LSL, so it is left unchanged.
#
# The JSON functions have been separated to their own module.

import re
from lslcommon import *
import lslcommon
from ctypes import c_float
import math
import hashlib
from base64 import b64encode, b64decode


# Regular expressions used along the code. They are needed mainly because
# Python lacks a C-like strtod/strtol (it comes close, but it is very picky
# with what it accepts). We need to extract the number part of a string, or
# Python will complain.
# Also, Base64 needs the correct count of characters (len mod 4 can't be = 1).
# The RE helps both in isolating the Base64 section and in trimming out the
# offending characters; it just doesn't help with padding, with which Python is
# also picky. We deal with that in the code by padding with '='*(-length & 3).

# Despite what http://www.gnu.org/software/libc/manual/html_node/Parsing-of-Floats.html#Parsing-of-Floats
# says, NaN(chars) does not work in LSL (which is relevant in vectors).
# Note infinity vs. inf is necessary for parsing vectors & rotations,
# e.g. (vector)"<1,inf,infix>" is not valid but (vector)"<1,inf,infinity>" is
# as is (vector)"<1,inf,info>". The 1st gives <0,0,0>, the others <1,inf,inf>.
# The lookahead (?!i) is essential for parsing them that way without extra code.
# Note that '|' in REs is order-sensitive.
float_re = re.compile(ur'^\s*[+-]?(?:0(x)(?:[0-9a-f]+(?:\.[0-9a-f]*)?|\.[0-9a-f]+)(?:p[+-]?[0-9]+)?'
                      ur'|(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)(?:e[+-]?[0-9]+)?|inf|nan)',
                      re.I)
vfloat_re = re.compile(ur'^\s*[+-]?(?:0(x)(?:[0-9a-f]+(?:\.[0-9a-f]*)?|\.[0-9a-f]+)(?:p[+-]?[0-9]+)?'
                      ur'|(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)(?:e[+-]?[0-9]+)?|infinity|inf(?!i)|nan)',
                      re.I)

int_re = re.compile(ur'^0(x)[0-9a-f]+|^\s*[+-]?[0-9]+', re.I)

key_re = re.compile(ur'^[0-9a-f]{8}(?:-[0-9a-f]{4}){4}[0-9a-f]{8}$', re.I)

b64_re = re.compile(ur'^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2,3})?')

ZERO_VECTOR      = Vector((0.0, 0.0, 0.0))
ZERO_ROTATION    = Quaternion((0.0, 0.0, 0.0, 1.0))
NULL_KEY         = u'00000000-0000-0000-0000-000000000000'

Infinity = float('inf')
Indet = Infinity * 0
NaN = -Indet # Don't use float("nan") - Windows gets upset.

class ELSLTypeMismatch(Exception):
    def __init__(self):
        super(ELSLTypeMismatch, self).__init__(u"Type mismatch")

class ELSLMathError(Exception):
    def __init__(self):
        super(ELSLMathError, self).__init__(u"Math Error")

class ELSLInvalidType(Exception):
    def __init__(self):
        super(ELSLInvalidType, self).__init__(u"Internal error: Invalid type")

class ELSLCantCompute(Exception):
    pass

# We don't yet support the LSO string model (arbitrary zero-terminated byte
# sequences). This exception is  triggered to report attempts at using it.
class ELSONotSupported(Exception):
    pass

# LSL types are translated to Python types as follows:
# * LSL string -> Python unicode
# * LSL key -> Key (class derived from unicode, no significant changes except __repr__)
# * LSL integer -> Python int (should never be long)
# * LSL float -> Python float
# * LSL vector -> Vector (class derived from Python tuple) of 3 numbers (float)
# * LSL rotation -> Quaternion (class derived from Python tuple) of 4 numbers (float)
# * LSL list -> Python list

Types = {
        int:            1, # TYPE_INTEGER
        float:          2, # TYPE_FLOAT
        unicode:        3, # TYPE_STRING
        Key:            4, # TYPE_KEY
        Vector:         5, # TYPE_VECTOR
        Quaternion:     6, # TYPE_ROTATION
        list:           0, # TYPE_INVALID
        }

# Utility functions

def F32(f, f32=True):
    """Truncate a float to have a precision equivalent to IEEE single"""

    if not f32: # don't truncate
        return f

    if isinstance(f, tuple): # vector, quaternion
        return f.__class__(F32(i) for i in f)

    # Alternative to the big blurb below. This relies on the machine using IEEE-754, though.

    # Using array:
    #from array import array
    #return array('f',(f,))[0]

    # Using struct:
    #from struct import pack, unpack
    #return unpack('f', pack('f', f))[0]

    # Using numpy:
    #import numpy
    #return float(numpy.float32(f))

    # Using ctypes:
    #from ctypes import c_float
    return c_float(f).value

# These are other approaches that are not fully debugged:

# This one is tested against c_float, but not carefully verified:
#    if math.isnan(f) or math.isinf(f) or f == 0.0:
#        return f
#
#    m, x = math.frexp(abs(f))
#
#    if x > 128:
#        return math.copysign(Infinity, f)
#
#    if x < -125:
#        m = math.ldexp(m, x + 149)
#        x = -125
#    else:
#        m = m * 0x1000000
#
#    frac = m % 1
#    m -= frac
#    assert m.is_integer()
#    m = int(m)
#
#    # Round to even
#    if frac > 0.5 or frac == 0.5 and (m & 1):
#        m += 1
#        if m == 0x1000000:
#            m = 0x800000
#            x += 1
#
#            # re-check for overflow
#            if x > 128:
#                return math.copysign(Infinity, f)
#
#    if m == 0:
#        return math.copysign(0.0, f)
#
#    return math.ldexp(math.copysign(m/16777216.0, f), x)

#    # Another alternative.
#    m, x = math.frexp(abs(f))
#    if x > 128:
#        return math.copysign(Infinity, f)
#    if x < -149:
#        return math.copysign(0.0, f)
#    if x < -125:
#        e = 1<<(x+149)
#    else:
#        e = 16777216.0
#    # Special corner case with rounding near the maximum float (e.g. 3.4028236e38 gets rounded up, going out of range for a F32)
#    if m*e >= 16777215.5 and x == 128:
#        return math.copysign(Infinity, f)
#    return math.ldexp(math.copysign(math.floor(m*e+0.5)/e, f), x)


#    # Original old-fashioned strategy (watch out for the 16777215.5 bug above):
#
#    if math.isinf(f) or math.isnan(f) or f==0:
#        return f
#    s = math.copysign(1, f)
#    # This number may not be precise enough if Python had infinite precision, but it works for us.
#    if f < 0.0000000000000000000000000000000000000000000007006492321624086132496:
#        return math.copysign(0.0, s)
#    f = abs(f)
#
#
#    # TO DO: Check this boundary (this is 2^128)
#    if f >= 340282366920938463463374607431768211456.0:
#        return math.copysign(Infinity, s)
#
#    # TO DO: Check this boundary (2^-126; hopefully there's some overlap and the precision can be cut)
#    if f < 0.000000000000000000000000000000000000011754943508222875079687365372222456778186655567720875215087517062784172594547271728515625:
#        # Denormal range
#        f *= 713623846352979940529142984724747568191373312.0
#        e = 0.00000000000000000000000000000000000000000000140129846432481707092372958328991613128026194187651577175706828388979108268586060148663818836212158203125 # 2^-149
#    else:
#        e = 1.0
#        # This first loop is an optimization to get closer to the destination faster for very small numbers
#        while f < 1.0:
#            f *= 16777216.0
#            e *= 0.000000059604644775390625
#        # Go bit by bit
#        while f < 8388608.0:
#            f *= 2.0
#            e *= 0.5
#
#        #This first loop is an optimization to get closer to the destination faster for very big numbers
#        while f >= 140737488355328.0:
#            f *= 0.000000059604644775390625
#            e *= 16777216.0
#        # Go bit by bit
#        while f >= 16777216.0:
#            f *= 0.5
#            e *= 2.0
#
#    return math.copysign(math.floor(f+0.5)*e, s)

def S32(val):
    """Return a signed integer truncated to 32 bits (must deal with longs too)"""
    if -2147483648 <= val <= 2147483647:
        return int(val)
    val &= 0xFFFFFFFF
    if val > 2147483647:
        return int(val - 4294967296)
    return int(val)

def zstr(s):
    if not isinstance(s, unicode):
        raise ELSLInvalidType

    zi = s.find(u'\0')
    if zi < 0:
        return s
    return s.__class__(s[:zi])

def ff(x):
    """Force x to be a float"""
    if type(x) != float:
        x = float(x)
    return F32(x)

def fk(k):
    """Force k to be a key"""
    if type(k) != Key:
        k = Key(k)
    return k

def fs(s):
    """Force s to be a string"""
    if type(s) != unicode:
        s = unicode(s)
    return s

def q2f(q):
    if type(q[0]) == type(q[1]) == type(q[2]) == type(q[3]) == float:
        return q
    return Quaternion((ff(q[0]), ff(q[1]), ff(q[2]), ff(q[3])))

def v2f(v):
    if type(v[0]) == type(v[1]) == type(v[2]) == float:
        return v
    return Vector((ff(v[0]), ff(v[1]), ff(v[2])))

def f2s(val, DP=6):
    if math.isinf(val):
        return u'Infinity' if val > 0 else u'-Infinity'
    if math.isnan(val):
        return u'NaN'
    if lslcommon.LSO or val == 0.:
        return u'%.*f' % (DP, val) # deals with -0.0 too

    # Format according to Mono rules (7 decimals after the DP, found experimentally)
    s = u'%.*f' % (DP+7, val)

    if s[:DP+3] == u'-0.' + '0'*DP and s[DP+3] < u'5':
        return u'0.' + '0'*DP # underflown negatives return 0.0 except for -0.0 dealt with above

    # Separate the sign
    sgn = u'-' if s[0] == u'-' else u''
    if sgn: s = s[1:]

    # Look for position of first nonzero from the left
    i = 0
    while s[i] in u'0.':
        i += 1

    dot = s.index(u'.')

    # Find rounding point. It's either the 7th digit after the first significant one,
    # or the (DP+1)-th decimal after the period, whichever comes first.
    digits = 0
    while digits < 7:
        if i >= dot+1+DP:
            break
        if i == dot:
            i += 1
        i += 1
        digits += 1

    if s[i if i != dot else i+1] >= u'5':
        # Rounding - increment s[:i] storing result into new_s
        new_s = u''
        ci = i-1 # carry index
        while ci >= 0 and s[ci] == u'9':
            new_s = u'0' + new_s
            ci -= 1
            if ci == dot:
                ci -= 1 # skip over the dot
                new_s = u'.' + new_s # but add it to new_s
        if ci < 0:
            new_s = u'1' + new_s # 9...9 -> 10...0
        else:
            # increment s[ci] e.g. 43999 -> 44000
            new_s = s[:ci] + chr(ord(s[ci])+1) + new_s
    else:
        new_s = s[:i]

    if i <= dot:
        return sgn + new_s + u'0'*(dot-i) + u'.' + u'0'*DP
    return sgn + new_s + u'0'*(dot+1+DP-i)

def vr2s(v, DP=6):
    if type(v) == Vector:
        return u'<'+f2s(v[0],DP)+u', '+f2s(v[1],DP)+u', '+f2s(v[2],DP)+u'>'
    return u'<'+f2s(v[0],DP)+u', '+f2s(v[1],DP)+u', '+f2s(v[2],DP)+u', '+f2s(v[3],DP)+u'>'


def qnz(q):
    if all(x == 0. for x in q):
        return Quaternion((0.,0.,0.,1.))
    return q

def qnorm(q):
    q = qnz(q)
    mag2 = math.fsum((q[0]*q[0], q[1]*q[1], q[2]*q[2], q[3]*q[3]))
    # Threshold for renormalization
    eps_h = 1.0000021457672119140625 #float.fromhex('0x1.000024p0')
    eps_l = 0.99999797344207763671875 # float.fromhex('0x1.FFFFBCp-1')
    if mag2 >= eps_h or mag2 <= eps_l:
        # Renormalize
        mag2 = math.sqrt(mag2)
        return Quaternion((q[0]/mag2, q[1]/mag2, q[2]/mag2, q[3]/mag2))
    return q

def InternalTypecast(val, out, InList, f32):
    """Type cast val to out, following LSL rules.

    To avoid mutual recursion, it deals with everything except lists. That way
    it does not need to call InternalList2Strings which needs to call it.
    """
    tval = type(val)
    # The case tval == list is handled in typecast() below.
    if out == list:
        return [val]

    if tval == int: # integer
        val = S32(val)
        if out == int: return val
        if out == float: return F32(val, f32)
        if out == unicode: return unicode(val)
        raise ELSLTypeMismatch

    if tval == float:
        val = F32(val, f32)
        if out == int: return S32(int(val)) if val >= -2147483648.0 and val < 2147483648.0 else -2147483648
        if out == float: return val
        if out == unicode: return f2s(val, 6)
        raise ELSLTypeMismatch

    if tval == Vector:
        val = v2f(val)
        if out == Vector: return val
        if out == unicode: return vr2s(val, 6 if InList else 5)
        raise ELSLTypeMismatch
    if tval == Quaternion:
        val = q2f(val)
        if out == Quaternion: return val
        if out == unicode: return vr2s(val, 6 if InList else 5)
        raise ELSLTypeMismatch
    if tval == Key: # key
        if out == Key: return zstr(val)
        if out == unicode: return zstr(unicode(val))
        raise ELSLTypeMismatch

    if tval == unicode:
        val = zstr(val)
        if out == unicode: return val
        if out == Key: return Key(val)
        if out == float:
            # Clean up the string for Picky Python
            match = float_re.search(val)
            if match is None:
                return 0.0
            if match.group(1):
                ret =  F32(float.fromhex(match.group(0)), f32)
            else:
                if match.group(0).lower() == '-nan':
                    # (float)"-nan" produces NaN instead of Indet, even though
                    # (vector)"<-nan,0,0>" produces <Indet, 0., 0.>. Go figure.
                    ret = NaN
                else:
                    ret = F32(float(match.group(0)), f32)
            if not lslcommon.LSO and abs(ret) < 1.1754943508222875e-38:
                # Mono doesn't return denormals when using (float)"val"
                # (but it returns them when using (vector)"<val,...>")
                ret = 0.0
            return ret
        if out == int:
            match = int_re.search(val)
            if match is None:
                return 0
            val = match.group(0)
            if match.group(1):
                val = int(val, 0)
            else:
                val = int(val)
            if -4294967295 <= val <= 4294967295:
                return S32(val)
            return -1
        if out in (Vector, Quaternion):
            Z,dim = (ZERO_VECTOR,3) if out == Vector else (ZERO_ROTATION,4)
            ret = []
            if val[0:1] != u'<':
                return Z
            val = val[1:]
            for _ in range(dim):
                match = vfloat_re.search(val)
                if match is None:
                    return Z
                if match.group(1):
                    ret.append(F32(float.fromhex(match.group(0)), f32))
                else:
                    ret.append(F32(float(match.group(0)), f32))
                if len(ret) < dim:
                    i = match.end()
                    if val[i:i+1] != u',':
                        return Z
                    val = val[i+1:]
            return out(ret) # convert type

    # To avoid mutual recursion, this was moved:
    #if tval == list: # etc.

    raise ELSLInvalidType

def InternalList2Strings(val):
    """Convert a list of misc.items to a list of strings."""
    ret = []
    for elem in val:
        ret.append(InternalTypecast(elem, unicode, InList=True, f32=True))
    return ret

def InternalUTF8toString(s):
    # Note Mono and LSO behave differently here.
    # LSO *CAN* store invalid UTF-8.
    # For example, llEscapeURL(llUnescapeURL("%80%C3")) gives "%80%C3" in LSO.
    # (But llEscapeURL(llUnescapeURL("%80%00%C3")) still gives "%80")
    # We don't emulate it, we've built this with Unicode strings in mind.

    # decode(..., 'replace') replaces invalid chars with U+FFFD which is not
    # what LSL does (LSL replaces with '?'). Since U+FFFD must be preserved if
    # present, we need to write our own algorithm.

    # Problem: Aliases are not valid UTF-8 for LSL, and code points above
    # U+10FFFF are not supported. Both things complicate the alg a bit.

    ret = u''
    partialchar = b''
    pending = 0
    for c in s:
        o = ord(c)
        if partialchar:
            if 0x80 <= o < 0xC0 and (
                    partialchar[1:2]
                    or b'\xC2' <= partialchar < b'\xF4' and partialchar not in b'\xE0\xED\xF0'
                    or partialchar == b'\xE0' and o >= 0xA0
                    or partialchar == b'\xED' and o < 0xA0
                    or partialchar == b'\xF0' and o >= 0x90
                    or partialchar == b'\xF4' and o < 0x90
                    ):
                partialchar += c
                pending -= 1
                if pending == 0:
                    ret += partialchar.decode('utf8')
                    partialchar = b''
                c = c
                # NOTE: Without the above line, the following one hits a bug in
                # python-coverage. It IS executed but not detected.
                continue
            if lslcommon.LSO:
                raise ELSONotSupported(u"Byte strings not supported")
            ret += u'?' * len(partialchar)
            partialchar = b''
            # fall through to process current character
        if o >= 0xC2 and o <= 0xF4:
            partialchar = c
            pending = 1 if o < 0xE0 else 2 if o < 0xF0 else 3
        elif o >= 0x80:
            if lslcommon.LSO:
                raise ELSONotSupported(u"Byte strings not supported")
            ret += u'?'
        else:
            ret += c.decode('utf8')

    if partialchar:
        if lslcommon.LSO:
            raise ELSONotSupported(u"Byte strings not supported")
        ret += u'?' * len(partialchar)

    return zstr(ret)

# The code of llDeleteSubList and llDeleteSubString is identical except for the
# type check. Same for llGetSubString and llList2List. They are all joined into
# one single function.
def InternalGetDeleteSubSequence(val, start, end, isGet):
    assert isinteger(start)
    assert isinteger(end)
    L = len(val)

    # Python does much of the same thing as LSL here, which helps a lot
    if end == -1: end += L
    if (start+L if start < 0 else start) > (end+L if end < 0 else end):
        # Exclusion range - get/delete from end and start
        if isGet:
            return val[:end+1] + val[start:]
        return val[end+1:start]
    if isGet:
        return val[start:end+1]
    return val[:start] + val[end+1:]

def typecast(val, out, InList=False, f32=True):
    """Type cast an item. Calls InternalList2Strings for lists and
    defers the rest to InternalTypecast.
    """
    if type(val) == list:
        if out == list:
            return val # NOTE: We're not duplicating it here.
        if out == unicode:
            return u''.join(InternalList2Strings(val))
        raise ELSLTypeMismatch
    return InternalTypecast(val, out, InList, f32)

def neg(val):
    if type(val) in (int, float):
        if type(val) == int and val == -2147483648:
            return val
        return -val
    if isinstance(val, tuple):
        return val.__class__(-f for f in val)
    raise ELSLTypeMismatch

def add(a, b, f32=True):
    # defined for:
    #   scalar+scalar
    #   vector+vector
    #   rotation+rotation
    #   string+string
    #   (our extension:) key+string, string+key
    #   list+any
    #   any+list
    ta=type(a)
    tb=type(b)
    if ta in (int, float) and tb in (int, float):
        if ta == tb == int:
            return S32(a+b)
        return F32(ff(a)+ff(b), f32)

    if ta == tb in (list, unicode):
        return a + b
    # string + key, key + string are allowed here
    if ta in (unicode, Key) and tb in (unicode, Key) and not (ta == tb == Key):
        return a + b
    if ta == list:
        return a + [b]
    if tb == list:
        return [a] + b
    if ta == tb in (Vector, Quaternion):
        return F32(ta(ff(a[i])+ff(b[i]) for i in range(len(a))), f32)
    raise ELSLTypeMismatch

def sub(a, b, f32=True):
    # defined for:
    #   scalar+scalar
    #   vector+vector
    #   rotation+rotation
    ta=type(a)
    tb=type(b)
    if ta in (int, float) and tb in (int, float):
        if ta == tb == int:
            return S32(a-b)
        return F32(ff(a)-ff(b), f32)
    if ta == tb in (Vector, Quaternion):
        return F32(ta(ff(a[i])-ff(b[i]) for i in range(len(a))), f32)
    raise ELSLTypeMismatch

def mul(a, b, f32=True):
    # defined for:
    #   scalar*scalar
    #   scalar*vector
    #   vector*scalar
    #   vector*vector
    #   vector*rotation
    #   rotation*rotation
    ta = type(a)
    tb = type(b)
    # If either type is string, list, or key, error
    if ta in (unicode, list, Key) or tb in (unicode, list, Key):
        raise ELSLTypeMismatch
    # only int, float, vector, quaternion here
    if ta in (int, float):
        if tb in (int, float):
            if ta == tb == int:
                return S32(a*b)
            return F32(ff(a)*ff(b), f32)
        if tb != Vector:
            # scalar * quat is not defined
            raise ELSLTypeMismatch
        # scalar * vector
        a = ff(a)
        b = v2f(b)
        return Vector(F32((a*b[0], a*b[1], a*b[2]), f32))

    if ta == Quaternion:
        # quat * scalar and quat * vector are not defined
        if tb != Quaternion:
            raise ELSLTypeMismatch
        a = q2f(a)
        b = q2f(b)
        # quaternion product - product formula reversed
        return Quaternion(F32((a[0] * b[3] + a[3] * b[0] + a[2] * b[1] - a[1] * b[2],
                               a[1] * b[3] - a[2] * b[0] + a[3] * b[1] + a[0] * b[2],
                               a[2] * b[3] + a[1] * b[0] - a[0] * b[1] + a[3] * b[2],
                               a[3] * b[3] - a[0] * b[0] - a[1] * b[1] - a[2] * b[2]), f32))

    if ta != Vector:
        raise ELSLInvalidType # Should never happen at this point

    if tb in (int, float):
        a = v2f(a)
        b = ff(b)
        return Vector(F32((a[0]*b, a[1]*b, a[2]*b), f32))

    if tb == Vector:
        # scalar product
        a = v2f(a)
        b = v2f(b)
        return F32(math.fsum((a[0]*b[0], a[1]*b[1], a[2]*b[2])), f32)

    if tb != Quaternion:
        raise ELSLInvalidType # Should never happen at this point

    # vector * quaternion: perform conjugation
    #v = mul(Quaternion((-b[0], -b[1], -b[2], b[3])), mul(Quaternion((a[0], a[1], a[2], 0.0)), b, f32=False))
    #return Vector((v[0], v[1], v[2]))
    # this is more precise as it goes directly to the gist of it:
    a = v2f(a)
    b = q2f(b)
    return Vector(F32((
        math.fsum(( a[0]*(b[0]*b[0]-b[1]*b[1]-b[2]*b[2]+b[3]*b[3]),
                    a[1]*2*(b[0]*b[1]-b[2]*b[3]),
                    a[2]*2*(b[0]*b[2]+b[1]*b[3]))),
        math.fsum(( a[0]*2*(b[0]*b[1]+b[2]*b[3]),
                   -a[1]*(b[0]*b[0]-b[1]*b[1]+b[2]*b[2]-b[3]*b[3]), # notice minus sign
                    a[2]*2*(b[1]*b[2]-b[0]*b[3]))),
        math.fsum(( a[0]*2*(b[0]*b[2]-b[1]*b[3]),
                    a[1]*2*(b[1]*b[2]+b[0]*b[3]),
                   -a[2]*(b[0]*b[0]+b[1]*b[1]-b[2]*b[2]-b[3]*b[3]))) # notice minus sign
        ), f32))

def div(a, b, f32=True):
    # defined for:
    #   scalar/scalar
    #   vector/scalar
    #   vector/rotation
    #   rotation/rotation
    ta = type(a)
    tb = type(b)
    if tb in (int, float):
        if b == 0:
            raise ELSLMathError
        if ta in (int, float):
            if ta == int and tb == int:
                # special case
                if a == -2147483648 and b == -1:
                    return a # this could be handled by using S32 but it's probably faster this way
                if (a < 0) ^ (b < 0):
                    # signs differ - Python rounds towards -inf, we need rounding towards 0
                    return -(a//-b)
                return a//b
            ret = F32(ff(a)/ff(b), f32)
            if math.isnan(ret): # A NaN result gives a math error.
                raise ELSLMathError
            return ret
        if ta == Vector:
            a = v2f(a)
            b = ff(b)
            return Vector(F32((a[0]/b, a[1]/b, a[2]/b), f32))
    if tb == Quaternion: # division by a rotation is multiplication by the conjugate of the rotation
        # defer the remaining type checks to mul()
        return mul(a, Quaternion((-b[0],-b[1],-b[2],b[3])), f32)
    raise ELSLTypeMismatch

def mod(a, b, f32=True):
    # defined only for integers and vectors
    if type(a) == type(b) == int:
        if b == 0:
            raise ELSLMathError
        if a < 0:
            return int(-((-a) % abs(b)))
        return int(a % abs(b))
    if type(a) == type(b) == Vector:
        # cross product
        a = v2f(a)
        b = v2f(b)
        return Vector(F32((a[1]*b[2]-a[2]*b[1],
                           a[2]*b[0]-a[0]*b[2],
                           a[0]*b[1]-a[1]*b[0]), f32))

    raise ELSLTypeMismatch

def compare(a, b, Eq = True):
    """Calculate a == b when Eq is True, or a != b when not"""

    # Defined for all types as long as one of them can be auto-cast to the other
    ta = type(a)
    tb = type(b)
    if ta in (int, float) and tb in (int, float):
        # we trust that NaN == NaN is False
        if ta == tb == int:
            ret = a == b
        else:
            ret = ff(a) == ff(b)
        return int(ret) if Eq else 1-ret
    if ta in (unicode, Key) and tb in (unicode, Key):
        ret = 0 if a == b else 1 if a > b or not lslcommon.LSO else -1
        return int(not ret) if Eq else ret
    if ta == tb in (Vector, Quaternion):
        for ae,be in zip(a,b):
            if ae != be:
                return int(not Eq)
        return int(Eq)
    if ta == tb == list:
        ret = len(a) - len(b)
        return int(not ret) if Eq else ret
    raise ELSLTypeMismatch

def less(a, b):
    """Calculate a < b. The rest can be derived by swapping components and by
    negating: a > b is less(b,a); a <= b is 1-less(b,a); a >= b is 1-less(a,b).
    """
    if type(a) == type(b) == int:
        return int(a < b)
    if type(a) in (int, float) and type(b) in (int, float):
        return int(ff(a) < ff(b))
    raise ELSLTypeMismatch

def cond(x):
    """Test whether x evaluates to True in a condition (if, while, for, ...)"""
    tx = type(x)
    if tx == Key:
        if x == NULL_KEY or len(x) != 36:
            return False
        return bool(key_re.search(x))
    if tx == Vector:
        return bool(compare(x, ZERO_VECTOR, Eq=False))
    if tx == Quaternion:
        return bool(compare(x, ZERO_ROTATION, Eq=False))
    if lslcommon.LSO and tx == list:
        # SVC-689: lists of 1 element count as false
        return len(x) > 1
    return bool(x) # works fine for int, float, string, list

def isinteger(x):
    return type(x) == int

def isfloat(x):
    return type(x) == float

def isvector(x):
    return type(x) == Vector and len(x) == 3 and type(x[0]) == type(x[1]) == type(x[2]) == float

def isrotation(x):
    return type(x) == Quaternion and len(x) == 4 and type(x[0]) == type(x[1]) == type(x[2]) == type(x[3]) == float

def isstring(x):
    return type(x) == unicode

def iskey(x):
    return type(x) == Key

def islist(x):
    return type(x) == list

def reduce(t):
    t = F32(t)
    if not t.is_integer():
        return t # Accurate-ish until big numbers come into play
    return int(t * 18446744073709551616) % 115904311329233965478 / 18446744073709551616.

#
# LSL-compatible computation functions
#

def llAbs(i):
    assert isinteger(i)
    if i != -2147483648:
        return abs(i)
    if lslcommon.LSO:
        return i
    # Mono raises an OverflowException in this case.
    raise ELSLCantCompute

def llAcos(f):
    assert isfloat(f)
    try:
        return F32(math.acos(f))
    except ValueError:
        return NaN

def llAngleBetween(r1, r2):
    assert isrotation(r1)
    assert isrotation(r2)
    return llRot2Angle(div(qnz(r1), qnz(r2), f32=False))

def llAsin(f):
    assert isfloat(f)
    try:
        return F32(math.asin(f))
    except ValueError:
        return NaN

def llAtan2(y, x):
    assert isfloat(y)
    assert isfloat(x)
    if math.isnan(x) and math.isnan(y):
        if math.copysign(1, x) == -1 and math.copysign(1, y) == -1:
            return -NaN
        return NaN
    elif math.isnan(x):
        return x
    elif math.isnan(y):
        return y
    return F32(math.atan2(y, x))

def llAxes2Rot(fwd, left, up):
    assert isvector(fwd)
    assert isvector(left)
    assert isvector(up)

    # One of the hardest.

    t = math.fsum((fwd[0], left[1], up[2]))
    if t > 0.: # no danger of division by zero or negative roots
        r = math.sqrt(1. + t)
        s = 0.5/r

        # For the case of ix+jy+kz > 0, it can return an unnormalized quaternion
        return Quaternion(F32((s*(left[2]-up[1]), s*(up[0]-fwd[2]), s*(fwd[1]-left[0]), r*0.5)))

    # Find a positive combo. LSL normalizes the result in these cases only, so we do the same.

    if left[1] <= fwd[0] >= up[2]: # is fwd[0] the greatest?
        r = math.sqrt(1. + fwd[0] - left[1] - up[2])
        s = 0.5/r
        q = (r*0.5, s*(fwd[1]+left[0]), s*(up[0]+fwd[2]), s*(left[2]-up[1]))

    elif fwd[0] <= left[1] >= up[2]: # is left[1] the greatest?
        r = math.sqrt(1. - fwd[0] + left[1] - up[2])
        s = 0.5/r
        q = (s*(fwd[1]+left[0]), r*0.5, s*(left[2]+up[1]), s*(up[0]-fwd[2]))

    else:
        # Only one case remaining: up[2] is the greatest
        r = math.sqrt(1. - fwd[0] - left[1] + up[2])
        s = 0.5/r
        q = (s*(up[0]+fwd[2]), s*(left[2]+up[1]), r*0.5, s*(fwd[1]-left[0]))

    # Normalize
    q = qnz(q)
    mag = math.sqrt(math.fsum((q[0]*q[0], q[1]*q[1], q[2]*q[2], q[3]*q[3])))
    return Quaternion(F32((q[0]/mag, q[1]/mag, q[2]/mag, q[3]/mag)))


def llAxisAngle2Rot(axis, angle):
    assert isvector(axis)
    assert isfloat(angle)
    axis = llVecNorm(axis, f32=False)
    if axis == ZERO_VECTOR:
        angle = 0.
    c = math.cos(angle*0.5)
    s = math.sin(angle*0.5)
    return Quaternion(F32((axis[0]*s, axis[1]*s, axis[2]*s, c)))

# NOTE: This one does not always return the same value in LSL. When it isn't
# deterministic, it raises ELSLCantCompute.
def llBase64ToInteger(s):
    assert isstring(s)
    if len(s) > 8:
        return 0
    s = b64_re.search(s).group()
    i = len(s)
    s = b64decode(s + u'='*(-i & 3))
    if len(s) < 3:
        # not computable deterministically
        raise ELSLCantCompute
    s = (s + b'\0')[:4]
    i = ord(s[0]) if s[0] < b'\x80' else ord(s[0])-256
    return (i<<24)+(ord(s[1])<<16)+(ord(s[2])<<8)+ord(s[3])

b64tos_re = re.compile(
    b'('
      # Those pass through and are caught by InternalUTF8toString:
      b'\x00$' # NUL at last position (zstr removes it)
      b'|[\x09\x0A\x0F\x1F-\x7F\xFE\xFF]|[\xC2-\xDF][\x80-\xBF]'
      b'|(?:\xE0[\xA0-\xBF]|[\xE1-\xEF][\x80-\xBF])[\x80-\xBF]'
      b'|(?:\xF0[\x90-\xBF]|[\xF1-\xF7][\x80-\xBF])[\x80-\xBF]{2}'
      b'|(?:\xF8[\x88-\xBF]|[\xF9-\xFB][\x80-\xBF])[\x80-\xBF]{3}'
      b'|(?:\xFC[\x84-\xBF]|\xFD[\x80-\xBF])[\x80-\xBF]{4}'
    b')|('
      # Those are caught here and substituted by a single "?"
      # (greediness is important here):
      b'[\x00-\x1F\x80-\xBF]'
      b'|[\xC0-\xDF][\x80-\xBF]?'
      b'|[\xE0-\xEF][\x80-\xBF]{0,2}'
      b'|[\xF0-\xF7][\x80-\xBF]{0,3}'
      b'|[\xF8-\xFB][\x80-\xBF]{0,4}'
      b'|[\xFC-\xFD][\x80-\xBF]{0,5}'
    b')|(.)' # should never be reached
)

def llBase64ToString(s):
    assert isstring(s)
    s = b64_re.search(s).group(0)

    # llUnescapeURL and llBase64ToString behave differently.
    # llBase64ToString does a first check on the UTF-8 before the standard
    # conversion, unlike llUnescapeURL. That makes it have a much more similar
    # behaviour to LSO's than llUnescapeURL does. But LL being LL, the check
    # is, of course, flawed, and some illegal sequences pass as good (but in
    # Mono they are fortunately stopped on the conversion to UTF-8 instead).
    # The check that llBase64ToString does has the quirk that the invalid
    # sequences that it catches are treated as 1 single bad character instead
    # of as many as the sequence has. The latter is what normal conversion to
    # UTF-8 does. This causes inconsistencies in the number of ?'s returned.

    # In llBase64ToString, trailing NUL is stripped, and embedded NULs are
    # converted to "?". In addition, characters in range 00-1F are also
    # converted to "?" except for \x09, \x0A, \x0F, \x1F.

    byteseq = bytearray(b64decode(s + u'=' * (-len(s) & 3)))

    pos = 0
    match = b64tos_re.search(byteseq, pos)
    while match is not None:
        assert match.group(3) is None, 'Fail in b64tos_re: ' + match.group(3)
        L = len(match.group(2) or '')
        if L:
            byteseq[pos:pos+L] = b'?'
            pos = match.end(2) - L + 1
        else:
            pos = match.end(1)

        match = b64tos_re.search(byteseq, pos)

    return InternalUTF8toString(bytes(byteseq))

def llCSV2List(s):
    assert isstring(s)

    bracketlevel = 0
    lastwascomma = True     # first space is eaten!!!
    lastidx = 0
    i = 0
    ret = []
    for c in s:
        if bracketlevel:
            # ignore ',', focus on nesting level
            if c == u'<':
                bracketlevel += 1
            elif c == u'>':
                bracketlevel -= 1
        elif lastwascomma and c == u' ': # eat space after comma
            lastwascomma = False
            lastidx = i+1
        else:
            lastwascomma = False
            if c == u',':
                lastwascomma = True
                ret.append(s[lastidx:i])
                lastidx = i+1
            elif c == u'<':
                bracketlevel += 1
        i += 1
    ret.append(s[lastidx:i])
    return ret

def llCeil(f):
    assert isfloat(f)
    if math.isnan(f) or math.isinf(f) or f >= 2147483648.0 or f < -2147483648.0:
        return -2147483648
    return int(math.ceil(f))

def llCos(f):
    assert isfloat(f)
    if math.isinf(f):
        return Indet
    if -9223372036854775808.0 < f < 9223372036854775808.0:
        return F32(math.cos(reduce(f)))
    return f

def llDeleteSubList(lst, start, end):
    # This acts as llList2List if there's wraparound
    assert islist(lst)
    return InternalGetDeleteSubSequence(lst, start, end, isGet=False)

def llDeleteSubString(s, start, end):
    # This acts as llGetSubString if there's wraparound
    assert isstring(s)
    return InternalGetDeleteSubSequence(s, start, end, isGet=False)

def llDumpList2String(lst, sep):
    assert islist(lst)
    assert isstring(sep)
    return sep.join(InternalList2Strings(lst))

def llEscapeURL(s):
    assert isstring(s)
    s = s.encode('utf8') # get bytes
    ret = u''
    for c in s:
        if b'A' <= c <= b'Z' or b'a' <= c <= b'z' or b'0' <= c <= b'9':
            ret += c.encode('utf8')
        else:
            ret += u'%%%02X' % ord(c)
    return ret

def llEuler2Rot(v):
    assert isvector(v)
    c0 = math.cos(v[0]*0.5)
    s0 = math.sin(v[0]*0.5)
    c1 = math.cos(v[1]*0.5)
    s1 = math.sin(v[1]*0.5)
    c2 = math.cos(v[2]*0.5)
    s2 = math.sin(v[2]*0.5)

    r = F32((s0 * c1 * c2 + c0 * s1 * s2,
             c0 * s1 * c2 - s0 * c1 * s2,
             c0 * c1 * s2 + s0 * s1 * c2,
             c0 * c1 * c2 - s0 * s1 * s2))

    # Fix the sign
    c0 = math.cos(v[0])
    s0 = math.sin(v[0])
    c1 = math.cos(v[1])
    s1 = math.sin(v[1])
    c2 = math.cos(v[2])
    s2 = math.sin(v[2])
    d1 = c1*c2
    d2 = c0*c2 - s0*s1*s2
    d3 = c0*c1
    if d1 + d2 + d3 > 0:
        return Quaternion(-f for f in r) if r[3] < 0 else Quaternion(r)
    i = 0
    if d2 > d1:
        i = 1
    if d1 < d3 > d2:
        i = 2
    return Quaternion(-f for f in r) if r[i] < 0 else Quaternion(r)

def llFabs(f):
    assert isfloat(f)
    return math.fabs(f)

def llFloor(f):
    assert isfloat(f)
    if math.isnan(f) or math.isinf(f) or f >= 2147483648.0 or f < -2147483648.0:
        return -2147483648
    return int(math.floor(f))

def llFrand(lim):
    assert isfloat(lim)
    if math.isinf(lim):
        return 0.
    if abs(lim) < float.fromhex('0x1p-126'):
        return -0. if lim < 0 else 0.
    if math.isnan(lim):
        return lim

    if lslcommon.IsCalc:
        import random
        val = random.random() * lim
        # Truncate, rather than rounding
        m, e = math.frexp(val)
        val = F32(math.ldexp(int(m * 16777216.) * .000000059604644775390625, e))
        if val == lim:
            # this should never happen
            # (it can happen on denormals, but these cause output of 0.0)
            val = 0. # pragma: no cover
        return val

    # Can't give a concrete value
    raise ELSLCantCompute

def llGenerateKey():
    if lslcommon.IsCalc:
        import time
        import random

        s = hashlib.md5((u'%.17g %f %f' % (time.time(), random.random(),
                                           random.random())).encode('utf8')
                       ).hexdigest()
        return Key(s[:8] + '-' + s[8:12] + '-' + s[12:16] + '-' + s[16:20]
                   + '-' + s[20:32])

    # Can't give a concrete value
    raise ELSLCantCompute

def llGetListEntryType(lst, pos):
    assert islist(lst)
    assert isinteger(pos)
    try:
        return Types[type(lst[pos])]
    except IndexError:
        return 0 # TYPE_INVALID
    except KeyError:
        raise ELSLInvalidType

def llGetListLength(lst):
    assert islist(lst)
    return len(lst)

def llGetSubString(s, start, end):
    assert isstring(s)
    return InternalGetDeleteSubSequence(s, start, end, isGet=True)

def llInsertString(s, pos, src):
    assert isstring(s)
    assert isinteger(pos)
    assert isstring(src)
    if pos < 0: pos = 0 # llInsertString does not support negative indices
    return s[:pos] + src + s[pos:]

def llIntegerToBase64(x):
    assert isinteger(x)
    return b64encode(chr((x>>24)&255) + chr((x>>16)&255) + chr((x>>8)&255) + chr(x&255)).decode('utf8')

def llList2CSV(lst):
    assert islist(lst)
    ret = []
    for elem in lst:
        # This always uses LSO rules for float to string.
        if type(elem) == float:
            if math.isnan(elem) and math.copysign(1.0, elem) < 0:
                ret.append(u'-nan')
            else:
                ret.append(u'%.6f' % elem)
        elif type(elem) in (Vector, Quaternion):
            ret.append(u'<' + llList2CSV(list(elem)) + u'>')
        else:
            ret.append(InternalTypecast(elem, unicode, InList=True, f32=True))
    ret = u', '.join(ret)
    return ret

def llList2Float(lst, pos):
    assert islist(lst)
    assert isinteger(pos)
    try:
        elem = lst[pos]
        if type(elem) == float:
            return elem
        if type(elem) in (int, unicode):
            return InternalTypecast(elem, float, InList=True, f32=True)
    except IndexError:
        pass
    return 0.0

def llList2Integer(lst, pos):
    assert islist(lst)
    assert isinteger(pos)
    try:
        elem = lst[pos]
        if type(elem) == int:
            return elem
        if type(elem) in (float, unicode):
            return InternalTypecast(elem, int, InList=True, f32=True)
        return 0
    except IndexError:
        return 0

def llList2Key(lst, pos):
    assert islist(lst)
    assert isinteger(pos)
    try:
        elem = lst[pos]
        if type(elem) == Key:
            return elem
        if type(elem) == unicode:
            return Key(elem)
    except IndexError:
        pass
    if lslcommon.LSO:
        return Key(NULL_KEY)
    return Key(u'')

def llList2List(lst, start, end):
    assert islist(lst)
    assert isinteger(start)
    assert isinteger(end)
    return InternalGetDeleteSubSequence(lst, start, end, isGet=True)

def llList2ListStrided(lst, start, end, stride):
    assert islist(lst)
    assert isinteger(start)
    assert isinteger(end)
    assert isinteger(stride)
    stride = abs(stride) if stride != 0 else 1
    L = len(lst)
    if start < 0: start += L
    if end < 0: end += L
    if start > end:
        start = 0
        end = L-1
    # start is rounded up to ceil(start/stride)*stride
    start = ((start+stride-1)/stride)*stride
    # end is rounded down to floor(start/stride)*stride
    end = (end/stride)*stride

    return lst[start:end+1:stride]

def llList2Rot(lst, pos):
    assert islist(lst)
    assert isinteger(pos)
    try:
        elem = lst[pos]
        if type(elem) == Quaternion:
            # The list should not contain integer quaternion components, but
            # we don't control that here. Instead we return the integer-less
            # quaternion when asked.
            return q2f(elem)
    except IndexError:
        pass
    return ZERO_ROTATION

def llList2String(lst, pos):
    assert islist(lst)
    assert isinteger(pos)
    try:
        return InternalTypecast(lst[pos], unicode, InList=True, f32=True)
    except IndexError:
        pass
    return u''

def llList2Vector(lst, pos):
    assert islist(lst)
    assert isinteger(pos)
    try:
        elem = lst[pos]
        if type(elem) == Vector:
            # The list should not contain integer vector components, but
            # we don't control that here. Instead we return the integer-less
            # vector when asked.
            return v2f(elem)
    except IndexError:
        pass
    return ZERO_VECTOR

def llListFindList(lst, elems):
    assert islist(lst)
    assert islist(elems)
    # NaN is found in floats, but not in vectors
    L1 = len(lst)
    L2 = len(elems)
    if L2 > L1:
        return -1 # can't find a sublist longer than the original list
    if L2 == 0:
        # empty list is always found at position 0 in Mono,
        # and in LSO if the first list isn't empty
        return -1 if lslcommon.LSO and L1 == 0 else 0
    for i in xrange(L1-L2+1):
        for j in xrange(L2):
            e1 = lst[i+j]
            e2 = elems[j]
            if type(e1) == type(e2) == float:
                if e1 == e2:
                    continue
                # Exceptionally, NaN equals NaN
                if math.isnan(e1) and math.isnan(e2):
                    continue
                # Mismatch
                break
            elif type(e1) == type(e2) in (Vector, Quaternion):
                # Act as if the list's vector/quat was all floats, even if not
                if type(e1) == Vector:
                    e1 = v2f(e1)
                    e2 = v2f(e2)
                else:
                    e1 = q2f(e1)
                    e2 = q2f(e2)
                # Unfortunately, Python fails to consider (NaN,) != (NaN,) sometimes
                # so we need to implement our own test
                for e1e,e2e in zip(e1,e2):
                    if e1e != e2e: # NaNs are considered different to themselves here as normal
                        # Mismatch in vector/quaternion sub-element
                        break
                else:
                    # No mismatch in any sub-element, try next list element
                    continue
                break # discrepancy found
            elif type(e1) != type(e2) or e1 != e2:
                break # mismatch
        else:
            # no mismatch
            return i
    return -1

def llListInsertList(lst, elems, pos):
    assert islist(lst)
    assert islist(elems)
    assert isinteger(pos)
    # Unlike llInsertString, this function does support negative indices.
    return lst[:pos] + elems + lst[pos:]

# not implemented as it does not give the same output for the same input
#def llListRandomize(x):

def llListReplaceList(lst, elems, start, end):
    assert islist(lst)
    assert islist(elems)
    assert isinteger(start)
    assert isinteger(end)
    L = len(lst)
    if start < -L:
        # llListReplaceList([0,1,2,3],[5],-5,-5) should return [0,1,2,3]
        # llListReplaceList([0,1,2,3],[5],-5,-4) should return [1,2,3]
        # llListReplaceList([0,1,2,3],[5],-5,-7) should return []
        elems = []
    if (start + L if start < 0 else start) > (end + L if end < 0 else end):
        # Exclusion range. Appends elems at 'start' i.e. at end :)
        if end == -1: end += L
        return lst[end+1:start] + elems
    if end == -1: end += L
    return lst[:start] + elems + lst[end+1:]

def llListSort(lst, stride, asc):
    assert islist(lst)
    assert isinteger(stride)
    assert isinteger(asc)
    lst = lst[:] # make a copy
    L = len(lst)
    broken = u'\ufb1a' > u'\U0001d41a' # that happens on Windows
    if stride < 1: stride = 1
    if L % stride:
        return lst
    for i in xrange(0, L-stride, stride):
        # Optimized by caching the element in the outer loop AND after swapping.
        a = lst[i]
        ta = type(a)
        if ta == Vector:
            a = v2f(a) # list should contain vectors made only of floats
            a = a[0]*a[0] + a[1]*a[1] + a[2]*a[2]
        if broken and ta in (unicode, Key):
            # Note this breaks type consistency between a and ta!
            # It should be OK because only equal types are compared.
            a = a.encode('utf-32-be') # pragma: no cover
        for j in xrange(i+stride, L, stride):
            b = lst[j]
            tb = type(b)
            gt = False
            if ta == tb:
                if tb == Vector:
                    b = v2f(b)
                    gt = not (a <= b[0]*b[0] + b[1]*b[1] + b[2]*b[2])
                    # (note NaNs compare as > thus the reversed condition!)
                elif tb != Quaternion:
                    if broken and tb in (unicode, Key):
                        b = b.encode('utf-32-be') # pragma: no cover
                    gt = not (a <= b) # float, integer, string, key all take this branch
                    # (note NaNs compare as > thus the reversed condition!)
            if gt ^ (asc != 1):
                # swap
                lst[i:i+stride],lst[j:j+stride] = lst[j:j+stride],lst[i:i+stride]
                # Re-cache
                a = lst[i]
                ta = type(a)
                if ta == Vector:
                    a = v2f(a)
                    a = a[0]*a[0] + a[1]*a[1] + a[2]*a[2]
                if broken and ta in (unicode, Key):
                    a = a.encode('utf-32-be') # pragma: no cover
    return lst

def llListStatistics(op, lst):
    assert isinteger(op)
    assert islist(lst)

    nums = []
    # Extract numbers in reverse order. LIST_STAT_MEDIAN uses that.
    for elem in lst:
        if type(elem) in (int, float):
            nums.insert(0, float(elem))

    if nums == []:
        return 0.0

    if op == 8: # LIST_STAT_NUM_COUNT
        return float(len(nums))

    if op in (0, 1, 2) : # LIST_STAT_RANGE, LIST_STAT_MIN, LIST_STAT_MAX
        min = None
        for elem in nums:
            if min is None:
                min = max = elem
            else:
                if elem < min:
                    min = elem
                if elem > max:
                    max = elem
        return F32(max - min if op == 0 else min if op == 1 else max)

    if op == 4: # LIST_STAT_MEDIAN requires special treatment
        # The function behaves very strangely with NaNs. This seems to reproduce it:

        # llListSort seems to do the right thing with NaNs as needed by the median.
        nums = llListSort(nums, 1, 1)
        L = len(nums)
        if L & 1:
            return F32(nums[L>>1])
        return F32((nums[(L>>1)-1] + nums[L>>1])*0.5)

    if op in (3, 5, 6, 7): # LIST_STAT_MEAN, STD_DEV, SUM, SUM_SQUARES
        sum = 0.
        sumsq = 0.
        mean = 0.
        N = 0.
        M2 = 0.
        for elem in nums:
            N += 1.
            sum += elem
            sumsq += elem*elem
            delta = elem - mean
            mean += delta/N
            M2 += delta*(elem-mean)

        if op == 5: # LIST_STAT_STD_DEV
            return 0. if N == 1. else F32(math.sqrt(M2/(N-1.)))
        if op == 6: # LIST_STAT_SUM
            return F32(sum)
        if op == 7: # LIST_STAT_SUM_SQUARES
            return F32(sumsq)
        return F32(mean)

    if op == 9: # LIST_STAT_GEOMETRIC_MEAN
        N = 0.
        GMlog = 0.
        for elem in nums:
            if elem <= 0.:
                return 0.
            N += 1.
            delta = math.log(elem) - GMlog
            GMlog += delta/N
        return F32(math.exp(GMlog))

    return 0.0

def llLog(f):
    assert isfloat(f)
    if math.isinf(f) and f < 0 or math.isnan(f) or f <= 0.0:
        return 0.0
    return F32(math.log(f))

def llLog10(f):
    assert isfloat(f)
    if math.isinf(f) and f < 0 or math.isnan(f) or f <= 0.0:
        return 0.0
    return F32(math.log10(f))

def llMD5String(s, salt):
    assert isstring(s)
    assert isinteger(salt)
    return hashlib.md5(zstr(s).encode('utf8') + b':' + bytes(salt)).hexdigest().decode('utf8')

def llModPow(base, exp, mod):
    assert isinteger(base)
    assert isinteger(exp)
    assert isinteger(mod)
    if not lslcommon.IsCalc:
        # This function has a delay, therefore it's not safe to compute it
        # unless in calculator mode.
        raise ELSLCantCompute
    # With some luck, this works fully with native ints on 64 bit machines.
    if mod in (0, 1):
        return 0
    if exp == 0:
        return 1
    # Convert all numbers to unsigned
    if base < 0:
        base += 4294967296
    if exp < 0:
        exp += 4294967296
    if mod < 0:
        mod += 4294967296
    prod = base
    ret = 1
    while True:
        if exp & 1:
            ret = ((ret * prod) & 0xFFFFFFFF) % mod
        exp = exp >> 1
        if exp == 0:
            break
        prod = ((prod * prod) & 0xFFFFFFFF) % mod

    return S32(ret)

def llParseString2List(s, exc, inc, KeepNulls=False):
    assert isstring(s)
    assert islist(exc)
    assert islist(inc)
    if s == u'' and KeepNulls:
        return [s]
    exc = exc[:8]
    inc = inc[:8]
    regex = u''
    for i in exc:
        if i != u'':
            regex += u'|' + re.escape(i)
    for i in inc:
        if i != u'':
            regex += u'|' + re.escape(i)
    if regex == u'':
        split = [s]
    else:
        regex = u'(' + regex[1:] + u')'
        split = re.split(regex, s)
    return [i for i in split if (KeepNulls or i != u'') and i not in exc]

def llParseStringKeepNulls(s, exc, inc):
    return llParseString2List(s, exc, inc, KeepNulls=True)

def llPow(base, exp):
    assert isfloat(base)
    assert isfloat(exp)
    try:
        # Python corner cases and LSL corner cases differ

        # Python matches these two, but we don't want to get trapped by our own checks.
        if math.isnan(base) or math.isnan(exp):
            return NaN
        if exp == 0.0:
            return 1.0

        if base == 0.0: # Python gives exception on these, LSL returns stuff
            if math.isinf(exp) and exp < 0:
                return Infinity # llPow(0.0, -inf) = inf

            if exp < 0.0:
                # Negative finite exponent cases
                if math.copysign(1, base) < 0 and exp.is_integer() and not (exp/2.).is_integer():
                    return -Infinity # llPow(-0.0, -odd_integer) = -inf
                return Infinity

        elif abs(base) == 1.0 and math.isinf(exp):
            return NaN # Python says 1.0

        f = F32(math.pow(base, exp))
        return 0.0 if f == 0.0 else f # don't return -0.0
    except ValueError: # should happen only with negative base and noninteger exponent
        return Indet

def llRot2Angle(r):
    assert isrotation(r)
    # Used by llAngleBetween.
    # Version based on research by Moon Metty, Miranda Umino and Strife Onizuka
    return F32(2.*math.atan2(math.sqrt(math.fsum((r[0]*r[0], r[1]*r[1], r[2]*r[2]))), abs(r[3])));

def llRot2Axis(r):
    assert isrotation(r)
    if r[3] < 0:
        return llVecNorm(Vector((-r[0], -r[1], -r[2])))
    return llVecNorm(Vector((r[0], r[1], r[2])))

def llRot2Euler(r):
    assert isrotation(r)

    # Another one of the hardest. The formula for Z angle in the
    # singularity case was inspired by the viewer code.
    r = qnorm(r)
    y = 2*(r[0]*r[2] + r[1]*r[3])

    # Check gimbal lock condition
    if abs(y) > 0.99999:
        return Vector(F32((0.,
                           math.asin(1. if y > 1. else y),
                           math.atan2(r[2]*r[3]+r[0]*r[1],
                                      .5-(r[0]*r[0]+r[2]*r[2]))
                        )))

    qy2 = r[1]*r[1]
    return Vector(F32((
        math.atan2(r[0]*r[3]-r[1]*r[2], .5-(r[0]*r[0]+qy2)),
        math.asin(y),
        math.atan2(r[2]*r[3]-r[0]*r[1], .5-(r[2]*r[2]+qy2))
        )))

def llRot2Fwd(r):
    assert isrotation(r)
    v = Vector((1., 0., 0.))
    return llVecNorm(mul(v, qnz(r), f32=False))

def llRot2Left(r):
    assert isrotation(r)
    v = Vector((0., 1., 0.))
    return llVecNorm(mul(v, qnz(r), f32=False))

def llRot2Up(r):
    assert isrotation(r)
    v = Vector((0., 0., 1.))
    return llVecNorm(mul(v, qnz(r), f32=False))

def llRotBetween(v1, v2):
    assert isvector(v1)
    assert isvector(v2)

    aabb = math.sqrt(mul(v1, v1, f32=False) * mul(v2, v2, f32=False)) # product of the squared lengths of the arguments
    if aabb == 0.:
        return ZERO_ROTATION # the arguments are too small, return zero rotation
    ab = mul(v1, v2, f32=False) / aabb # normalized dotproduct of the arguments (cosine)
    c = Vector(((v1[1] * v2[2] - v1[2] * v2[1]) / aabb, # normalized crossproduct of the arguments
                (v1[2] * v2[0] - v1[0] * v2[2]) / aabb,
                (v1[0] * v2[1] - v1[1] * v2[0]) / aabb))
    cc = mul(c, c, f32=False) # squared length of the normalized crossproduct (sine)
    if cc != 0.: # test if the arguments are (anti)parallel
        if ab > -0.7071067811865476: # test if the angle is smaller than 3/4 PI
            s = 1. + ab # use the cosine to adjust the s-element
        else:
            s = cc / (1. + math.sqrt(1. - cc)); # use the sine to adjust the s-element
        m = math.sqrt(cc + s * s) # the magnitude of the quaternion
        return Quaternion(F32((c[0] / m, c[1] / m, c[2] / m, s / m))) # return the normalized quaternion
    if ab > 0.: # test if the angle is smaller than PI/2
        return ZERO_ROTATION # the arguments are parallel
    m = math.sqrt(v1[0] * v1[0] + v1[1] * v1[1]) # the length of one argument projected on the XY-plane
    if m != 0.:
        return Quaternion(F32((v1[1] / m, -v1[0] / m, 0., 0.))) # return rotation with the axis in the XY-plane
    return Quaternion((0., 0., 1., 0.)) # rotate around the Z-axis

    # Algorithm by Moon Metty (for reference)
    dot = mul(v1, v2, f32=False)
    cross = mod(v1, v2, f32=False)
    csq = mul(cross, cross, f32=False)

    ddc2 = dot*dot + csq

    DenormalStart = float.fromhex('0x1p-149')
    if ddc2 >= DenormalStart:
        if csq >= DenormalStart:
            s = math.sqrt(ddc2) + dot;
            m = math.sqrt(csq + s*s);
            return Quaternion(F32((cross[0]/m, cross[1]/m, cross[2]/m, s/m)))

        # Deal with degenerate cases here
        if dot > 0:
            return ZERO_ROTATION
        m = math.sqrt(v1[0]*v1[0] + v1[1]*v1[1])
        if m >= DenormalStart:
            return Quaternion(F32((v1[1]/m, -v1[0]/m, 0., 0.)))
        return Quaternion((1., 0., 0., 0.))
    return ZERO_ROTATION

def llRound(f):
    assert isfloat(f)
    if math.isnan(f) or math.isinf(f) or f >= 2147483647.5 or f < -2147483648.0:
        return -2147483648
    return int(math.floor(F32(f+0.5)))

def llSHA1String(s):
    assert isstring(s)
    return hashlib.sha1(s.encode('utf8')).hexdigest().decode('utf8')

def llSin(f):
    assert isfloat(f)
    if math.isinf(f):
        return Indet
    if -9223372036854775808.0 < f < 9223372036854775808.0:
        return F32(math.sin(reduce(f)))
    return f

def llSqrt(f):
    assert isfloat(f)
    if f < 0.0:
        return Indet
    # LSL and Python both produce -0.0 when the input is -0.0.
    return F32(math.sqrt(f))

def llStringLength(s):
    assert isstring(s)
    return len(s)

def llStringToBase64(s):
    assert isstring(s)
    return b64encode(s.encode('utf8')).decode('utf8')

def llStringTrim(s, mode):
    assert isstring(s)
    assert isinteger(mode)
    head = 0
    length = len(s)
    tail = length-1
    if mode & 1: # STRING_TRIM_HEAD
        while head < length and s[head] in u'\x09\x0a\x0b\x0c\x0d\x20':
            head += 1
    if mode & 2: # STRING_TRIM_TAIL
        while tail >= head and s[tail] in u'\x09\x0a\x0b\x0c\x0d\x20':
            tail -= 1
    return s[head:tail+1]

def llSubStringIndex(s, pattern):
    assert isstring(s)
    assert isstring(pattern)
    return s.find(pattern)

def llTan(f):
    assert isfloat(f)
    if math.isinf(f):
        return Indet
    if -9223372036854775808.0 < f < 9223372036854775808.0:
        return F32(math.tan(reduce(f)))
    return f

def llToLower(s):
    assert isstring(s)
    if lslcommon.LSO:
        return zstr(re.sub(u'[A-Z]', lambda x: x.group().lower(), s))
    return zstr(s.lower())

def llToUpper(s):
    assert isstring(s)
    if lslcommon.LSO:
        return zstr(re.sub(u'[a-z]', lambda x: x.group().upper(), s))
    return zstr(s.upper())

def llUnescapeURL(s):
    assert isstring(s)
    ret = b''
    L = len(s)
    i = 0
    while i < L:
        c = s[i]
        i += 1
        if c != u'%':
            ret += c.encode('utf8')
            continue
        if i >= L:
            break
        c = s[i] # First digit
        i += 1
        if i >= L:
            break
        v = 0
        if u'0' <= c <= u'9' or u'A' <= c <= u'F' or u'a' <= c <= u'f':
            v = int(c, 16)<<4
        c = s[i] # Second digit
        if c == u'%':
            ret += chr(v)
            i += 1
            continue
        i += 1
        if u'0' <= c <= u'9' or u'A' <= c <= u'F' or u'a' <= c <= u'f':
            v += int(c, 16)
        ret += chr(v)
    return InternalUTF8toString(ret)

def llVecDist(v1, v2):
    assert isvector(v1)
    assert isvector(v2)
    # For improved accuracy, do the intermediate calcs as doubles
    vx = v1[0]-v2[0]
    vy = v1[1]-v2[1]
    vz = v1[2]-v2[2]
    return F32(math.sqrt(math.fsum((vx*vx, vy*vy, vz*vz))))

def llVecMag(v):
    assert isvector(v)
    return F32(math.sqrt(math.fsum((v[0]*v[0], v[1]*v[1], v[2]*v[2]))))

def llVecNorm(v, f32 = True):
    assert isvector(v)
    if v == ZERO_VECTOR:
        return v
    f = math.sqrt(math.fsum((v[0]*v[0], v[1]*v[1], v[2]*v[2])))
    return F32(Vector((v[0]/f,v[1]/f,v[2]/f)), f32)

def llXorBase64(s, xor):
    assert isstring(s)
    assert isstring(xor)

    # Xor the underlying bytes.

    if xor == u'':
        return s

    s = b64_re.search(s).group(0)
    L1 = len(s)
    xor = b64_re.search(xor).group(0)
    L2 = len(xor)

    if L2 == 0:
        # The input xor string starts with zero or one valid Base64 characters.
        # This produces garbage bytes (the first byte is zero though).
        if L1 > 2:
            # We don't produce a result in this case.
            raise ELSLCantCompute
        L2 = 2
        xor = u'AA'

    s = b64decode(s + u'=' * (-L1 & 3))
    xor = b64decode(xor + u'=' * (-L2 & 3))
    L2 = len(xor)

    i = 0
    ret = b''

    Bug3763 = 3763 in Bugs
    # BUG-3763 consists of the binary string having an extra NULL every time after the second repetition of
    # the XOR pattern. For example, if the XOR binary string is b'pqr' and the input string is
    # b'12345678901234567890', the XOR binary string behaves as if it was b'pqrpqr\0pqr\0pqr\0pqr\0pq'.
    # We emulate that by adding the zero and increasing the length the first time.
    for c in s:
        ret += chr(ord(c) ^ ord(xor[i]))
        i += 1
        if i >= L2:
            i = 0
            if Bug3763:
                Bug3763 = False
                xor = xor + b'\x00'
                L2 += 1
    return b64encode(ret).decode('utf8')

def llXorBase64Strings(s, xor):
    assert isstring(s)
    assert isstring(xor)

    if not lslcommon.IsCalc:
        # This function has a delay, therefore it's not safe to compute it
        # unless in calculator mode.
        raise ELSLCantCompute

    if xor == u'':
        return s

    B64 = u'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'

    # Special case when the first character is not a Base64 one. (LL's ways are inextricable)
    base = B64.find(xor[0])
    if base < 0:
        if xor[0] == u'=':
            xor = u'+' + xor[1:]
            base = 62
        else:
            xor = u'/' + xor[1:]
            base = 63

    ret = u''
    i = 0
    L = len(xor)
    for c1 in s:
        val1 = B64.find(c1)
        val2 = B64.find(xor[i])
        i += 1
        if i >= L:
            i = 0

        if val1 < 0:
            ret += u'='
        else:
            if val2 < 0:
                val2 = base
                i = 1
            ret += B64[val1 ^ val2]
    return ret

def llXorBase64StringsCorrect(s, xor):
    assert isstring(s)
    assert isstring(xor)

    # Xor the underlying bytes but repeating the xor parameter pattern at the first zero (SCR-35).

    if xor == u'':
        return s


    s = b64_re.search(s).group(0)
    L1 = len(s)
    xor = b64_re.search(xor).group(0)
    L2 = len(xor)

    if L2 == 0:
        # The input xor string starts with zero or one valid Base64 characters.
        # This produces garbage bytes (the first byte is zero though).
        if L1 > 2:
            # We don't produce a result in this case.
            raise ELSLCantCompute
        L2 = 2
        xor = u'AA'

    s = b64decode(s + u'=' * (-L1 & 3))
    xor = b64decode(xor + u'=' * (-L2 & 3)) + b'\x00'

    i = 0
    ret = b''

    for c in s:
        ret += chr(ord(c) ^ ord(xor[i]))
        i += 1
        if xor[i] == b'\x00':
            i = 0
    return b64encode(ret).decode('utf8')
