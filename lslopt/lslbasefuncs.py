# This module is used by the optimizer for resolving constant values.

# The functions it implements are all functions that always return the same result when given the same input, and that have no side effects.

# For example, llAbs() is here, but llFrand() is not, because it doesn't always return the same result.

# This implies that functions present in this module can be precomputed if their arguments are constants.

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
# also picky. We deal with that in the code by padding with '='*(-length&3).

# Despite what http://www.gnu.org/software/libc/manual/html_node/Parsing-of-Floats.html#Parsing-of-Floats
# says, NaN(chars) does not work in LSL (which is relevant in vectors).
# Note infinity vs. inf is necessary for parsing vectors & rotations,
# e.g. (vector)"<1,inf,infix>" is not valid but (vector)"<1,inf,infinity>" is
# as is (vector)"<1,inf,info>". The 1st gives <0,0,0>, the others <1,inf,inf>.
# The lookahead (?!i) is essential for parsing them that way without extra code.
# Note that '|' in REs is order-sensitive.
float_re = re.compile(ur'^\s*[+-]?(?:0(x)(?:[0-9a-f]+(?:\.[0-9a-f]*)?|\.[0-9a-f]+)(?:p[+-]?[0-9]+)?'
                      ur'|(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)(?:e[+-]?[0-9]+)?|infinity|inf(?!i)|nan)',
                      re.I)

int_re = re.compile(ur'^0(x)[0-9a-f]+|^\s*[+-]?[0-9]+', re.I)

key_re = re.compile(ur'^[0-9a-f]{8}(?:-[0-9a-f]{4}){4}[0-9a-f]{8}$', re.I)

b64_re = re.compile(ur'^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2,3})?')

ZERO_VECTOR      = Vector((0.0, 0.0, 0.0))
ZERO_ROTATION    = Quaternion((0.0, 0.0, 0.0, 1.0))

Infinity = float('inf')
NaN = float('nan')

class ELSLTypeMismatch(Exception):
    def __init__(self):
        super(self.__class__, self).__init__("Type mismatch")

class ELSLMathError(Exception):
    def __init__(self):
        super(self.__class__, self).__init__("Math Error")

class ELSLInvalidType(Exception):
    def __init__(self):
        super(self.__class__, self).__init__("Internal error: Invalid type")

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
        #list:           7, # Undefined
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

    # Using ctypes:
    #from ctypes import c_float
    return c_float(f).value

#    # Another alternative. frexp and ldexp solve a lot (but are still troublesome):
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

    if s[i if i != dot else i+1] >= u'5': # no rounding necessary
        # Rounding - increment s[:i] storing result into news
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
        if out == Vector: return val
        if out == unicode: return vr2s(val, 6 if InList else 5)
        raise ELSLTypeMismatch
    if tval == Quaternion:
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
            match = float_re.match(val)
            if match is None:
                return 0.0
            if match.group(1):
                return F32(float.fromhex(match.group(0)), f32)
            return F32(float(match.group(0)), f32)
        if out == int:
            match = int_re.match(val)
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
                match = float_re.match(val)
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

def minus(val):
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
    #   list+any
    #   any+list
    ta=type(a)
    tb=type(b)
    if ta in (int, float) and tb in (int, float):
        if ta == int and tb == int:
            return S32(a+b)
        return F32(a+b, f32)
    if ta == list and tb == list or ta == unicode and tb == unicode:
        return a + b
    if ta == list:
        return a + [b]
    if tb == list:
        return [a] + b
    if ta == tb in (Vector, Quaternion):
        return F32(ta(a[i]+b[i] for i in range(len(a))), f32)
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
        return F32(a-b, f32)
    if ta == tb in (Vector, Quaternion):
        return F32(ta(a[i]-b[i] for i in range(len(a))), f32)
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
            return F32(a*b, f32)
        if tb != Vector:
            # scalar * quat is not defined
            raise ELSLTypeMismatch
        # scalar * vector
        return Vector(F32((a*b[0], a*b[1], a*b[2]), f32))

    if ta == Quaternion:
        # quat * scalar and quat * vector are not defined
        if tb != Quaternion:
            raise ELSLTypeMismatch
        # quaternion product - product formula reversed
        return Quaternion(F32((a[0] * b[3] + a[3] * b[0] + a[2] * b[1] - a[1] * b[2],
                               a[1] * b[3] - a[2] * b[0] + a[3] * b[1] + a[0] * b[2],
                               a[2] * b[3] + a[1] * b[0] - a[0] * b[1] + a[3] * b[2],
                               a[3] * b[3] - a[0] * b[0] - a[1] * b[1] - a[2] * b[2]), f32))

    if ta != Vector:
        raise ELSLInvalidType # Should never happen at this point

    if tb in (int, float):
        return Vector(F32((a[0]*b, a[1]*b, a[2]*b), f32))

    if tb == Vector:
        # scalar product
        return F32(math.fsum((a[0]*b[0], a[1]*b[1], a[2]*b[2])), f32)

    if tb != Quaternion:
        raise ELSLInvalidType # Should never happen at this point

    # vector * quaternion: perform conjugation
    #v = mul(Quaternion((-b[0], -b[1], -b[2], b[3])), mul(Quaternion((a[0], a[1], a[2], 0.0)), b, f32=False))
    #return Vector((v[0], v[1], v[2]))
    # this is more precise as it goes directly to the gist of it:
    return Vector(F32((math.fsum((a[0]*(b[0]*b[0]-b[1]*b[1]-b[2]*b[2]+b[3]*b[3]),
                                 a[1]*2*(b[0]*b[1]-b[2]*b[3]),
                                 a[2]*2*(b[0]*b[2]+b[1]*b[3]))),
                       math.fsum((a[0]*2*(b[0]*b[1]+b[2]*b[3]),
                                -a[1]*(b[0]*b[0]-b[1]*b[1]+b[2]*b[2]-b[3]*b[3]), # notice minus sign
                                 a[2]*2*(b[1]*b[2]-b[0]*b[3]))),
                       math.fsum((a[0]*2*(b[0]*b[2]-b[1]*b[3]),
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
            if math.isnan(a): # NaN/anything gives math error
                raise ELSLMathError
            if ta == int and tb == int:
                # special case
                if a == -2147483648 and b == -1:
                    return a # this could be handled by using S32 but it's probably faster this way
                if (a < 0) ^ (b < 0):
                    # signs differ - Python rounds towards -inf, we need rounding towards 0
                    return - a//-b # that's -(a//-b) not (-a)//-b
                return a//b
            return F32(a/b, f32)
        if ta == Vector:
            return Vector(F32((a[0]/b, a[1]/b, a[2]/b), f32))
    if tb == Quaternion: # division by a rotation is multiplication by the conjugate of the rotation
        # defer the remaining type checks to mul()
        return mul(a, (-b[0],-b[1],-b[2],b[3]), f32)
    raise ELSLTypeMismatch

def mod(a, b, f32=True):
    # defined only for integers and vectors
    if type(a) == type(b) == int:
        if a < 0:
            return int(-((-a) % abs(b)))
        return int(a % abs(b))
    if type(a) == type(b) == Vector:
        # cross product
        return F32((a[1]*b[2]-a[2]*b[1], a[2]*b[0]-a[0]*b[2], a[0]*b[1]-a[1]*b[0]), f32)

    raise ELSLTypeMismatch

# TODO: Change shouldbeXXX to asserts
def shouldbeint(x):
    if type(x) != int:
        raise ELSLInvalidType

def shouldbefloat(x):
    if type(x) != float:
        raise ELSLInvalidType

def shouldbevector(x):
    if type(x) == Vector and len(x) == 3 and type(x[0]) == type(x[1]) == type(x[2]) == float:
        return
    raise ELSLInvalidType

def shouldberot(x):
    if type(x) == Quaternion and len(x) == 4 and type(x[0]) == type(x[1]) == type(x[2]) == type(x[3]) == float:
        return
    raise ELSLInvalidType

def shouldbestring(x):
    if type(x) != unicode:
        raise ELSLInvalidType

def shouldbekey(x):
    if type(x) != Key:
        raise ELSLInvalidType

def shouldbelist(x):
    if type(x) != list:
        raise ELSLInvalidType

#
# LSL-compatible computation functions
#

def llAbs(i):
    shouldbeint(i)
    return abs(i)

def llAcos(f):
    shouldbefloat(f)
    try:
        return F32(math.acos(f))
    except ValueError:
        return NaN

def llAngleBetween(r1, r2):
    shouldberot(r1)
    shouldberot(r2)
    return llRot2Angle(div(r1, r2, f32=False))

def llAsin(f):
    shouldbefloat(f)
    try:
        return F32(math.asin(f))
    except ValueError:
        return NaN

def llAtan2(y, x):
    shouldbefloat(y)
    shouldbefloat(x)
    return F32(math.atan2(y, x))

def llAxes2Rot(fwd, left, up):
    shouldbevector(fwd)
    shouldbevector(left)
    shouldbevector(up)

    # One of the hardest.

    t = math.fsum((fwd[0], left[1], up[2]))
    if t >= 0.: # no danger of division by zero or negative roots
        r = math.sqrt(1. + t)
        s = 0.5/r

        # For the case of ix+jy+kz > 0, it can return an unnormalized quaternion
        return Quaternion((s*(left[2]-up[1]), s*(up[0]-fwd[2]), s*(fwd[1]-left[0]), r*0.5))

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
    if q == (0.,0.,0.,0.):
        return Quaternion((0.,0.,0.,1.))
    mag = math.fsum((q[0]*q[0], q[1]*q[1], q[2]*q[2], q[3]*q[3]))
    return Quaternion(F32((q[0]/mag, q[1]/mag, q[2]/mag, q[3]/mag)))


def llAxisAngle2Rot(axis, angle):
    shouldbevector(axis)
    shouldbefloat(angle)
    axis = llVecNorm(axis)
    if axis == ZERO_VECTOR:
        angle = 0.
    c = math.cos(angle*0.5)
    s = math.sin(angle*0.5)
    return Quaternion(F32((axis[0]*s, axis[1]*s, axis[2]*s, c)))

# NOTE: This one does not always return the same value in LSL, but no one should depend
# on the garbage bytes returned. We implement it deterministically.
def llBase64ToInteger(s):
    shouldbestring(s)
    if len(s) > 8:
        return 0
    s = b64_re.match(s).group()
    i = len(s)
    s = (b64decode(s + u'='*(-i & 3)) + b'\0\0\0\0')[:4] # actually the last 3 bytes should be garbage
    i = ord(s[0]) if s[0] < b'\x80' else ord(s[0])-256
    return (i<<24)+(ord(s[1])<<16)+(ord(s[2])<<8)+ord(s[3])

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
                    or b'\xC2' <= partialchar < b'\xF4' and partialchar not in b'\xE0\xF0'
                    or partialchar == b'\xE0' and o >= 0xA0
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
            ret += u'?' * len(partialchar)
            partialchar = b''
            # fall through to process current character
        if o >= 0xC2 and o <= 0xF4:
            partialchar = c
            pending = 1 if o < 0xE0 else 2 if o < 0xF0 else 3
        elif o >= 0x80:
            ret += u'?'
        else:
            ret += c.decode('utf8')

    if partialchar:
        ret += u'?' * len(partialchar)

    return zstr(ret)

def llBase64ToString(s):
    shouldbestring(s)
    s = b64_re.match(s).group(0)
    return InternalUTF8toString(b64decode(s + u'='*(-len(s)&3)))

def llCSV2List(s):
    shouldbestring(s)

    bracketlevel = 0
    lastwascomma = False
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
    shouldbefloat(f)
    if math.isnan(f) or math.isinf(f) or f >= 2147483648.0 or f < -2147483648.0:
        return -2147483648
    return int(math.ceil(f))

def llCos(f):
    shouldbefloat(f)
    if math.isinf(f):
        return NaN
    if -9223372036854775808.0 <= f < 9223372036854775808.0:
        return F32(math.cos(f))
    return f

# The code of llDeleteSubList and llDeleteSubString is identical except for the type check
def InternalDeleteSubSequence(val, start, end):
    shouldbeint(start)
    shouldbeint(end)
    L = len(val)
    if L == 0:
        return val[:]

    # Python does much of the same thing here, which helps a lot
    if (start+L if start < 0 else start) <= (end+L if end < 0 else end):
        if end == -1: end += L
        return val[:start] + val[end+1:]
    if end == -1: end += L
    return val[end+1:start] # Exclusion range

# The code of llGetSubString and llList2List is identical except for the type check
def InternalGetSubSequence(val, start, end):
    shouldbeint(start)
    shouldbeint(end)
    L = len(val)
    if L == 0:
        return val[:]

    # Python does much of the same thing as LSL here, which helps a lot
    if start < 0: start += L
    if end < 0: end += L
    if start > end:
        if end == -1: end += L
        return val[:end+1] + val[start:] # Exclusion range
    if end == -1: end += L
    return val[start:end+1]

def llDeleteSubList(lst, start, end):
    # This acts as llList2List if there's wraparound
    shouldbelist(lst)
    return InternalDeleteSubSequence(lst, start, end)

def llDeleteSubString(s, start, end):
    # This acts as llGetSubString if there's wraparound
    shouldbestring(s)
    return InternalDeleteSubSequence(s, start, end)

def llDumpList2String(lst, sep):
    return sep.join(InternalList2Strings(lst))

def llEscapeURL(s):
    shouldbestring(s)
    s = s.encode('utf8') # get bytes
    ret = u''
    for c in s:
        if b'A' <= c <= b'Z' or b'a' <= c <= b'z' or b'0' <= c <= b'9':
            ret += c.encode('utf8')
        else:
            ret += u'%%%02X' % ord(c)
    return ret

def llEuler2Rot(v):
    shouldbevector(v)
    c0 = math.cos(v[0]*0.5)
    s0 = math.sin(v[0]*0.5)
    c1 = math.cos(v[1]*0.5)
    s1 = math.sin(v[1]*0.5)
    c2 = math.cos(v[2]*0.5)
    s2 = math.sin(v[2]*0.5)

    return Quaternion((s0 * c1 * c2 + c0 * s1 * s2,
                       c0 * s1 * c2 - s0 * c1 * s2,
                       c0 * c1 * s2 + s0 * s1 * c2,
                       c0 * c1 * c2 - s0 * s1 * s2))

def llFabs(f):
    shouldbefloat(f)
    return math.fabs(f)

def llFloor(f):
    shouldbefloat(f)
    if math.isnan(f) or math.isinf(f) or f >= 2147483648.0 or f < -2147483648.0:
        return -2147483648
    return int(math.floor(f))

# not implemented as it does not give the same output for the same input
#def llFrand(lim):

# not implemented as it does not give the same output for the same input
#def llGenerateKey():

def llGetListEntryType(lst, pos):
    shouldbelist(lst)
    shouldbeint(pos)
    try:
        return Types(lst[pos])
    except IndexError:
        return 0 # TYPE_INVALID
    except KeyError:
        raise ELSLInvalidType

def llGetListLength(lst):
    shouldbelist(lst)
    return len(lst)

def llGetSubString(s, start, end):
    shouldbestring(s)
    return InternalGetSubSequence(s, start, end)

def llInsertString(s, pos, src):
    shouldbestring(s)
    shouldbeint(pos)
    shouldbestring(src)
    if pos < 0: pos = 0 # llInsertString does not support negative indices
    return s[:pos] + src + s[pos:]

def llIntegerToBase64(x):
    shouldbeint(x)
    return b64encode(chr((x>>24)&255) + chr((x>>16)&255) + chr((x>>8)&255) + chr(x&255)).decode('utf8')

def llList2CSV(lst):
    shouldbelist(lst)
    tmp = lslcommon.LSO
    lslcommon.LSO = True # Use LSO rules for float to string conversion
    ret = u', '.join(InternalList2Strings(lst))
    lslcommon.LSO = tmp
    return ret

def llList2Float(lst, pos):
    shouldbelist(lst)
    shouldbeint(pos)
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
    shouldbelist(lst)
    shouldbeint(pos)
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
    shouldbelist(lst)
    shouldbeint(pos)
    try:
        elem = lst[pos]
        if type(elem) == Key:
            return elem
        if type(elem) == unicode:
            return Key(elem)
    except IndexError:
        pass
    if lslcommon.LSO:
        return Key(u'00000000-0000-0000-0000-000000000000') # NULL_KEY
    return Key(u'')

def llList2List(lst, start, end):
    shouldbelist(lst)
    shouldbeint(start)
    shouldbeint(end)
    return InternalGetSubSequence(lst, start, end)

def llList2ListStrided(lst, start, end, stride):
    shouldbelist(lst)
    shouldbeint(start)
    shouldbeint(end)
    shouldbeint(stride)
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
    shouldbelist(lst)
    shouldbeint(pos)
    try:
        elem = lst[pos]
        if type(elem) == Quaternion:
            return elem
    except IndexError:
        pass
    return ZERO_ROTATION

def llList2String(lst, pos):
    shouldbelist(lst)
    shouldbeint(pos)
    try:
        return InternalTypecast(lst[pos], unicode, InList=True, f32=True)
    except IndexError:
        pass
    return u''

def llList2Vector(lst, pos):
    shouldbelist(lst)
    shouldbeint(pos)
    try:
        elem = lst[pos]
        if type(elem) == Vector:
            return elem
    except IndexError:
        pass
    return ZERO_VECTOR

def llListFindList(lst, elems):
    shouldbelist(lst)
    shouldbelist(elems)
    # NaN is found in floats, but not in vectors
    L1 = len(lst)
    L2 = len(elems)
    if L2 > L1:
        return -1 # can't find a sublist longer than the original list
    if L2 == 0:
        return 0 # empty list is always found at position 0
    for i in xrange(L1-L2+1):
        for j in xrange(L2):
            e1 = lst[i+j]
            e2 = elems[j]
            if type(e1) == type(e2) == float:
                if e1 == e2:
                    continue
                if math.isnan(e1) and math.isnan(e2):
                    continue
                break
            elif type(e1) == type(e2) in (Vector, Quaternion):
                # Unfortunately, Python fails to consider (NaN,) != (NaN,) sometimes
                # so we need to implement our own test
                for e1e,e2e in zip(e1,e2):
                    if e1e != e2e: # NaNs are considered different to themselves here as normal
                        break
                else:
                    continue # equal
                break # discrepancy found
            elif type(e1) != type(e2) or e1 != e2:
                break
        else:
            return i
    return -1

def llListInsertList(lst, elems, pos):
    shouldbelist(lst)
    shouldbelist(elems)
    shouldbeint(pos)
    # Unlike llInsertString, this function does support negative indices.
    return lst[:pos] + elems + lst[pos:]

# not implemented as it does not give the same output for the same input
#def llListRandomize(x):

def llListReplaceList(lst, elems, start, end):
    shouldbelist(lst)
    shouldbelist(elems)
    shouldbeint(start)
    shouldbeint(end)
    L = len(lst)
    if (start + L if start < 0 else start) > (end + L if end < 0 else end):
        # Exclusion range. Appends elems at 'start' i.e. at end :)
        if end == -1: end += L
        return lst[end+1:start] + elems
    if end == -1: end += L
    return lst[:start] + elems + lst[end+1:]

def llListSort(lst, stride, asc):
    shouldbelist(lst)
    shouldbeint(stride)
    shouldbeint(asc)
    lst = lst[:] # make a copy
    L = len(lst)
    if stride < 1: stride = 1
    if L % stride:
        return lst
    for i in xrange(0, L-stride, stride):
        # Optimized by caching the element in the outer loop AND after swapping.
        a = lst[i]
        ta = type(a)
        if ta == Vector:
            a = a[0]*a[0] + a[1]*a[1] + a[2]*a[2]
        for j in xrange(i+stride, L, stride):
            b = lst[j]
            tb = type(b)
            gt = False
            if ta == tb:
                if tb == Vector:
                    gt = not (a <= b[0]*b[0] + b[1]*b[1] + b[2]*b[2])
                                        # (note NaNs compare as > thus the reversed condition!)
                elif tb != Quaternion:
                    gt = not (a <= b) # float integer, string, key all compare with this
                                        # (note NaNs compare as > thus the reversed condition!)
            if gt ^ (asc != 1):
                # swap
                lst[i:i+stride],lst[j:j+stride] = lst[j:j+stride],lst[i:i+stride]
                # Re-cache
                a = lst[i]
                ta = type(a)
                if ta == Vector:
                    a = a[0]*a[0] + a[1]*a[1] + a[2]*a[2]
    return lst

def llListStatistics(op, lst):
    shouldbeint(op)
    shouldbelist(lst)

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
        return F32((max - min, min, max)[op])

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
    shouldbefloat(f)
    if math.isinf(f) and f < 0 or math.isnan(f) or f <= 0.0:
        return 0.0
    return F32(math.log(f))

def llLog10(f):
    shouldbefloat(f)
    if math.isinf(f) and f < 0 or math.isnan(f) or f <= 0.0:
        return 0.0
    return F32(math.log10(f))

def llMD5String(s, salt):
    shouldbestring(s)
    shouldbeint(salt)
    return hashlib.md5(zstr(s).encode('utf8') + b':' + bytes(salt)).hexdigest().decode('utf8')

def llModPow(base, exp, mod):
    shouldbeint(base)
    shouldbeint(exp)
    shouldbeint(mod)
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
    shouldbestring(s)
    shouldbelist(exc)
    shouldbelist(inc)
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
    shouldbefloat(base)
    shouldbefloat(exp)
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
        return NaN

def llRot2Angle(r):
    shouldberot(r)
    # Version based on research by Moon Metty, Miranda Umino and Strife Onizuka
    return F32(2.*math.atan2(math.sqrt(math.fsum((r[0]*r[0], r[1]*r[1], r[2]*r[2]))), abs(r[3])));

def llRot2Axis(r):
    shouldberot(r)
    return llVecNorm((r[0], r[1], r[2]))

def llRot2Euler(r):
    shouldberot(r)

    # Another one of the hardest. The formula for Z angle in the
    # singularity case was inspired by the viewer code.
    y = 2*(r[0]*r[2] + r[1]*r[3])

    # Check gimbal lock conditions
    if abs(y) > 0.99999:
        return (0., math.asin(y), math.atan2(2.*(r[2]*r[3]+r[0]*r[1]),
            1.-2.*(r[0]*r[0]+r[2]*r[2])))

    qy2 = r[1]*r[1]
    return (
        math.atan2(2.*(r[0]*r[3]-r[1]*r[2]), 1.-2.*(r[0]*r[0]+qy2)),
        math.asin(y),
        math.atan2(2.*(r[2]*r[3]-r[0]*r[1]), 1.-2.*(r[2]*r[2]+qy2))
        )

def llRot2Fwd(r):
    shouldberot(r)
    v = (1., 0., 0.)
    if r == (0., 0., 0., 0.):
        return v
    return llVecNorm(mul(v, r, f32=False))

def llRot2Left(r):
    shouldberot(r)
    v = (0., 1., 0.)
    if r == (0., 0., 0., 0.):
        return v
    return llVecNorm(mul(v, r, f32=False))

def llRot2Up(r):
    shouldberot(r)
    v = (0., 0., 1.)
    if r == (0., 0., 0., 0.):
        return v
    return llVecNorm(mul(v, r, f32=False))

def llRotBetween(v1, v2):
    shouldbevector(v1)
    shouldbevector(v2)

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
        return Quaternion((c[0] / m, c[1] / m, c[2] / m, s / m)) # return the normalized quaternion
    if ab > 0.: # test if the angle is smaller than PI/2
        return ZERO_ROTATION # the arguments are parallel
    m = math.sqrt(v1[0] * v1[0] + v1[1] * v1[1]) # the length of one argument projected on the XY-plane
    if m != 0.:
        return Quaternion((v1[1] / m, -v1[0] / m, 0., 0.)) # return rotation with the axis in the XY-plane
    return Quaternion((0., 0., 1., 0.)) # rotate around the Z-axis

    # Algorithm by Moon Metty
    dot = mul(v1, v2, f32=False)
    cross = mod(v1, v2, f32=False)
    csq = mul(cross, cross, f32=False)

    ddc2 = dot*dot + csq

    if ddc2 >= 1.5e-45:
        if csq >= 1.5e-45:
            s = math.sqrt(ddc2) + dot;
            m = math.sqrt(csq + s*s);
            return F32(Quaternion((cross[0]/m, cross[1]/m, cross[2]/m, s/m)))

        # Deal with degenerate cases here
        if dot > 0:
            return ZERO_ROTATION
        m = math.sqrt(v1[0]*v1[0] + v1[1]*v1[1])
        if m >= 1.5e-45:
            return F32(Quaternion((v1[1]/m, -v1[0]/m, 0., 0.)))
        return Quaternion((1., 0., 0., 0.))
    return ZERO_ROTATION

def llRound(f):
    shouldbefloat(f)
    if math.isnan(f) or math.isinf(f) or f >= 2147483647.5 or f < -2147483648.0:
        return -2147483648
    return int(math.floor(f+0.5))

def llSHA1String(s):
    shouldbestring(s)
    return hashlib.sha1(s.encode('utf8')).hexdigest().decode('utf8')

def llSin(f):
    shouldbefloat(f)
    if math.isinf(f):
        return NaN
    if -9223372036854775808.0 <= f < 9223372036854775808.0:
        return F32(math.sin(f))
    return f

def llSqrt(f):
    shouldbefloat(f)
    if f < 0.0:
        return NaN
    # LSL and Python both produce -0.0 when the input is -0.0.
    return math.sqrt(f)

def llStringLength(s):
    shouldbestring(s)
    return len(s)

def llStringToBase64(s):
    shouldbestring(s)
    return b64encode(s.encode('utf8')).decode('utf8')

def llStringTrim(s, mode):
    shouldbestring(s)
    shouldbeint(mode)
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
    shouldbestring(s)
    shouldbestring(pattern)
    return s.find(pattern)

def llTan(f):
    shouldbefloat(f)
    if math.isinf(f):
        return NaN
    if -9223372036854775808.0 <= f < 9223372036854775808.0:
        # We only consider the first turn for anomalous results.
        if abs(f) == 1.570796251296997:
            return math.copysign(13245400.0, f);
        if abs(f) == 1.5707963705062866:
            return -math.copysign(22877330.0, f);
        return F32(math.tan(f))
    return f

def llToLower(s):
    shouldbestring(s)
    if lslcommon.LSO:
        return zstr(re.sub(u'[A-Z]', lambda x: x.group().lower(), s))
    return zstr(s.lower())

def llToUpper(s):
    shouldbestring(s)
    if lslcommon.LSO:
        return zstr(re.sub(u'[a-z]', lambda x: x.group().upper(), s))
    return zstr(s.upper())

def llUnescapeURL(s):
    shouldbestring(s)
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
            continue
        i += 1
        if u'0' <= c <= u'9' or u'A' <= c <= u'F' or u'a' <= c <= u'f':
            v += int(c, 16)
        ret += chr(v)
    return InternalUTF8toString(ret)

def llVecDist(v1, v2):
    shouldbevector(v1)
    shouldbevector(v2)
    return llVecMag((v1[0]-v2[0],v1[1]-v2[1],v1[2]-v2[2]))

def llVecMag(v):
    shouldbevector(v)
    return F32(math.sqrt(math.fsum((v[0]*v[0], v[1]*v[1], v[2]*v[2]))))

def llVecNorm(v):
    shouldbevector(v)
    if v == ZERO_VECTOR:
        return v
    f = math.sqrt(math.fsum((v[0]*v[0], v[1]*v[1], v[2]*v[2])))
    return F32((v[0]/f,v[1]/f,v[2]/f))

# NOTE: llXorBase64 returns garbage bytes if the input xor string
# starts with zero or one valid Base64 characters. We don't emulate that here;
# our output is deterministic.
def llXorBase64(s, xor):
    shouldbestring(s)
    shouldbestring(xor)

    # Xor the underlying bytes.

    if xor == u'':
        return s

    s = b64_re.match(s).group(0)
    L1 = len(s)
    xor = b64_re.match(xor).group(0)
    L2 = len(xor)

    if L2 == 0:
        # This is not accurate. This returns garbage (of undefined length) in LSL.
        # The first returned byte seems to be zero always though.
        xor = u'ABCD';

    s = b64decode(s + u'='*(-L1 & 3))
    xor = b64decode(xor + u'='*(-L2 & 3))
    L2 = len(xor)

    i = 0
    ret = b''

    Bug3763 = 3763 in Bugs
    # BUG-3763 consists of the binary string having an extra NULL every time after the second repetition of
    # the XOR pattern. For example, if the XOR binary stirng is b'pqr' and the input string is
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
    shouldbestring(s)
    shouldbestring(xor)

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

# NOTE: llXorBase64StringsCorrect returns garbage bytes if the input xor string
# starts with zero or one valid Base64 characters. We don't emulate that here;
# our output is deterministic.
def llXorBase64StringsCorrect(s, xor):
    shouldbestring(s)
    shouldbestring(xor)

    # Xor the underlying bytes but repeating the xor parameter pattern at the first zero (SCR-35).

    if xor == u'':
        return s


    s = b64_re.match(s).group(0)
    L1 = len(s)
    xor = b64_re.match(xor).group(0)
    L2 = len(xor)

    if L2 == 0:
        # This is not accurate. This returns garbage (of length 4?) in LSL.
        # The first returned byte seems to be zero always though.
        xor = u'ABCD'

    s = b64decode(s + u'='*(-L1 & 3))
    xor = b64decode(xor + u'='*(-L2 & 3)) + b'\x00'

    i = 0
    ret = b''

    for c in s:
        ret += chr(ord(c) ^ ord(xor[i]))
        i += 1
        if xor[i] == b'\x00':
            i = 0
    return b64encode(ret).decode('utf8')
