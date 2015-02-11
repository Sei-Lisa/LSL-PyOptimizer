from lslcommon import *
from lslbasefuncs import ELSLCantCompute, isinteger, \
  isvector, NULL_KEY, ZERO_VECTOR, ZERO_ROTATION
#isfloat, isstring, iskey, isrotation, islist

TouchEvents = ('touch', 'touch_start', 'touch_end')
DetectionEvents = ('touch', 'touch_start', 'touch_end',
                   'collision', 'collision_start', 'collision_end',
                   'sensor')

def llCloud(v):
    assert isvector(v)
    return 0.0

def llAvatarOnLinkSitTarget(link):
    assert isinteger(link)
    if link > 255 or link == -2147483648:
        return Key(NULL_KEY)
    raise ELSLCantCompute

def llDetectedGrab(idx, event=None):
    assert isinteger(idx)
    if 0 <= idx <= 15 and (event == 'touch' or event is None):
        raise ELSLCantCompute
    return ZERO_VECTOR

def llDetectedGroup(idx, event=None):
    assert isinteger(idx)
    if 0 <= idx <= 15 and (event in DetectionEvents or event is None):
        raise ELSLCantCompute
    return 0

def llDetectedKey(idx, event=None):
    assert isinteger(idx)
    if 0 <= idx <= 15 and (event in DetectionEvents or event is None):
        raise ELSLCantCompute
    return Key(NULL_KEY)

def llDetectedLinkNumber(idx, event=None):
    assert isinteger(idx)
    if 0 <= idx <= 15 and (event in DetectionEvents or event is None):
        raise ELSLCantCompute
    return 0

def llDetectedName(idx, event=None):
    assert isinteger(idx)
    if 0 <= idx <= 15 and (event in DetectionEvents or event is None):
        raise ELSLCantCompute
    return u''

def llDetectedOwner(idx, event=None):
    assert isinteger(idx)
    if 0 <= idx <= 15 and (event in DetectionEvents or event is None):
        raise ELSLCantCompute
    return Key(NULL_KEY)

def llDetectedPos(idx, event=None):
    assert isinteger(idx)
    if 0 <= idx <= 15 and (event in DetectionEvents or event is None):
        raise ELSLCantCompute
    return ZERO_VECTOR

def llDetectedRot(idx, event=None):
    assert isinteger(idx)
    if 0 <= idx <= 15 and (event in DetectionEvents or event is None):
        raise ELSLCantCompute
    return ZERO_ROTATION

def llDetectedTouchBinormal(idx, event=None):
    assert isinteger(idx)
    if 0 <= idx <= 15 and (event in TouchEvents or event is None):
        raise ELSLCantCompute
    return ZERO_VECTOR

def llDetectedTouchFace(idx, event=None):
    assert isinteger(idx)
    if 0 <= idx <= 15 and (event in TouchEvents or event is None):
        raise ELSLCantCompute
    return 0

def llDetectedTouchNormal(idx, event=None):
    assert isinteger(idx)
    if 0 <= idx <= 15 and (event in TouchEvents or event is None):
        raise ELSLCantCompute
    return ZERO_VECTOR

def llDetectedTouchPos(idx, event=None):
    assert isinteger(idx)
    if 0 <= idx <= 15 and (event in TouchEvents or event is None):
        raise ELSLCantCompute
    return ZERO_VECTOR

def llDetectedTouchST(idx, event=None):
    assert isinteger(idx)
    if 0 <= idx <= 15 and (event in TouchEvents or event is None):
        raise ELSLCantCompute
    return ZERO_VECTOR

def llDetectedTouchUV(idx, event=None):
    assert isinteger(idx)
    if 0 <= idx <= 15 and (event in TouchEvents or event is None):
        raise ELSLCantCompute
    return ZERO_VECTOR

def llDetectedType(idx, event=None):
    assert isinteger(idx)
    if 0 <= idx <= 15 and (event in DetectionEvents or event is None):
        raise ELSLCantCompute
    return 0

