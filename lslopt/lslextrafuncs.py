#    (C) Copyright 2015-2025 Sei Lisa. All rights reserved.
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

# Extra functions that have predictable return values for certain arguments.

from lslopt.lslcommon import Key, Vector #, Quaternion
from lslopt.lslbasefuncs import ELSLCantCompute, fi,ff,fs,fk,v2f,q2f,fl, \
  NULL_KEY, ZERO_VECTOR, ZERO_ROTATION, \
  TOUCH_INVALID_TEXCOORD, cond
#from strutil import unicode
ff, q2f  # keep pyflakes happy as these are not used

#GetEnvSettings = frozenset({'agent_limit', 'dynamic_pathfinding', 'estate_id'
#    , 'estate_name', 'frame_number', 'region_cpu_ratio', 'region_idle'
#    , 'region_product_name', 'region_product_sku', 'region_start_time'
#    , 'sim_channel', 'sim_version', 'simulator_hostname'
#    , 'region_max_prims'  # <http://wiki.secondlife.com/wiki/Release_Notes/Second_Life_RC_Magnum/16#16.11.02.321369>
#    , 'region_object_bonus'  # <http://wiki.secondlife.com/wiki/Release_Notes/Second_Life_RC_Magnum/16#16.12.03.322072>
#    , 'whisper_range', 'chat_range', 'shout_range'  #? (jun 2020)
#    , 'agent_limit_max', 'agent_reserved', 'agent_underserved'  # <https://releasenotes.secondlife.com/simulator/2022-04-21.571166.html>
#    })

# Values valid for llGetVisualDetails parameters
GVD_ValidValues = \
    { 33, "height"
    , 38, "torso_length"
    , 80, "male"
    , 198, "heel_height"
    , 503, "platform_height"
    , 616, "shoe_height"
    , 692, "leg_length"
    , 693, "arm_length"
    , 756, "neck_length"
    , 814, "waist_height"
    , 842, "hip_length"
    , 11001, "hover"
    }

xp_error_messages = {
    -1:u'unknown error id',
    0:u'no error', 1:u'exceeded throttle', 2:u'experiences are disabled',
    3:u'invalid parameters', 4:u'operation not permitted',
    5:u'script not associated with an experience', 6:u'not found',
    7:u'invalid experience', 8:u'experience is disabled',
    9:u'experience is suspended', 10:u'unknown error',
    11:u'experience data quota exceeded',
    12:u'key-value store is disabled',
    13:u'key-value store communication failed', 14:u'key doesn\'t exist',
    15:u'retry update', 16:u'experience content rating too high',
    17:u'not allowed to run in current location',
    18:u'experience permissions request timed out'
}

valid_inventory_kinds = frozenset({0, 1, 3, 5, 6, 7, 10, 13, 20, 21})

def llCloud(v):
    v = v2f(v)
    return 0.0

def llAvatarOnLinkSitTarget(link):
    link = fi(link)
    if link > 255 or link == -2147483648:
        return Key(NULL_KEY)
    raise ELSLCantCompute

# llClearPrimMedia always has side effects (emits errors for every face that
# is not supported)
def llClearLinkMedia(link, face):
    if link > 255 or link == -2147483648:
        return 0
    raise ELSLCantCompute

def llDetectedGrab(idx, evsym=None):
    idx = fi(idx)
    if 0 <= idx <= 15 and (evsym is None or 'grab' in evsym):
        raise ELSLCantCompute
    return ZERO_VECTOR

def llDetectedGroup(idx, evsym=None):
    idx = fi(idx)
    if 0 <= idx <= 15 and (evsym is None or 'detect' in evsym):
        raise ELSLCantCompute
    return 0

def llDetectedKey(idx, evsym=None):
    idx = fi(idx)
    if 0 <= idx <= 15 and (evsym is None or 'detect' in evsym):
        raise ELSLCantCompute
    return Key(NULL_KEY)

def llDetectedLinkNumber(idx, evsym=None):
    idx = fi(idx)
    if 0 <= idx <= 15 and (evsym is None or 'detect' in evsym):
        raise ELSLCantCompute
    return 0

def llDetectedName(idx, evsym=None):
    idx = fi(idx)
    if 0 <= idx <= 15 and (evsym is None or 'detect' in evsym):
        raise ELSLCantCompute
    return NULL_KEY

def llDetectedOwner(idx, evsym=None):
    idx = fi(idx)
    if 0 <= idx <= 15 and (evsym is None or 'detect' in evsym):
        raise ELSLCantCompute
    return Key(NULL_KEY)

def llDetectedPos(idx, evsym=None):
    idx = fi(idx)
    if 0 <= idx <= 15 and (evsym is None or 'detect' in evsym):
        raise ELSLCantCompute
    return ZERO_VECTOR

def llDetectedRot(idx, evsym=None):
    idx = fi(idx)
    if 0 <= idx <= 15 and (evsym is None or 'detect' in evsym):
        raise ELSLCantCompute
    return ZERO_ROTATION

def llDetectedTouchBinormal(idx, evsym=None):
    idx = fi(idx)
    if 0 <= idx <= 15 and (evsym is None or 'touch' in evsym):
        raise ELSLCantCompute
    return ZERO_VECTOR

def llDetectedTouchFace(idx, evsym=None):
    idx = fi(idx)
    if 0 <= idx <= 15 and (evsym is None or 'touch' in evsym):
        raise ELSLCantCompute
    return -1 if 'detect' in evsym and 0 <= idx <= 15 else 0

def llDetectedTouchNormal(idx, evsym=None):
    idx = fi(idx)
    if 0 <= idx <= 15 and (evsym is None or 'touch' in evsym):
        raise ELSLCantCompute
    return ZERO_VECTOR

def llDetectedTouchPos(idx, evsym=None):
    idx = fi(idx)
    if 0 <= idx <= 15 and (evsym is None or 'touch' in evsym):
        raise ELSLCantCompute
    return ZERO_VECTOR

def llDetectedTouchST(idx, evsym=None):
    idx = fi(idx)
    if 0 <= idx <= 15 and (evsym is None or 'detect' in evsym):
        # In detection events that are not touch events, it returns
        # TOUCH_INVALID_TEXCOORD if idx < num, else ZERO_VECTOR,
        # but we only know that num >= 1.
        if idx == 0 and evsym is not None and 'touch' not in evsym:
            # index 0 always exists, so we know the result
            return TOUCH_INVALID_TEXCOORD
        raise ELSLCantCompute
    return ZERO_VECTOR

def llDetectedTouchUV(idx, evsym=None):
    idx = fi(idx)
    if 0 <= idx <= 15 and (evsym is None or 'detect' in evsym):
        # In detection events that are not touch events, it returns
        # TOUCH_INVALID_TEXCOORD if idx < num, else ZERO_VECTOR,
        # but we only know that num >= 1.
        if idx == 0 and evsym is not None and 'touch' not in evsym:
            # index 0 always exists, so we know the result
            return TOUCH_INVALID_TEXCOORD
        raise ELSLCantCompute
    return ZERO_VECTOR

def llDetectedType(idx, evsym=None):
    idx = fi(idx)
    if 0 <= idx <= 15 and (evsym is None or 'detect' in evsym):
        raise ELSLCantCompute
    return 0

def llDetectedVel(idx, evsym=None):
    idx = fi(idx)
    if 0 <= idx <= 15 and (evsym is None or 'detect' in evsym):
        raise ELSLCantCompute
    return ZERO_VECTOR

def llEdgeOfWorld(v1, v2):
    v1 = v2f(v1)
    v2 = v2f(v2)
    if v2[0] == v2[1] == 0:
        return 1
    raise ELSLCantCompute

def llGetAgentInfo(id):
    id = fk(id)
    if not cond(id):
        return 0
    raise ELSLCantCompute

def llGetAgentLanguage(id):
    id = fk(id)
    if not cond(id):
        return u''
    raise ELSLCantCompute

def llGetAgentList(scope, options):
    scope = fi(scope)
    options = fl(options)
    if scope not in (1, 2, 4):
        return [u'INVALID_SCOPE']
    raise ELSLCantCompute

def llGetAgentSize(id):
    id = fk(id)
    if not cond(id):
        return ZERO_VECTOR
    raise ELSLCantCompute

def llGetAlpha(face):
    face = fi(face)
    if face > 8:
        return 1.0
    # Negative face numbers return (float)llGetNumberOfSides(), which isn't
    # computable.
    raise ELSLCantCompute

def llGetAnimation(id):
    id = fk(id)
    if not cond(id):
        return u''
    raise ELSLCantCompute

def llGetAnimationList(id):
    id = fk(id)
    if not cond(id):
        return []
    raise ELSLCantCompute

def llGetAttachedList(id):
    id = fk(id)
    if not cond(id):
        return [u'NOT FOUND']
    raise ELSLCantCompute

def llGetBoundingBox(id):
    id = fk(id)
    if not cond(id):
        return []
    raise ELSLCantCompute

def llGetColor(face):
    face = fi(face)
    if face > 8:
        return Vector((1.,1.,1.))
    # Returns the average colour when negative (can't be computed)
    raise ELSLCantCompute

def llGetDisplayName(id):
    id = fk(id)
    if not cond(id):
        return u''
    raise ELSLCantCompute

def llGetEnv(s):
    s = fs(s)
#    if s not in GetEnvSettings:
#        return u""
    raise ELSLCantCompute

def llGetExperienceErrorMessage(errno):
    errno = fi(errno)
    if errno < -1 or errno > 18:
        errno = -1
    return xp_error_messages[errno]

def llGetExperienceList(id):
    id = fk(id)
    # This function is not implemented and always returns empty list
    return []

def llGetHTTPHeader(id, s):
    id = fk(id)
    s = fs(s)
    if not cond(id):
        return u''
    raise ELSLCantCompute

def llGetInventoryKey(s):
    s = fs(s)
    if s == u'':
        return Key(NULL_KEY)
    raise ELSLCantCompute

def llGetInventoryName(kind, index):
    kind = fi(kind)
    index = fi(index)
    if kind != -1 and kind not in valid_inventory_kinds or index < 0:
        return u''
    raise ELSLCantCompute

def llGetInventoryNumber(kind):
    kind = fi(kind)
    if kind != -1 and kind not in valid_inventory_kinds:
        return 0
    raise ELSLCantCompute

def llGetInventoryPermMask(item, category):
    item = fs(item)
    category = fi(category)
    if category < 0 or category > 4 or item == u'':
        return 0
    raise ELSLCantCompute

def llGetLinkName(link):
    link = fi(link)
    if link < -2147483646 or link > 256:
        return NULL_KEY
    raise ELSLCantCompute

def llGetLinkSitFlags(link):
    link = fi(link)
    if link > 256 or (link < 0 and link != -4):
        return 0
    raise ELSLCantCompute

def llGetObjectLinkKey(id, link):
    # TODO: Investigate behaviour with invalid key, invalid link etc.
    raise ELSLCantCompute

def llGetOwnerKey(id):
    id = fk(id)
    if not cond(id):
        return Key(NULL_KEY)
    raise ELSLCantCompute

def llGetStatus(mask):
    # Leave out STATUS_CAST_SHADOWS from the flags to check.
    if (mask & ~0x200) == 0:
        return 0
    raise ELSLCantCompute

def llGetVisualParams(id, params):
    id = fk(id)
    params = fl(params)
    if not cond(id) or params.nt == 'CONST' and not params.ch:
        return []
# TODO: This needs to check whether the list is a literal, and whether the
# elements are constant.
#    if params.nt == 'LIST':
#    for i in params:
#        if (i.lower() if type(i) == unicode else i) in GVD_ValidValues:
#            raise ELSLCantCompute
#    return [u""] * len(params)
    raise ELSLCantCompute

def llIsFriend(id):
    id = fk(id)
    if not cond(id):
        return 0
    raise ELSLCantCompute

# TODO: Add more predictable functions.
