#    (C) Copyright 2015-2017 Sei Lisa. All rights reserved.
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

# Optimize calls to LSL library functions and parameters where possible
# This is dependent on the LSL function library.

import lslcommon
from lslcommon import Key, Vector, Quaternion
import lslfuncs

def OptimizeArgs(node, sym):
    """Transform function arguments to shorter equivalents where possible."""
    assert node['nt'] == 'FNCALL'
    params = node['ch']
    name = node['name']

    if name in ('llSensor', 'llSensorRepeat'):
        # The cutoff value is at a bit less than 3.1275 for some reason,
        # but we use 3.14159.
        if params[4]['nt'] == 'CONST' and params[4]['t'] == 'float' and params[4]['value'] > 3.14159:
            params[4]['value'] = 4.0

    types = sym['ParamTypes']
    if name != 'llMessageLinked':
        # Transform invalid/null keys to "" e.g. llGetOwnerKey(NULL_KEY) -> llGetOwnerKey("")
        # llMessageLinked is the exception.
        for i in range(len(types)):
            if types[i] == 'key':
                if params[i]['nt'] == 'CONST':
                    if not lslfuncs.cond(Key(params[i]['value'])):
                        params[i]['value'] = u""
                        params[i]['type'] = 'string'


# Type of each entry in llGetObjectDetails. Last: 38.
objDetailsTypes = 'issvrvkkkiiififfffkiiiiiiffkiviiksiisii'
primParamsTypes = \
    ( False, False # 0 (unassigned) and 1=PRIM_TYPE_LEGACY
    , 'i' # 2=PRIM_MATERIAL
    , 'i' # 3=PRIM_PHYSICS
    , 'i' # 4=PRIM_TEMP_ON_REZ
    , 'i' # 5=PRIM_PHANTOM
    , 'v' # 6=PRIM_POSITION
    , 'v' # 7=PRIM_SIZE
    , 'r' # 8=PRIM_ROTATION
    , 'i*' # 9=PRIM_TYPE
    , False, False, False, False # 10, 11, 12, 13 (unassigned)
    , False, False, False # 14, 15, 16 (unassigned)
    , 'svvf' # 17=PRIM_TEXTURE
    , 'vf' # 18=PRIM_COLOR
    , 'ii' # 19=PRIM_BUMP_SHINY
    , 'i' # 20=PRIM_FULLBRIGHT
    , 'iiffffv' # 21=PRIM_FLEXIBLE
    , 'i' # 22=PRIM_TEXGEN
    , 'ivfff' # 23=PRIM_POINT_LIGHT
    , False # 24 (unassigned)
    , 'f' # 25=PRIM_GLOW
    , 'svf' # 26=PRIM_TEXT
    , 's' # 27=PRIM_NAME
    , 's' # 28=PRIM_DESC
    , 'r' # 29=PRIM_ROT_LOCAL
    , 'i' # 30=PRIM_PHYSICS_SHAPE_TYPE
    , False # 31 (unassigned)
    , 'vff' # 32=PRIM_OMEGA
    , 'v' # 33=PRIM_POS_LOCAL
    , '' # 34=PRIM_LINK_TARGET
    , 'v' # 35=PRIM_SLICE
    , 'svvfvii' # 36=PRIM_SPECULAR
    , 'svvf' # 37=PRIM_NORMAL
    , 'ii' # 38=PRIM_ALPHA_MODE
    , 'i' # 39=PRIM_ALLOW_UNSIT
    , 'i' # 40=PRIM_SCRIPTED_SIT_ONLY
    , 'ivv' # 41=PRIM_SIT_TARGET
    )
# Primitive Params with arguments. F=face, L=link.
primParamsArgs = \
    { 17: 'F' # 17=PRIM_TEXTURE
    , 18: 'F' # 18=PRIM_COLOR
    , 19: 'F' # 19=PRIM_BUMP_SHINY
    , 20: 'F' # 20=PRIM_FULLBRIGHT
    , 22: 'F' # PRIM_TEXGEN
    , 25: 'F' # PRIM_GLOW
    , 34: 'L' # PRIM_LINK_TARGET
    , 36: 'F' # PRIM_SPECULAR
    , 37: 'F' # PRIM_NORMAL
    , 38: 'F' # PRIM_ALPHA_MODE
    }

# Compatibility: list extraction function / input type (by type's first
# letter), e.g. 'si' means llList2String can extract an integer.
listCompat = frozenset({'ss', 'sk', 'si', 'sf', 'sv', 'sr', 'ks', 'kk',
                        'is', 'ii', 'if', 'fs', 'fi', 'ff', 'vv', 'rr'})

defaultListVals = {'llList2Integer':0, 'llList2Float':0.0,
    'llList2String':u'',
    # llList2Key is set programmatically in FuncOptSetup
    #'llList2Key':Key(u''),
    'llList2Vector':Vector((0.,0.,0.)),
    'llList2Rot':Quaternion((0.,0.,0.,1.))}

# The 'self' parameter here is the constant folding object.
def OptimizeFunc(self, parent, index):
    """Look for possible optimizations taking advantage of the specific LSL
    library function semantics.
    """
    node = parent[index]
    assert node['nt'] == 'FNCALL'
    name = node['name']
    child = node['ch']
    if self.optlistlength and name == 'llGetListLength':
        # Convert llGetListLength(expr) to (expr != [])
        node = {'nt':'CONST', 't':'list', 'value':[]}
        parent[index] = node = {'nt':'!=', 't':'integer',
                                'ch':[child[0], node]}
        # SEF if the list is
        node['SEF'] = 'SEF' in child[0]
    if name == 'llDumpList2String':
        if (child[1]['nt'] == 'CONST'
            and child[1]['t'] in ('string', 'key')
            and child[1]['value'] == u""
           ):
            # Convert llDumpList2String(expr, "") to (string)(expr)
            node['nt'] = 'CAST'
            del child[1]
            del node['name']
            return

    if (name in ('llList2String', 'llList2Key', 'llList2Integer',
                 'llList2Float', 'llList2Vector', 'llList2Rot')
        and child[1]['nt'] == 'CONST'
       ):
        # 2nd arg to llList2XXXX must be integer
        assert child[1]['t'] == 'integer'

        listarg = child[0]
        idx = child[1]['value']
        value = self.GetListNodeElement(listarg, idx)
        tvalue = self.TypeFromNodeOrConst(value)
        const = self.ConstFromNodeOrConst(value)
        if const is not False and 'SEF' in node:
            # Managed to get a constant from a list, even if the
            # list wasn't constant. Handle the type conversion.
            if (node['t'][0] + tvalue[0]) in listCompat:
                const = lslfuncs.InternalTypecast(const,
                    lslcommon.LSLType2Python[node['t']],
                    InList=True, f32=True)
            else:
                const = defaultListVals[name]

            parent[index] = {'nt':'CONST', 't':node['t'],
                             'value':const, 'SEF':True}
            return

        if listarg['nt'] == 'FNCALL' \
           and listarg['name'] == 'llGetObjectDetails':

            listarg = listarg['ch'][1] # make it the list argument of llGetObjectDetails
            value = self.GetListNodeElement(listarg, idx)
            tvalue = self.TypeFromNodeOrConst(value)
            const = self.ConstFromNodeOrConst(value)
            if type(const) == int and self.GetListNodeLength(listarg) == 1:
                # Some of these can be handled with a typecast to string.
                if name == 'llList2String':
                    # turn the node into a cast of arg 0 to string
                    node['nt'] = 'CAST'
                    del child[1]
                    del node['name']
                    return
                # The other ones that support cast to string then to
                # the final type in some cases (depending on the
                # list type, which we know) are key/int/float.
                finaltype = objDetailsTypes[const:const+1]
                if (name == 'llList2Key' # checked via listCompat
                    or (name == 'llList2Integer'
                        and finaltype in ('s', 'i')) # won't work for floats
                    or (name == 'llList2Float'
                        and finaltype in ('s', 'i')) # won't work for floats
                   ) and (node['t'][0] + finaltype) in listCompat:
                    # ->  (key)((string)llGetObjectDetails...)
                    # or (integer)((string)llGetObjectDetails...)
                    node['nt'] = 'CAST'
                    del child[1]
                    del node['name']
                    child[0] = {'nt':'CAST', 't':'string',
                                'ch':[child[0]]}
                    if 'SEF' in child[0]['ch'][0]:
                        child[0]['SEF'] = True
                    return

            # Check for type incompatibility or index out of range
            # and replace node with a constant if that's the case
            if (value is False
                or type(const) == int
                   and (node['t'][0] + objDetailsTypes[const])
                       not in listCompat
               ) and 'SEF' in node:
                parent[index] = {'nt':'CONST', 't':node['t'],
                    'value':defaultListVals[name],
                    'SEF':True}

        elif listarg['nt'] == 'FNCALL' and listarg['name'] in (
             'llGetPrimitiveParams', 'llGetLinkPrimitiveParams'):
            # We're going to work with the primitive params list.
            listarg = listarg['ch'][
                0 if listarg['name'] == 'llGetPrimitiveParams'
                else 1]
            length = self.GetListNodeLength(listarg)
            if length is not False:
                # Construct a list (string) of return types.
                # A '*' in the list means the type can't be
                # determined past this point (used with PRIM_TYPE).
                i = 0
                returntypes = ''
                while i < length:
                    param = self.GetListNodeElement(listarg, i)
                    param = self.ConstFromNodeOrConst(param)
                    if (param is False
                        or type(param) != int
                        # Parameters with arguments have
                        # side effects (errors).
                        # We could check whether there's a face
                        # argument and the face is 0, which is
                        # guaranteed to exist, but it's not worth
                        # the effort.
                        or param in primParamsArgs
                        or param < 0
                        or param >= len(primParamsTypes)
                        or primParamsTypes[param] is False
                       ):
                        # Can't process this list.
                        returntypes = '!'
                        break
                    returntypes += primParamsTypes[param]
                    i += 1
                if returntypes != '!':
                    if (len(returntypes) == 1
                        and returntypes != '*'
                        and idx in (0, -1)
                       ):
                        if name == 'llList2String':
                            node['nt'] = 'CAST'
                            del child[1]
                            del node['name']
                            return
                        if ((name == 'llList2Key'
                             or name == 'llList2Integer'
                                and returntypes in ('s', 'i')
                             or name == 'llList2Float'
                                and returntypes in ('s', 'i')
                            )
                            and (node['t'][0] + returntypes)
                                in listCompat
                           ):
                            node['nt'] = 'CAST'
                            del child[1]
                            del node['name']
                            child[0] = {'nt':'CAST', 't':'string',
                                        'ch':[child[0]]}
                            if 'SEF' in child[0]['ch'][0]:
                                child[0]['SEF'] = True
                            return

                    if (returntypes.find('*') == -1
                        or idx >= 0 and idx < returntypes.find('*')
                        or idx < 0 and idx > returntypes.rfind('*')
                                             - len(returntypes)
                       ):
                        # Check for type incompatibility or index
                        # out of range.
                        if idx < 0:
                            # s[-1:0] doesn't return the last char
                            # so we have to compensate
                            idx += len(returntypes)
                        if (node['t'][0] + returntypes[idx:idx+1]) \
                           not in listCompat \
                           and 'SEF' in node:
                            parent[index] = {'nt':'CONST',
                                't':node['t'],
                                'value':defaultListVals[name],
                                'SEF':True}
                            return

                del returntypes

        del listarg, idx, value, tvalue, const
        return

    if name == 'llDialog':
        if self.GetListNodeLength(child[2]) == 1:
            button = self.ConstFromNodeOrConst(self.GetListNodeElement(child[2],
                                                                       0))
            if type(button) == unicode and button == u'OK':
                # remove the element, as 'OK' is the default button in SL
                child[2] = {'nt':'CONST','t':'list','value':[],'SEF':True}
        return

    if (name == 'llDeleteSubList'
        or name == 'llListReplaceList' and child[1]['nt'] == 'CONST'
           and not child[1]['value']
       ):
        # llDeleteSubList(x, 0, -1)  ->  [] if x is SEF
        # llListReplaceList(x, [], 0, -1)  ->  [] if x is SEF
        if ('SEF' in child[0]
            and child[-2]['nt'] == 'CONST' and child[-1]['nt'] == 'CONST'
            and child[-2]['value'] == 0 and child[-1]['value'] == -1
           ):
            parent[index] = {'nt':'CONST','SEF':True,'t':'list','value':[]}
            return

def FuncOptSetup():
    # Patch the default values list for LSO
    if lslcommon.LSO:
        defaultListVals['llList2Key'] = Key(lslfuncs.NULL_KEY)
    else:
        defaultListVals['llList2Key'] = Key(u"")
