#    (C) Copyright 2015-2023 Sei Lisa. All rights reserved.
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

# Classes, functions and variables for use of all modules.

import sys
from strutil import *
strutil_used

_exclusions = frozenset({'nt','t','name','value','ch', 'X','SEF'})

# Node Record type. Used for AST nodes.
class nr(object):
    nt  = None   # node type
    t   = None   # LSL type
    ch  = None   # children
    SEF = False  # Side Effect-Free flag
    def __init__(self, **kwargs):
        for k in kwargs:
            setattr(self, k, kwargs[k])

    def copy(self):
        new = nr()
        for k, v in self.__dict__.items():
            setattr(new, k, v)
        return new

    # Debug output

    def __str__(self, indent = 0):
        spaces = ' ' * (4 * indent)
        s = '\n{sp}{{ nt:{nr.nt}\n{sp}  ,t:{nr.t}'.format(sp=spaces, nr=self)
        if hasattr(self, 'name'):
            s += '\n{sp}  ,name:{nr.name}'.format(sp=spaces, nr=self)
        if hasattr(self, 'value'):
            s += '\n{sp}  ,value:{v}'.format(sp=spaces, v=repr(self.value))
        for k in sorted(self.__dict__):
            if k not in _exclusions:
                v = self.__dict__[k]
                s += '\n{sp}  ,{k}:{v}'.format(sp=spaces, k=k, v=repr(v))
        if self.ch is not None:
            if self.ch:
                s += '\n{sp}  ,ch:['.format(sp=spaces)
                isFirst = True
                for v in self.ch:
                    if not isFirst:
                        s += ','
                    isFirst = False
                    s += v.__str__(indent + 1)
                s += '\n{sp}  ]'.format(sp=spaces)
            else:
                s += '\n{sp}  ,ch:[]'.format(sp=spaces)
        s += '\n{sp}}}'.format(sp=spaces)
        return s if indent > 0 else s[1:]  # remove leading \n at level 0

# These types just wrap the Python types to make type() work on them.
# There are no ops defined on them or anything.

class Key(unicode):
    def __repr__(self):
        return 'Key(' + super(Key, self).__repr__() + ')'

class Vector(tuple):
    def __repr__(self):
        return 'Vector(' + super(Vector, self).__repr__() + ')'

class Quaternion(tuple):
    def __repr__(self):
        return 'Quaternion(' + super(Quaternion, self).__repr__() + ')'

# Recognized: 3763, 6466, 6495
# BUG-3763 affected llXorBase64 (see lslbasefuncs.py). It's been fixed.
# BUG-6466 is about some valid numbers in JSON not being accepted. It's fixed.
# BUG-6495 is about "]" inside a JSON string closing an array. It's NOT FIXED.
Bugs = set([6495])

# LSO is generally not supported, but some calculations can handle it. The
# main intention of this setting is for the code to serve as documentation
# of how LSO's behaviour differs from Mono's in the fine details.
LSO = False

# Set to True by lslcalc's main
IsCalc = False

DataPath = ''

# Language

# These are hardcoded because additions or modifications imply
# important changes to the code anyway.
types = frozenset({'integer','float','string','key','vector',
    'quaternion','rotation','list'})

# Conversion of LSL types to Python types and vice versa.

PythonType2LSL = {int: 'integer', float: 'float',
    unicode: 'string', Key: 'key', Vector: 'vector',
    Quaternion: 'rotation', list: 'list'}

LSLType2Python = {'integer':int, 'float':float,
    'string':unicode, 'key':Key, 'vector':Vector,
    'rotation':Quaternion, 'list':list}

LSLTypeDefaults = {'integer':0, 'float':0.0, 'string':u'', 'key':Key(u''),
    'vector':Vector((0.,0.,0.)), 'rotation':Quaternion((0.,0.,0.,1.)),
    'list':[]}

def warning(txt):
    assert type(txt) == unicode
    sys.stderr.write(u"WARNING: " + txt + u"\n")
