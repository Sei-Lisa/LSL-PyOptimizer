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

# Classes, functions and variables for use of all modules.

import sys

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
types = frozenset(('integer','float','string','key','vector',
    'quaternion','rotation','list'))

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

# Debug function
def print_node(node, indent = 0):
    nt = node['nt']
    write = sys.stdout.write
    spaces = ' ' * (indent*4+2)
    write('%s{ nt:%s\n' % (' '*(indent*4), nt))
    if 't' in node:
        write('%s,t:%s\n' % (spaces, node['t']))
    if 'name' in node:
        write('%s,name:%s\n' % (spaces, node['name']))
    if 'value' in node:
        write('%s,value:%s\n' % (spaces, repr(node['value'])))

    for prop in node:
        if prop not in ('ch', 'nt', 't', 'name', 'value','X','SEF'):
            write('%s,%s:%s\n' % (spaces, prop, repr(node[prop])))
    if 'ch' in node:
        write(spaces + ',ch:[\n')
        for subnode in node['ch']:
            print_node(subnode, indent+1)
        write(spaces + ']\n')
    write(' '*(indent*4) + '}\n\n')
