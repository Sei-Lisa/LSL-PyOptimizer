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

# Transform function parameters to shorter equivalents where possible.
# This is dependent on the LSL function library.

from lslcommon import Key#, Vector, Quaternion
import lslfuncs

def OptimizeParams(node, sym):
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
