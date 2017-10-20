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

# Load the builtins and function properties.

import sys, re
from lslcommon import types, warning, Vector, Quaternion
import lslcommon, lslfuncs

def LoadLibrary(builtins = None, seftable = None):
    """Load builtins.txt and seftable.txt (or the given filenames) and return
    a tuple with the events, constants and functions, each in a dict.
    """

    if builtins is None:
        builtins = lslcommon.DataPath + 'builtins.txt'

    if seftable is None:
        seftable = lslcommon.DataPath + 'seftable.txt'

    events = {}
    constants = {}
    functions = {}


    # Library read code

    parse_lin_re = re.compile(
        r'^\s*([a-z]+)\s+'
        r'([a-zA-Z_][a-zA-Z0-9_]*)\s*\(\s*('
            r'[a-z]+\s+[a-zA-Z_][a-zA-Z0-9_]*'
            r'(?:\s*,\s*[a-z]+\s+[a-zA-Z_][a-zA-Z0-9_]*)*'
        r')?\s*\)\s*$'
        r'|'
        r'^\s*const\s+([a-z]+)'
        r'\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*=\s*(.*?)\s*$'
        r'|'
        r'^\s*(?:#.*|//.*)?$')
    parse_arg_re = re.compile(r'^\s*([a-z]+)\s+[a-zA-Z_][a-zA-Z0-9_]*\s*$')
    parse_num_re = re.compile(r'^\s*(-?(?=[0-9]|\.[0-9])[0-9]*((?:\.[0-9]*)?(?:[Ee][+-]?[0-9]+)?))\s*$')
    parse_str_re = re.compile(ur'^"((?:[^"\\]|\\.)*)"$')

    f = open(builtins, 'rb')
    try:
        linenum = 0
        try:
            ubuiltins = builtins.decode(sys.getfilesystemencoding())
        except UnicodeDecodeError:
            # This is just a guess at the filename encoding.
            ubuiltins = builtins.decode('iso-8859-15')
        while True:
            linenum += 1
            line = f.readline()
            if not line: break
            if line[-1] == '\n': line = line[:-1]
            try:
                uline = line.decode('utf8')
            except UnicodeDecodeError:
                warning(u"Bad Unicode in %s line %d" % (ubuiltins, linenum))
                continue
            match = parse_lin_re.search(line)
            if not match:
                warning(u"Syntax error in %s, line %d" % (ubuiltins, linenum))
                continue
            if match.group(1):
                # event or function
                typ = match.group(1)
                if typ == 'quaternion':
                    typ = 'rotation'
                if typ == 'void':
                    typ = None
                elif typ != 'event' and typ not in types:
                    warning(u"Invalid type in %s, line %d: %s" % (ubuiltins, linenum, typ))
                    continue
                args = []
                arglist = match.group(3)
                if arglist:
                    arglist = arglist.split(',')
                    bad = False
                    for arg in arglist:
                        argtyp = parse_arg_re.search(arg).group(1)
                        if argtyp not in types:
                            uargtyp = argtyp.decode('utf8')
                            warning(u"Invalid type in %s, line %d: %s" % (ubuiltins, linenum, uargtyp))
                            del uargtyp
                            bad = True
                            break
                        args.append(argtyp)
                    if bad:
                        continue
                name = match.group(2)
                if typ == 'event':
                    if name in events:
                        uname = name.decode('utf8')
                        warning(u"Event at line %d was already defined in %s, overwriting: %s" % (linenum, ubuiltins, uname))
                        del uname
                    events[name] = tuple(args)
                else:
                    # Library functions go to the functions table. If
                    # they are implemented in lslfuncs.*, they get a
                    # reference to the implementation; otherwise None.
                    if name in functions:
                        uname = name.decode('utf8')
                        warning(u"Function at line %d was already defined in %s, overwriting: %s" % (linenum, ubuiltins, uname))
                        del uname
                    fn = getattr(lslfuncs, name, None)
                    functions[name] = {'Kind':'f', 'Type':typ, 'ParamTypes':args}
                    if fn is not None:
                        functions[name]['Fn'] = fn
            elif match.group(4):
                # constant
                name = match.group(5)
                if name in constants:
                    uname = name.decode('utf8')
                    warning(u"Global at line %d was already defined in %s, overwriting: %s" % (linenum, ubuiltins, uname))
                    del uname
                typ = match.group(4)
                if typ not in types:
                    utyp = typ.decode('utf8')
                    warning(u"Invalid type in %s, line %d: %s" % (ubuiltins, linenum, utyp))
                    del utyp
                    continue
                if typ == 'quaternion':
                    typ = 'rotation'
                value = match.group(6)
                if typ == 'integer':
                    value = int(value, 0)
                elif typ == 'float':
                    value = lslfuncs.F32(float(value))
                elif typ == 'string':
                    value = value.decode('utf8')
                    if parse_str_re.search(value):
                        esc = False
                        tmp = value[1:-1]
                        value = u''
                        for c in tmp:
                            if esc:
                                if c == u'n':
                                    c = u'\n'
                                elif c == u't':
                                    c = u'    '
                                value += c
                                esc = False
                            elif c == u'\\':
                                esc = True
                            else:
                                value += c
                        #if typ == 'key':
                        #    value = Key(value)
                    else:
                        warning(u"Invalid string in %s line %d: %s" % (ubuiltins, linenum, uline))
                        value = None
                elif typ == 'key':
                    warning(u"Key constants not supported in %s, line %d: %s" % (ubuiltins, linenum, uline))
                    value = None
                elif typ in ('vector', 'rotation'):
                    try:
                        if value[0:1] != '<' or value[-1:] != '>':
                            raise ValueError
                        value = value[1:-1].split(',')
                        if len(value) != (3 if typ == 'vector' else 4):
                            raise ValueError
                        num = parse_num_re.search(value[0])
                        if not num:
                            raise ValueError
                        value[0] = lslfuncs.F32(float(num.group(1)))
                        num = parse_num_re.search(value[1])
                        if not num:
                            raise ValueError
                        value[1] = lslfuncs.F32(float(num.group(1)))
                        num = parse_num_re.search(value[2])
                        if not num:
                            raise ValueError
                        value[2] = lslfuncs.F32(float(num.group(1)))
                        if typ == 'vector':
                            value = Vector(value)
                        else:
                            num = parse_num_re.search(value[3])
                            if not num:
                                raise ValueError
                            value[3] = lslfuncs.F32(float(num.group(1)))
                            value = Quaternion(value)
                    except ValueError:
                        warning(u"Invalid vector/rotation syntax in %s line %d: %s" % (ubuiltins, linenum, uline))
                else:
                    assert typ == 'list'
                    if value[0:1] != '[' or value[-1:] != ']':
                        warning(u"Invalid list value in %s, line %d: %s" % (ubuiltins, linenum, uline))
                    elif value[1:-1].strip() != '':
                        warning(u"Non-empty list constants not supported in %s, line %d: %s" % (ubuiltins, linenum, uline))
                        value = None
                    else:
                        value = []
                if value is not None:
                    constants[name] = value

    finally:
        f.close()

    # Load the side-effect-free table as well.
    # TODO: Transform the SEF Table into a function properties table
    #       that includes domain data (min, max) and stability data
    #       (whether multiple successive calls return the same result)
    f = open(seftable, 'rb')
    try:
        while True:
            line = f.readline()
            if line == '':
                break
            line = line.strip()
            if line and line[0] != '#' and line in functions:
                functions[line]['SEF'] = True
    finally:
        f.close()

    return events, constants, functions
