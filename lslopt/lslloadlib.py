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

def LoadLibrary(builtins = None, fndata = None):
    """Load builtins.txt and fndata.txt (or the given filenames) and return
    a tuple with the events, constants and functions, each in a dict.
    """

    if builtins is None:
        builtins = lslcommon.DataPath + 'builtins.txt'

    if fndata is None:
        fndata = lslcommon.DataPath + 'fndata.txt'

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
    parse_arg_re = re.compile(r'^\s*([a-z]+)\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*$')
    parse_fp_re  = re.compile(r'^\s*(-?(?=[0-9]|\.[0-9])[0-9]*'
                              r'((?:\.[0-9]*)?(?:[Ee][+-]?[0-9]+)?))\s*$')
    parse_int_re = re.compile(r'^\s*(-?0x[0-9A-Fa-f]+|-?[0-9]+)\s*$')
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
                    warning(u"Invalid type in %s, line %d: %s"
                            % (ubuiltins, linenum, typ))
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
                            warning(u"Invalid type in %s, line %d: %s"
                                    % (ubuiltins, linenum, uargtyp))
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
                        warning(u"Event at line %d was already defined in %s,"
                                u" overwriting: %s"
                                % (linenum, ubuiltins, uname))
                        del uname
                    events[name] = {'pt':tuple(args), 'NeedsData':True}
                else:
                    # Library functions go to the functions table. If
                    # they are implemented in lslfuncs.*, they get a
                    # reference to the implementation; otherwise None.
                    if name in functions:
                        uname = name.decode('utf8')
                        warning(u"Function at line %d was already defined"
                                u" in %s, overwriting: %s"
                                % (linenum, ubuiltins, uname))
                        del uname
                    fn = getattr(lslfuncs, name, None)
                    functions[name] = {'Kind':'f', 'Type':typ, 'uns':True,
                                       'ParamTypes':args, 'NeedsData':True}
                    if fn is not None:
                        functions[name]['Fn'] = fn
            elif match.group(4):
                # constant
                name = match.group(5)
                if name in constants:
                    uname = name.decode('utf8')
                    warning(u"Global at line %d was already defined in %s,"
                            u" overwriting: %s" % (linenum, ubuiltins, uname))
                    del uname
                typ = match.group(4)
                if typ not in types:
                    utyp = typ.decode('utf8')
                    warning(u"Invalid type in %s, line %d: %s"
                            % (ubuiltins, linenum, utyp))
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
                        warning(u"Invalid string in %s line %d: %s"
                                % (ubuiltins, linenum, uline))
                        value = None
                elif typ == 'key':
                    warning(u"Key constants not supported in %s, line %d: %s"
                            % (ubuiltins, linenum, uline))
                    value = None
                elif typ in ('vector', 'rotation'):
                    try:
                        if value[0:1] != '<' or value[-1:] != '>':
                            raise ValueError
                        value = value[1:-1].split(',')
                        if len(value) != (3 if typ == 'vector' else 4):
                            raise ValueError
                        num = parse_fp_re.search(value[0])
                        if not num:
                            raise ValueError
                        value[0] = lslfuncs.F32(float(num.group(1)))
                        num = parse_fp_re.search(value[1])
                        if not num:
                            raise ValueError
                        value[1] = lslfuncs.F32(float(num.group(1)))
                        num = parse_fp_re.search(value[2])
                        if not num:
                            raise ValueError
                        value[2] = lslfuncs.F32(float(num.group(1)))
                        if typ == 'vector':
                            value = Vector(value)
                        else:
                            num = parse_fp_re.search(value[3])
                            if not num:
                                raise ValueError
                            value[3] = lslfuncs.F32(float(num.group(1)))
                            value = Quaternion(value)
                    except ValueError:
                        warning(u"Invalid vector/rotation syntax in %s"
                                u" line %d: %s" % (ubuiltins, linenum, uline))
                else:
                    assert typ == 'list'
                    if value[0:1] != '[' or value[-1:] != ']':
                        warning(u"Invalid list value in %s, line %d: %s"
                                % (ubuiltins, linenum, uline))
                    elif value[1:-1].strip() != '':
                        warning(u"Non-empty list constants not supported"
                                u" in %s, line %d: %s"
                                % (ubuiltins, linenum, uline))
                        value = None
                    else:
                        value = []
                if value is not None:
                    constants[name] = value

    finally:
        f.close()

    # Load the function data table as well.

    parse_flag_re = re.compile(r'^\s*-\s+(?:(?:(sef)|return\s+'
        r'("(?:\\.|[^"])*"|<[^>]+>|[-+0-9x.e]+'  # strings, vectors, numbers
        r'|\[(?:[^]"]|"(?:\\.|[^"])*")*\]))'      # lists
        r'(?:\s+if\s+(.*\S))?'
        r'|(unstable|stop)|(min|max|delay)\s+([-0-9.]+))\s*$', re.I)

    # TODO: "quaternion" doesn't compare equal to "rotation" even if they are
    #       equivalent. Canonicalize it before comparison, to avoid false
    #       reports of mismatches.
    f = open(fndata, 'rb')
    try:
        linenum = 0
        curr_fn = None
        curr_ty = None
        skipping = False
        try:
            ufndata = fndata.decode(sys.getfilesystemencoding())
        except UnicodeDecodeError:
            # This is just a guess at the filename encoding.
            ufndata = fndata.decode('iso-8859-15')
        while True:
            linenum += 1
            line = f.readline()
            if not line: break
            if line[-1] == '\n': line = line[:-1]
            try:
                uline = line.decode('utf8')
            except UnicodeDecodeError:
                warning(u"Bad Unicode in %s line %d" % (ufndata, linenum))
                continue
            match_fn = parse_lin_re.search(line)
            if match_fn and not match_fn.group(4) and not match_fn.group(1):
                # comment or empty
                continue

            rettype = match_fn.group(1) if match_fn else None
            if match_fn and (rettype in ('void', 'event') or rettype in types):
                skipping = True  # until proven otherwise
                name = match_fn.group(2)
                uname = name.decode('utf8')
                if (rettype == 'event' and name not in events
                    or rettype != 'event' and name not in functions
                   ):
                    warning(u"%s %s is not in builtins, in %s line %d,"
                            u" skipping."
                            % (u"Function" if rettype != 'event' else u"Event",
                               uname, ufndata, linenum))
                    continue
                rettype = rettype if rettype != 'void' else None
                if 'event' != rettype != functions[name]['Type']:
                    warning(u"Function %s returns invalid type, in %s line %d,"
                            u" skipping."
                            % (uname, ufndata, linenum))
                    continue
                argnames = []
                arglist = match_fn.group(3)
                current_args = (functions[name]['ParamTypes']
                                if rettype != 'event'
                                else events[name]['pt'])
                if arglist:
                    arglist = arglist.split(',')
                    if len(current_args) != len(arglist):
                        warning(u"Parameter list mismatch in %s line %d,"
                                u" %s %s. Skipping."
                                % (ufndata, linenum,
                                   u"function" if rettype != 'event'
                                   else u"event", uname))
                        continue

                    bad = False  # used to 'continue' at this loop level
                    for idx, arg in enumerate(arglist):
                        argmatch = parse_arg_re.search(arg)
                        argtyp = argmatch.group(1)
                        argname = argmatch.group(2)
                        if current_args[idx] != argtyp:
                            warning(u"Parameter list mismatch in %s line %d,"
                                    u" %s %s. Skipping."
                                    % (ufndata, linenum,
                                       u"function" if rettype != 'event'
                                       else u"event", uname))
                            bad = True
                            break
                        argnames.append(argname)
                    if bad:
                        del bad
                        continue
                    del bad

                if 'NeedsData' not in (functions[name] if rettype != 'event'
                                       else events[name]):
                    warning(u"Duplicate %s %s in %s line %d. Skipping."
                            % (u"function" if rettype != 'event' else u"event",
                               uname, ufndata, linenum))
                    continue

                # passed all tests
                curr_fn = name
                curr_ty = rettype
                skipping = False
                if curr_ty == 'event':
                    del events[name]['NeedsData']
                else:
                    del functions[name]['NeedsData'], functions[name]['uns']

            else:
                match_flag = parse_flag_re.search(line)
                if match_flag:
                    if curr_fn is None and not skipping:
                        warning(u"Flags present before any function or event"
                                u" in %s line %d: %s"
                                % (ufndata, linenum, uline))
                        skipping = True
                        continue
                    if not skipping:
                        ucurr_fn = curr_fn.decode('utf8')
                        if match_flag.group(1):
                            # SEF
                            # We don't handle conditions yet. Take the
                            # condition as never met for now (every function
                            # that is conditionally SEF is taken as not SEF)
                            if curr_ty == 'event' and match_flag.group(3):
                                warning(u"Events do not support conditions"
                                        u" in SEF flags, in line %d, event %s"
                                        % (ufndata, linenum, ucurr_fn))
                                continue
                            elif curr_ty == 'event':
                                events[curr_fn]['SEF'] = True
                            else:
                                if not match_flag.group(3):
                                    functions[curr_fn]['SEF'] = True

                        elif curr_ty == 'event':
                            warning(u"Events only support bare SEF flags"
                                    u", in line %d, event %s. Omitting %s."
                                    % (ufndata, linenum, ucurr_fn, uline))
                            continue
                        elif match_flag.group(2):
                            pass # return not handled yet
                        elif (match_flag.group(4)
                              and match_flag.group(4).lower() == 'unstable'
                             ):
                            functions[curr_fn]['uns'] = True
                        elif match_flag.group(4):  # must be stop
                            functions[curr_fn]['stop'] = True
                        elif match_flag.group(5).lower() in ('min', 'max'):
                            minmax = match_flag.group(5).lower()
                            value = match_flag.group(6)
                            typ = functions[curr_fn]['Type']
                            if typ == 'integer':
                                good = parse_int_re.search(value)
                                if good:
                                    value = lslfuncs.S32(int(good.group(1), 0))
                            elif typ == 'float':
                                good = parse_fp_re.search(value)
                                if good:
                                    value = lslfuncs.F32(float(good.group(1)))
                            else:
                                good = False
                            if good:
                                functions[curr_fn][minmax] = value
                            else:
                                warning(u"Type mismatch or value error in %s"
                                        u" line %d: %s"
                                        % (ufndata, linenum, uline))
                                continue
                        else:  # delay
                            value = parse_fp_re.search(match_flag.group(6))
                            if not value:
                                warning(u"Invalid delay value in %s"
                                        u" line %d: %s"
                                        % (ufndata, linenum, uline))
                                continue

                            value = float(value.group(1))  # no need to F32
                            if value != 0 and 'SEF' in functions[curr_fn]:
                                warning(u"Side-effect-free function"
                                        u" %s contradicts delay, in %s"
                                        u" line %d"
                                        % (ucurr_fn, ufndata, linenum))
                                continue

                            functions[curr_fn]['delay'] = value
                else:
                    warning(u"Syntax error in %s line %d, skipping: %s"
                            % (ufndata, linenum, uline))
                    continue

    finally:
        f.close()

    # Post-checks
    for i in functions:
        ui = i.decode('utf8')
        if 'NeedsData' in functions[i]:
            del functions[i]['NeedsData']
            warning(u"Library data, file %s: Function %s has no data."
                    % (ufndata, ui))
        if 'min' in functions[i] and 'max' in functions[i]:
            if functions[i]['min'] > functions[i]['max']:
                warning(u"Library data: Function %s has min > max:"
                        u" min=%s max=%s, removing both."
                        % (ui, repr(functions[i]['min']).decode('utf8'),
                           repr(functions[i]['max'])))
                del functions[i]['min'], functions[i]['max']
        if 'SEF' in functions[i] and 'delay' in functions[i]:
            warning(u"Library data: Side-effect-free function %s contradicts"
                    u" delay. Removing SEF." % ui)
            del functions[i]['SEF']
    for i in events:
        ui = i.decode('utf8')
        if 'NeedsData' in events[i]:
            del events[i]['NeedsData']
            warning(u"Library data, file %s: Event %s has no data."
                    % (ufndata, ui))

    return events, constants, functions
