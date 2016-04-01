#!/usr/bin/env python2

#    (C) Copyright 2015 Sei Lisa. All rights reserved.
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

# This is the main executable program that imports the libraries.

from lslopt.lslparse import parser,EParse,fieldpos
from lslopt.lsloutput import outscript
from lslopt.lsloptimizer import optimizer
import sys, os, getopt, re
import lslopt.lslcommon


VERSION = '0.2.0beta'


def ReportError(script, e):
    sys.stderr.write(script[fieldpos(script, '\n', e.lno):
                            fieldpos(script, '\n', e.lno+1)-1] + '\n')
    sys.stderr.write(' ' * e.cno + '^\n')
    sys.stderr.write(e[0] + '\n')

class UniConvScript(object):
    '''Converts the script to Unicode, setting the properties required by
    EParse to report a meaningful error position.
    '''
    def __init__(self, script):
        self.script = script

    def to_unicode(self):
        if type(self.script) is not unicode:
            try:
                self.script = self.script.decode('utf8')
            except UnicodeDecodeError as e:
                self.errorpos = e.start
                raise EParse(self, 'Invalid UTF-8 in script')
        return self.script

def PreparePreproc(script):
    s = ''
    nlines = 0
    col = 0

    # Trigraphs make our life really difficult.
    # We join lines that have \<return> or ??/<return> inside strings,
    # and we also replace regular <return> inside strings with \n, counting how
    # many lines we join, to add them back at the end of the string in order to
    # keep the line count exact prior to preprocessing. We also preserve the
    # original column of the text after the string, by adding as many spaces as
    # necessary.
    # We could let the preprocessor do the line joining on backslash-newline,
    # but by eliminating all newlines, we have control over the output column
    # of the text that follows the string and can report an accurate column
    # and line position in case of error.
    # The REs skip as much as possible in one go every time, only stopping to
    # analyze critical tokens.
    # We don't follow the C convention that backslash-return is analyzed first.
    # In c, the string "a\\<return>nb" is the same as "a\nb" which prints as
    # a<return>b. But in LSL, forgetting about the preprocessor, the string
    # "a\\<return>nb" is valid and stands for a\<return>nb. The principle of
    # least surprise seems to suggest to accept valid LSL strings as LSL
    # instead of reproducing that C quirk.
    tok = re.compile(
        r'(?:'
            r'/(?:\?\?/\n|\\\n)*\*.*?\*(?:\?\?/\n|\\\n)*/'
            r'|/(?:\?\?/\n|\\\n)*/(?:\?\?/\n|\\\n|[^\n])*\n'
            r'|[^"]'
        r')+'
        r'|"'
        , re.S)
    # RE used inside strings.
    tok2 = re.compile(
        r'(?:'
            r"\?\?[='()!<>-]"   # valid trigraph except ??/ (backslash)
            r"|(?:\?\?/|\\)(?:\?\?[/='()!<>-]|[^\n])"
                                # backslash trigraph or actual backslash,
                                # followed by any trigraph or non-newline
            r'|(?!\?\?/\n|\\\n|"|\n).'
                                # any character that doesn't start a trigraph/
                                # backslash escape followed by a newline
                                # or is a newline or double quote, as we're
                                # interested in all those individually.
        r')+'                   # as many of those as possible
        r'|\?\?/\n|\\\n|\n|"'   # or any of those individually
        )

    pos = 0
    match = tok.search(script, pos)
    while match:
        matched = match.group(0)
        pos += len(matched)
        if matched == '"':
            s += matched
            nlines = col = 0
            match2 = tok2.search(script, pos)
            while match2:
                matched2 = match2.group(0)
                pos += len(matched2)

                if matched2 == '\\\n' or matched2 == '??/\n':
                    nlines += 1
                    col = 0
                    match2 = tok2.search(script, pos)
                    continue
                if matched2 == '"':
                    if nlines:
                        if script[pos:pos+1] == '\n':
                            col = -1 # don't add spaces if not necessary
                        # col misses the quote added here, so add 1
                        s += '"' + '\n'*nlines + ' '*(col+1)
                    else:
                        s += '"'
                    break
                if matched2 == '\n':
                    nlines += 1
                    col = 0
                    s += '\\n'
                else:
                    col += len(matched2)
                    s += matched2
                match2 = tok2.search(script, pos)

        else:
            s += matched
        match = tok.search(script, pos)

    return s

def ScriptHeader(script, avname):
    if avname:
        avname = ' - ' + avname
    return ('//start_unprocessed_text\n/*'
        + re.sub(r'([*/])(?=[*|/])', r'\1|', script)
        + '*/\n//end_unprocessed_text\n//nfo_preprocessor_version 0\n'
          '//program_version LSL PyOptimizer v' + VERSION + avname
        + '\n//mono\n\n')

def Usage(about = None):
    if about is None:
        sys.stderr.write(
r'''LSL optimizer v{version}

    (C) Copyright 2015 Sei Lisa. All rights reserved.

    This program comes with ABSOLUTELY NO WARRANTY.
    This is free software, and you are welcome to redistribute it
    under certain conditions; see the file COPYING for details.
    This program is licensed under the GNU General Public License
    version 3.

Usage: {progname}
    [-O|--optimizer-options=[+|-]option[,[+|-]option[,...]]]
                                optimizer options (use '-O help' for help)
    [-h|--help]                 print this help
    [--version]                 print this program's version
    [-o|--output=<filename>]    output to file rather than stdout
    [-H|--header]               add the script as a comment in Firestorm format
    [-T|--timestamp]            add a timestamp as a comment at the beginning
    [-p|--preproc=mode]         run external preprocessor (see below for modes)
                                (resets the preprocessor command line so far)
    [-P|--prearg=<arg>]         add parameter to preprocessor's command line
    [--precmd=<cmd>]            preprocessor command ('cpp' by default)
    [--prenodef]                no LSL specific defines (__AGENTKEY__ etc.)
    [--preshow]                 show preprocessor output, and stop
    [--avid=<UUID>]             specify UUID of avatar saving the script
    [--avname=<name>]           specify name of avatar saving the script
    [--assetid=<UUID>]          specify the asset UUID of the script
    [--scriptname=<name>]       specify the script's file name
    filename                    input file

If filename is a dash (-) then standard input is used.
Use: {progname} -O help for help on the command line options.

Preprocessor modes:
    ext       Invoke preprocessor
    mcpp      Invoke mcpp as preprocessor, setting default parameters pertinent
              to it. Implies --precmd=mcpp
    gcpp      Invoke GNU cpp as preprocessor, setting default parameters
              pertinent to it. Implies --precmd=cpp
    none      No preprocessing (default)

Normally, running the preprocessor needs -O skippreproc to make the output
readable by the optimizer.

'''.format(progname=sys.argv[0], version=VERSION))
        return

    if about == 'optimizer-options':
        sys.stderr.write(
r'''
Optimizer options (+ means active by default, - means inactive by default):

  Syntax extensions options:

  extendedglobalexpr + Enables arbitrary expressions in globals (as opposed to
                       dull simple expressions allowed by regular LSL). Needs
                       constant folding active for the result to be compilable.
  breakcont          - Allow break/continue statements for loops. Note that
                       when active, 'break' and 'continue' become reserved
                       words, but when inactive they can be used as variables.
  extendedtypecast   + Allows extended typecast syntax e.g. (string)(integer)a
                       is valid with this option.
  extendedassignment + Enables &=, |=, ^=, <<=, >>= assignment operators.
  allowkeyconcat     + Allow string + key and key + string (both return string)
  allowmultistrings  + Allow C-like string juxtaposition, e.g. "ab" "cd" means
                       "abcd", no concatenation involved. Very useful when used
                       with a preprocessor. Similar to addstrings, but this one
                       is not an optimization, it introduces new syntax.
  duplabels          - Normally, a duplicate label within a function is allowed
                       by the syntax by using {} blocks; however, the server
                       will just refuse to save the script (under Mono) or do
                       something completely unexpected (under LSO: all jumps
                       will go to the last label with that name). This flag
                       works around that limitation by replacing the names of
                       the labels in the output with unique ones.

  Deprecated / compatibility syntax extensions options:

  lazylists          - Support syntax like mylist[index] = 5; rather than using
                       llListReplaceList. Only assignment supported. The list
                       is extended when the argument is greater than the list
                       length, by inserting integer zeros. This is implemented
                       for compatibility with Firestorm, but its use is not
                       recommended, as it adds a new function, wasting memory
                       against the very spirit of this program.
  enableswitch       - Support C-like switch() syntax, with some limitations.
                       Like lazylists, it's implemented for compatibility with
                       Firestorm, but not recommended. Note that the operand to
                       switch() may be evaluated more than once.
  funcoverride       - Allow duplicate function definitions to override the
                       previous definition. For compatibility with Firestorm's
                       optimizer.

  Optimization options

  optimize           + Runs the optimizer.
  optsigns           + Optimize signs in float and integer constants.
  optfloats          + Optimize floats that represent an integral value.
  constfold          + Fold constant expressions to their values, and simplify
                       some expressions and statements.
  dcr                + Dead code removal. This option removes several instances
                       of code that will never execute, and performs other
                       optimizations like removal of unused variables,
                       functions or expressions.
  shrinknames        - Reduces script memory by shrinking identifiers. In the
                       process, it turns the script into unreadable gibberish,
                       hard to debug, but this gets big savings for complex
                       scripts.
  addstrings         - Concatenate strings together when possible. Note that
                       such an optimization can be counter-productive in some
                       cases, that's why it's disabled by default. For example:
                       string a="a"+"longstring"; string b="b"+"longstring";
                       would keep a single copy of "longstring", while if the
                       strings are added, both "alongstring" and "blongstring"
                       take memory.

  Miscellaneous options

  foldtabs           - Tabs can't be copy-pasted, so expressions that produce
                       tabs, e.g. llUnescapeURL("%09"), aren't optimized by
                       default. This option overrides that check, enabling
                       expansion of functions that produce strings with tabs.
                       The resulting source isn't guaranteed to be
                       copy-paste-able to the viewer.
  nofoldtabs         - Suppress warning when a function can't be optimized
                       because it generates a string or list with a tab.
  skippreproc        + Skip preprocessor directives in the source as if they
                       were comments. Not useful unless the script is itself
                       the output of a preprocessor like GNU cpp, which inserts
                       directives like: # 123 "filename".
  explicitcast       - Add explicit casts where they are implicit. This option
                       is useless with 'optimize' and 'optsigns', and is of
                       basically no use in general, other than to see where
                       automatic casts happen.
''')
        return

def main():
    '''Main executable.'''

    # If it's good to append the basename to it, it's good to append the
    # auxiliary files' names to it, which should be located where this file is.
    lslopt.lslcommon.DataPath = __file__[:-len(os.path.basename(__file__))]

    # Default options
    options = set(('extendedglobalexpr','extendedtypecast','extendedassignment',
        'allowkeyconcat','allowmultistrings','skippreproc','optimize',
        'optsigns','optfloats','constfold','dcr'
        ))

    try:
        opts, args = getopt.gnu_getopt(sys.argv[1:], 'hO:o:p:P:HT',
            ('optimizer-options=', 'help', 'version', 'output=', 'header',
            'timestamp',
            'preproc=', 'precmd=', 'prearg=', 'prenodef', 'preshow',
            'avid=', 'avname=', 'assetid=', 'scriptname='))
    except getopt.GetoptError:
        Usage()
        return 1

    outfile = '-'
    avid = '00000000-0000-0000-0000-000000000000'
    avname = ''
    shortname = ''
    assetid = '00000000-0000-0000-0000-000000000000'
    preproc_cmdline = ['cpp']
    preproc = 'none'
    predefines = True
    script_header = ''
    script_timestamp = ''
    mcpp_mode = False
    preshow = False

    for opt, arg in opts:
        if type(opt) is unicode:
            opt = opt.encode('utf8')
        if type(arg) is unicode:
            arg = arg.encode('utf8')

        if opt in ('-O', '--optimizer-options'):
            if arg == 'help':
                Usage('optimizer-options')
                return 0

            optchanges = arg.split(',')
            for chg in optchanges:
                if chg[0:1] not in ('+', '-'):
                    chg = '+' + chg
                if chg[0] == '-':
                    options.discard(chg[1:])
                else:
                    options.add(chg[1:])

        elif opt in ('-h', '--help'):
            Usage()
            return 0

        elif opt == '--version':
            sys.stdout.write('LSL PyOptimizer version %s\n' % VERSION)
            return 0

        elif opt in ('-o', '--output'):
            outfile = arg

        elif opt in ('-p', '--preproc'):
            preproc = arg.lower()
            if preproc not in ('ext', 'gcpp', 'mcpp', 'none'):
                Usage()
                return 1

            mcpp_mode = False
            del preproc_cmdline[1:]

            if preproc == 'gcpp':
                preproc_cmdline = [
                    'cpp', '-undef', '-x', 'c', '-std=c99', '-nostdinc',
                    '-trigraphs', '-dN', '-fno-extended-identifiers',
                    ]

            elif preproc == 'mcpp':
                mcpp_mode = True
                preproc_cmdline = [
                    'mcpp', '-e', 'UTF-8', '-I-', '-N', '-3', '-j',
                    ]

            if predefines:
                preproc_cmdline += [
                    '-Dinteger(...)=((integer)(__VA_ARGS__))',
                    '-Dfloat(...)=((float)(__VA_ARGS__))',
                    '-Dstring(...)=((string)(__VA_ARGS__))',
                    '-Dkey(...)=((key)(__VA_ARGS__))',
                    '-Drotation(...)=((rotation)(__VA_ARGS__))',
                    '-Dquaternion(...)=((quaternion)(__VA_ARGS__))',
                    '-Dvector(...)=((vector)(__VA_ARGS__))',
                    '-Dlist(...)=((list)(__VA_ARGS__))',
                    ]

        elif opt == '--precmd':
            preproc_cmdline[0] = arg

        elif opt in ('-P', '--prearg'):
            preproc_cmdline.append(arg)

        elif opt == '--prenodef':
            predefines = False

        elif opt == '--preshow':
            preshow = True

        elif opt in ('-H', '--header'):
            script_header = True

        elif opt in ('-T', '--timestamp'):
            script_timestamp = True

        elif opt == '--avid':
            avid = arg

        elif opt == '--avname':
            avname = arg

        elif opt == '--assetid':
            assetid = arg

        elif opt == '--shortname':
            shortname = arg
    del opts

    fname = args[0] if args else None
    if fname is None:
        Usage()
        return 1

    del args

    script = ''
    if fname == '-':
        script = sys.stdin.read()
    else:
        f = open(fname, 'r')
        try:
            script = f.read()
        finally:
            f.close()
            del f

    if script_header:
        script_header = ScriptHeader(script, avname)

    if script_timestamp:
        import time
        tmp = time.time()
        script_timestamp = time.strftime(
            '// Generated on %Y-%m-%dT%H:%M:%S.{0:06d}Z\n'
            .format(int(tmp % 1 * 1000000)), time.gmtime(tmp))
        del tmp

    if shortname == '':
        shortname = os.path.basename(fname)

    if predefines:
        preproc_cmdline.append('-D__AGENTKEY__="' + avid + '"')
        preproc_cmdline.append('-D__AGENTID__="' + avid + '"')
        preproc_cmdline.append('-D__AGENTIDRAW__=' + avid)
        preproc_cmdline.append('-D__AGENTNAME__="' + avname + '"')
        preproc_cmdline.append('-D__ASSETID__=' + assetid)
        preproc_cmdline.append('-D__SHORTFILE__="' + shortname + '"')
        preproc_cmdline.append('-D__OPTIMIZER__=LSL PyOptimizer')
        preproc_cmdline.append('-D__OPTIMIZER_VERSION__=' + VERSION)

    if preproc != 'none':
        # At this point, for the external preprocessor to work we need the
        # script as a byte array, not as unicode, but it should be valid UTF-8.
        if type(script) is unicode:
            script = script.encode('utf8')
        else:
            try:
                # Try converting the script to Unicode, to report any encoding
                # errors with accurate line information. At this point we don't
                # need the result.
                UniConvScript(script).to_unicode()
            except EParse as e:
                # We don't call ReportError to prevent problems due to
                # displaying invalid UTF-8
                sys.stderr.write(e[0] + '\n')
                return 1
        script = PreparePreproc(script)
        if mcpp_mode:
            # As a special treatment for mcpp, we force it to output its macros
            # so we can read if USE_xxx are defined. With GCC that is achieved
            # with -dN but with mcpp there's no command line option.
            script += '\n#pragma MCPP put_defines\n'

        # Invoke the external preprocessor
        import subprocess

        p = subprocess.Popen(preproc_cmdline, stdin=subprocess.PIPE,
            stdout=subprocess.PIPE)
        script = p.communicate(input=script)[0]
        status = p.wait()
        if status:
            return status
        del p, status

        for x in re.findall(r'(?:(?<=\n)|^)\s*#\s*define\s+('
                            r'USE_SWITCHES'
                            r'|USE_LAZY_LISTS'
                            r')(?:$|[^A-Za-z0-9_])', script, re.S):
            if x == 'USE_SWITCHES':
                options.add('enableswitch')
            elif x == 'USE_LAZY_LISTS':
                options.add('lazylists')

    if not preshow:

        p = parser()
        try:
            ts = p.parse(script, options)
        except EParse as e:
            ReportError(script, e)
            return 1
        del p, script

        opt = optimizer()
        ts = opt.optimize(ts, options)
        del opt

        outs = outscript()
        script = script_header + script_timestamp + outs.output(ts, options)
        del outs, ts

    del script_header, script_timestamp

    if outfile == '-':
        sys.stdout.write(script)
    else:
        outf = open(outfile, 'w')
        try:
            outf.write(script)
        finally:
            outf.close()
    return 0

if __name__ == '__main__':
    ret = main()
    if ret:
        sys.exit(ret)
