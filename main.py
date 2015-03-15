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

from lslopt.lslparse import parser,EParse
from lslopt.lsloutput import outscript
from lslopt.lsloptimizer import optimizer
import sys, os, getopt, re
import lslopt.lslcommon


VERSION = '0.1.1alpha'


def PreparePreproc(script):
    s = ''
    nlines = 0
    col = 0

    # Trigraphs make our life really difficult.
    # We join lines with \<return> or ??/<return> inside strings,
    # and count <return>s to add them back at the end of the string,
    # as well as spaces.
    # We skip as much as possible in one go every time, only stopping to
    # analyze critical substrings.
    tok = re.compile(r'[^"/]+|"|/(?:\?\?\/\n)*\*.*?\*(?:\?\?\/\n)*/'
        r'|/(?:\?\?\/\n)*/(?:\?\?\/.|\\.|.)*?\n'
        , re.S)
    #tok2 = re.compile(r'(?:(?!\?\?/.|\\.|"|\n).)+|\\.|\?\?/.|.', re.S)
    tok2 = re.compile(
        r"\\\n|\?\?/\n|" '"' r"|\n|"
        r"(?:"
            # negative match for the above - tough
            # eat as a unit:
            # - a backslash or corresponding trigraph followed by any trigraph
            #   or by any non-newline character
            # - any trigraph other than ??/
            # - any character that is not a newline, double quote, backslash
            #   or the start of a trigraph
            # - any trigraph-like sequence that is not a trigraph
            r"(?:\\|\?\?/)(?:\?\?[=/'()!<>\-]|[^\n])"
            r"|\?\?[='()!<>\-]"
            r"|[^\n" '"' r"\\?]|\?(?!\?[=/'()!<>\-])"
        r")+"
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
    [-H|--header]               Add the script as a comment in Firestorm format
    [-p|--preproc=mode]         run external preprocessor (default is GNU cpp)
    [-P|--prearg=<arg>]         add parameter to preprocessor's command line
                                (or command name if first after --prereset)
    [--prereset]                reset the preprocessor cmd/arg list
    [--avid=<UUID>]             specify UUID of avatar saving the script
    [--avname=<name>]           specify name of avatar saving the script
    [--assetid=<UUID>]          specify the asset UUID of the script
    [--scriptname=<name>]       specify the script's file name
    filename                    input file

If filename is a dash (-) then standard input is used.
Use: {progname} -O help for help on the command line options.

Preprocessor modes:
    external: Invoke GNU cpp
    extnodef: Invoke GNU cpp, don't add extra defines
    none:     No preprocessing (default)

'''.format(progname=sys.argv[0], version=VERSION))
        return

    if about == 'optimizer-options':
        sys.stderr.write(
r'''
Optimizer options (+ means active by default, - means inactive by default):

  Syntax extensions options:

  extendedglobalexpr + Enables arbitrary expressions in globals (as opposed to
                       dull simple expressions allowed by regular LSL). Needs
                       the optimizer to run for the result to be compilable.
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
  skippreproc        + Skip preprocessor directives in the source as if they
                       were comments. Not useful unless the script is itself
                       the output of a preprocessor like cpp, which inserts
                       directives like: # 123 "filename".
  explicitcast       - Add explicit casts where they are implicit. This option
                       is useless with 'optimize' and 'optsigns', and is of
                       basically no use in general, other than to see where
                       automatic casts happen.
''')
        return

def main():
    # If it's good to append the basename to it, it's good to append the
    # auxiliary files' names to it, which should be located where this file is.
    lslopt.lslcommon.DataPath = __file__[:-len(os.path.basename(__file__))]

    # Default options
    options = set(('extendedglobalexpr','extendedtypecast','extendedassignment',
        'allowkeyconcat','allowmultistrings','skippreproc','optimize',
        'optsigns','optfloats','constfold','dcr'
        ))

    try:
        opts, args = getopt.gnu_getopt(sys.argv[1:], 'hO:o:pP:H',
            ('optimizer-options=', 'help', 'version', 'output=', 'header',
            'preproc=', 'prereset', 'prearg=',
            'avid=', 'avname=', 'assetid=', 'scriptname='))
    except getopt.GetoptError:
        Usage()
        return 1

    outfile = '-'
    avid = '00000000-0000-0000-0000-000000000000'
    avname = ''
    shortname = ''
    assetid = '00000000-0000-0000-0000-000000000000'
    preproc_cmdline = [
        'cpp', '-undef', '-x', 'c', '-std=c99', '-nostdinc', '-trigraphs',
        '-dN', '-fno-extended-identifiers',
        '-Dinteger(x)=((integer)(x))', '-Dfloat(x)=((float)(x))',
        '-Dstring(x)=((string)(x))', '-Dkey(x)=((key)(x))',
        '-Drotation(x)=((rotation)(x))', '-Dquaternion(x)=((quaternion)(x))',
        '-Dvector(x)=((vector)(x))', '-Dlist(x)=((list)(x))']
    preproc = False
    script_header = False

    for opt, arg in opts:

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
            sys.stdout.write('LSL PyOptimizer v%s\n' % VERSION)
            return 0

        elif opt in ('-o', '--output'):
            outfile = arg

        elif opt in ('-p', '--preproc'):
            preproc = arg.lower()
            if preproc not in ('external', 'extnodef', 'none'):
                Usage()
                return 1

        elif opt == '--prereset':
            preproc_cmdline = []

        elif opt in ('-P', '--prearg'):
            preproc_cmdline.append(arg)

        elif opt in ('-H', '--header'):
            script_header = True

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

    if shortname == '':
        shortname = os.path.basename(fname)

    if preproc == 'external':
        preproc_cmdline.append('-D__AGENTKEY__="' + avid + '"')
        preproc_cmdline.append('-D__AGENTID__="' + avid + '"')
        preproc_cmdline.append('-D__AGENTIDRAW__=' + avid)
        preproc_cmdline.append('-D__AGENTNAME__="' + avname + '"')
        preproc_cmdline.append('-D__ASSETID__=' + assetid)
        preproc_cmdline.append('-D__SHORTFILE__="' + shortname + '"')
        preproc_cmdline.append('-D__OPTIMIZER__=LSL PyOptimizer')
        preproc_cmdline.append('-D__OPTIMIZER_VERSION__=' + VERSION)

    if preproc in ('external', 'extnodef'):
        \
print PreparePreproc(script)
        import subprocess
        import time

        stdout = ''
        p = subprocess.Popen(preproc_cmdline, stdin=subprocess.PIPE,
            stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        p.stdin.write(PreparePreproc(script))
        p.stdin.close()
        while True:
            status = p.poll()
            if status is not None:
                break
            stdout += p.stdout.read()
            sys.stderr.write(p.stderr.read())
            time.sleep(0.1)
        sys.stderr.write(p.stderr.read())
        stdout += p.stdout.read()
        if status:
            return status
        script = stdout
        del p, status, stdout

        if ('\n'+script).find('\n#define USE_SWITCHES\n') != -1:
            options.add('enableswitch')
        if ('\n'+script).find('\n#define USE_LAZY_LISTS\n') != -1:
            options.add('lazylists')

    p = parser()
    try:
        ts = p.parse(script, options)
    except EParse as e:
        sys.stderr.write(e.message + '\n')
        return 1
    del p

    opt = optimizer()
    ts = opt.optimize(ts, options)
    del opt

    outs = outscript()
    script = outs.output(ts, options)
    del outs
    del ts

    if script_header is not False:
        script = script_header + script
        del script_header

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
