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
import sys, os, getopt
import lslopt.lslcommon


VERSION = '0.1.1'


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
    [{{-O|--optimizer-options}} [+|-]option[,[+|-]option[,...]]]
    [-h|--help]
    [--version]
    [{{-o|--output=}} filename]
    filename

If filename is a dash (-) then standard input is used.
Use: {progname} -O help for help on the command line options.

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
        opts, args = getopt.gnu_getopt(sys.argv[1:], 'hO:o:',
            ("help", "version", "optimizer-options=", "output="))
    except getopt.GetoptError:
        Usage()
        return 1

    outfile = '-'

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

        elif opt in ('-v', '--version'):
            sys.stdout.write('LSL PyOptimizer v%s\n' % VERSION)
            return 0

        elif opt in ('-o', '--output'):
            outfile = arg
    del opts

    fname = args[0] if args else None
    if fname is None:
        Usage()
        return 1

    del args

    p = parser()
    try:
        if fname == '-':
            script = sys.stdin.read()
            ts = p.parse(script, options)
        else:
            ts = p.parsefile(fname, options)
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
