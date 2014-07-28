#!/usr/bin/env python

from lslopt.lslparse import parser,EParse
from lslopt.lsloutput import outscript
from lslopt.lsloptimizer import optimizer
import sys

def main():
    if len(sys.argv) < 2:
        sys.stderr.write(r'''LSL optimizer v0.1

Usage: %s [-O [+|-]option[,[+|-]option[,...]]] filename

That's an upper case o, not the number zero.
If filename is a dash (-) then standard input is used.

Options (+ means default, - means not default, * means not implemented):
  +extendedglobalexpr  Enables arbitrary expressions in globals (as opposed to
                       dull simple expressions allowed by regular LSL). Needs
                       the optimizer to run for the result to be compilable.
  +extendedtypecast    Allows extended typecast syntax e.g. (string)(integer)a
                       is valid with this option.
  +extendedassignment  Enables &=, |=, ^=, <<=, >>= assignment operators.
  -explicitcast        Add explicit casts where they are implicit. This option
                       is useless with 'optimize' and 'optsigns', and is of
                       basically no use in general.
  +allowkeyconcat      Allow string + key and key + string (both return string)
  +allowmultistrings   Allow C-like string juxtaposition, e.g. "ab" "cd" means
                       "abcd", no concatenation involved. Very useful when used
                       with a preprocessor (although the optimizer would
                       optimize concatenated strings if they are parenthesized
                       correctly, see note at the footer).
  +skippreproc         Skip preprocessor directives in the source as if they
                       were comments. Not useful unless the script is itself
                       the output of a preprocessor like cpp, which inserts
                       directives like: # 123 "filename".
  +optimize            Runs the optimizer.
  +optsigns            Optimize signs and float as int.
  -foldtabs            Tabs can't be copy-pasted, so they aren't optimized by
                       default. But with support from the viewer, they can be
                       folded too and make it to the uploaded source. This
                       option overrides that check, enabling optimization of
                       strings with tabs. The resulting source isn't guaranteed
                       to be copy-paste-able to a different script, though.
  * allowcescapes      Enables use of \r, \b, \xNN, \NNN, etc.
  * enableswitch       Enables Firestorm-compatible switch statements
                       (not recommended)
  * lazylists          Enables Firestorm-compatible list syntax, e.g.
                       mylist[3] = 5; v = (float)mylist[2]; (needs to know the
                       type to work). Works better with extendedtypecast.

Note that the optimizer doesn't reorder expressions to fold constants. This
means that e.g. a + 3 + 5 is not optimized to a + 8; however a + (3 + 5) is.
''' % sys.argv[0])
        return 1

    options = set(('extendedglobalexpr','extendedtypecast','extendedassignment',
        'allowkeyconcat','allowmultistrings','skippreproc','optimize','optsigns'
        ))

    if sys.argv[1] == '-O':
        if len(sys.argv) < 4:
            sys.stderr.write('Command line: Not enough parameters\n')
            return 1
        optchanges = sys.argv[2].split(',')
        for chg in optchanges:
            if chg[0:1] != '+':
                chg = '+' + chg
            if chg[0] == '-':
                options.discard(chg[1:])
            else:
                options.add(chg[1:])
        fname = sys.argv[3]
    else:
        fname = sys.argv[1]

    p = parser()
    try:
        if fname == '-':
            script = sys.stdin.read()
            p.parse(script, options)
        else:
            p.parsefile(fname, options)
        funcs = p.functions
        symtab = p.symtab
    except EParse as e:
        print e.message
        return 1
    del p

    opt = optimizer()
    opt.optimize(symtab, funcs, options)
    del opt

    outs = outscript()
    script = outs.output(symtab, options)
    del outs
    del symtab
    sys.stdout.write(script)
    return 0

ret = main()
if ret:
    sys.exit(ret)
