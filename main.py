#!/usr/bin/env python

from lslopt.lslparse import parser,EParse
from lslopt.lsloutput import outscript
from lslopt.lsloptimizer import optimizer
import sys

def main():
    if len(sys.argv) < 2:
        sys.stderr.write(r'''LSL optimizer v0.1

Usage: %s [-o option[,option[,...]]] filename

Options (* means not implemented):
  extendedglobalexpr   Enables arbitrary expressions in globals (as opposed to
                       dull simple expressions allowed by regular LSL). Needs
                       the optimizer to run for the result to be compilable.
  extendedtypecast     Allows extended typecast syntax e.g. (string)(integer)a
                       is valid with this option.
  extendedassignment   Enables &=, |=, ^=, <<=, >>= assignment operators.
  explicitcast         Add explicit casts where they are implicit. This option
                       is useless with 'optimize' and 'optsigns', and is of
                       basically no use in general.
  allowkeyconcat       Allow string + key and key + string (both return string)
  allowmultistrings    Allow C-like string juxtaposition, e.g. "ab" "cd" means
                       "abcd", no concatenation involved. Very useful when used
                       with a preprocessor (although the optimizer would
                       optimize concatenated strings if they are parenthesized
                       correctly, see note at the footer).
  skippreproc          Skip preprocessor directives in the source as if they
                       were comments. Not useful unless the script is itself
                       the output of a preprocessor like cpp, which inserts
                       directives like: # 123 "filename".
  optimize             Runs the optimizer.
  optsigns             Optimize signs and float as int.
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

    if sys.argv[1] == '-o':
        if len(sys.argv) < 4:
            sys.stderr.write('Command line: Not enough parameters\n')
            return 1
        options = sys.argv[2].split(',')
        fname = sys.argv[3]
    else:
        options = ()
        fname = sys.argv[1]

    p = parser()
    try:
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
