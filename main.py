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

Options (+ means active by default, - means inactive by default):
  extendedglobalexpr + Enables arbitrary expressions in globals (as opposed to
                       dull simple expressions allowed by regular LSL). Needs
                       the optimizer to run for the result to be compilable.
  extendedtypecast   + Allows extended typecast syntax e.g. (string)(integer)a
                       is valid with this option.
  extendedassignment + Enables &=, |=, ^=, <<=, >>= assignment operators.
  explicitcast       - Add explicit casts where they are implicit. This option
                       is useless with 'optimize' and 'optsigns', and is of
                       basically no use in general.
  allowkeyconcat     + Allow string + key and key + string (both return string)
  allowmultistrings  + Allow C-like string juxtaposition, e.g. "ab" "cd" means
                       "abcd", no concatenation involved. Very useful when used
                       with a preprocessor (although the optimizer would
                       optimize concatenated strings if they are parenthesized
                       correctly, see note at the footer).
  skippreproc        + Skip preprocessor directives in the source as if they
                       were comments. Not useful unless the script is itself
                       the output of a preprocessor like cpp, which inserts
                       directives like: # 123 "filename".
  optimize           + Runs the optimizer.
  optsigns           + Optimize signs in float and integer constants.
  optfloats          + Optimize a float when it is an integral value.
  foldtabs           - Tabs can't be copy-pasted, so they aren't optimized by
                       default. But with support from the viewer, they can be
                       folded too and make it to the uploaded source. This
                       option overrides that check, enabling optimization of
                       strings with tabs. The resulting source isn't guaranteed
                       to be copy-paste-able to a different script, though.
  duplabels          - Normally, a duplicate label within a function is allowed
                       by the syntax by using {} blocks; however, the server
                       will just refuse to save the script (under Mono) or do
                       something completely unexpected (under LSO: all jumps
                       will go to the last label with that name). This flag
                       works around that limitation by replacing the names of
                       the labels in the output with unique ones.
  shrinknames        - Reduces script memory by shrinking identifiers. In the
                       process, it turns the script into unreadable gibberish,
                       hard to debug, but this gets big savings for complex
                       scripts.

Note that the optimizer doesn't reorder expressions to fold constants. This
means that e.g. a + 3 + 5 is not optimized to a + 8; however a + (3 + 5) is.
''' % sys.argv[0])
        return 1

    options = set(('extendedglobalexpr','extendedtypecast','extendedassignment',
        'allowkeyconcat','allowmultistrings','skippreproc','optimize',
        'optsigns','optfloats'
        ))

    if sys.argv[1] == '-O':
        if len(sys.argv) < 4:
            sys.stderr.write('Command line: Not enough parameters\n')
            return 1
        optchanges = sys.argv[2].split(',')
        for chg in optchanges:
            if chg[0:1] not in ('+', '-'):
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
            ts = p.parse(script, options)
        else:
            ts = p.parsefile(fname, options)
    except EParse as e:
        print e.message
        return 1
    del p

    opt = optimizer()
    ts = opt.optimize(ts, options)
    del opt

    outs = outscript()
    script = outs.output(ts, options)
    del outs
    del ts
    sys.stdout.write(script)
    return 0

ret = main()
if ret:
    sys.exit(ret)
