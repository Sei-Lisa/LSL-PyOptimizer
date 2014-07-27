#!/usr/bin/env python

from lslopt.lslparse import parser,EParse
from lslopt.lsloutput import outscript
from lslopt.lsloptimizer import optimizer
import sys

def main():
    if len(sys.argv) > 1:
        p = parser()
        try:
            p.parsefile(sys.argv[1])
            funcs = p.functions
            symtab = p.symtab
        except EParse as e:
            print e.message
            return 1
        del p

        opt = optimizer()
        opt.optimize(symtab, funcs)
        del opt

        outs = outscript()
        script = outs.output(symtab)
        del outs
        del symtab
        sys.stdout.write(script)
        return 0

ret = main()
if ret:
    sys.exit(ret)
