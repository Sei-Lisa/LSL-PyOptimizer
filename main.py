#!/usr/bin/env python

from lslopt.lslparse import parser,EParse
from lslopt.lsloutput import outscript
import sys

def main():
    if len(sys.argv) > 1:
        p = parser()
        try:
            symtab = p.parsefile(sys.argv[1])
        except EParse as e:
            print e.message
            return 1
        del p
        outs = outscript()
        script = outs.output(symtab)
        del outs
        del symtab
        print script.decode('utf8'),
        return 0

sys.exit(main())
