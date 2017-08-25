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

# Optimizer class that wraps and calls the other parts.

import lslfuncs
from lslcommon import Key, Vector, Quaternion

from lslfoldconst import foldconst
from lslrenamer import renamer
from lsldeadcode import deadcode

class optimizer(foldconst, renamer, deadcode):

    # Default values per type when declaring variables
    DefaultValues = {'integer': 0, 'float': 0.0, 'string': u'',
        'key': lslfuncs.Key(u''), 'vector': lslfuncs.ZERO_VECTOR,
        'rotation': lslfuncs.ZERO_ROTATION, 'list': []
        }

    # explicitly exclude assignments
    binary_ops = frozenset(('+','-','*','/','%','<<','>>','<','<=','>','>=',
        '==','!=','|','^','&','||','&&'))
    assign_ops = frozenset(('=','+=','-=','*=','/=','%=','&=','|=','^=','<<=','>>='))

    def Cast(self, value, newtype):
        """Return a CAST node if the types are not equal, otherwise the
        value unchanged.
        """
        if value['t'] == newtype:
            return value
        ret = {'nt':'CAST', 't':newtype, 'ch':[value]}
        if 'SEF' in value:
            ret['SEF'] = True
        if 'X' in value:
            ret['X'] = value['X']
        return ret

    def optimize(self, treesymtab, options = ('optimize','constfold','dcr',
                 'warntabs')):
        """Optimize the symbolic table symtab in place. Requires a table of
        predefined functions for folding constants.
        """
        if 'optimize' not in options:
            return treesymtab

        # Don't perform "a"+"b"  ->  "ab" unless explicitly requested.
        self.addstrings = 'addstrings' in options

        self.foldtabs = 'foldtabs' in options
        self.warntabs = 'warntabs' in options

        self.shrinknames = 'shrinknames' in options

        self.constfold = 'constfold' in options
        self.optlistlength = 'listlength' in options
        self.dcr = 'dcr' in options

        # Math that works fine except in rare corner-cases can be optimized.
        self.cornermath = 'cornermath' in options

        tree, symtab = self.tree, self.symtab = treesymtab

        self.globalmode = False

        if self.dcr:
            if self.constfold:
                self.FoldScript(warningpass=False)

            self.RemoveDeadCode()

        # Make another fold pass, since RemoveDeadCode can embed expressions
        # into other expressions and generate unoptimized code.
        # Or make the first pass here if DCR is disabled.
        if self.constfold:
            self.FoldScript(warningpass=True)

        if self.shrinknames:
            self.ShrinkNames()

        treesymtab = (self.tree, self.symtab)
        del self.tree
        del self.symtab
        return treesymtab
