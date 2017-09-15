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

# Optimizations that have a negative effect on other stages.

import lslcommon
#from lslcommon import Vector, Quaternion
#import lslfuncs
#from lslfuncs import ZERO_VECTOR, ZERO_ROTATION
#import math
#from lslparse import warning
#from lslfuncopt import OptimizeFunc, OptimizeArgs, FuncOptSetup

class lastpass(object):
    def LastPassPreOrder(self, parent, index):
        node = parent[index]
        nt = node['nt']
        child = node['ch'] if 'ch' in node else None

        if (self.optlistadd and not self.globalmode
            and (nt == 'CONST' and node['t'] == 'list' or nt == 'LIST'
                 or nt == '+' and child[0]['t'] == 'list' and
                 (child[1]['nt'] == 'CONST' and child[1]['t'] == 'list'
                  or child[1]['nt'] == 'LIST')
                )
           ):
                # Perform these transformations if the list is SEF:
                # [a, b, ...] -> (list)a + b...
                # ListExpr + [a, b, ..]  ->  ListExpr + a + b...
                # (ListExpr doesn't need to be SEF for this to work)

                # This transformation makes it difficult to handle lists during
                # optimization, that's why it's done in a separate pass.
                top = child[0] if nt == '+' else None
                elemnode = child[1] if nt == '+' else node
                if 'SEF' in elemnode:
                    elements = (elemnode['value'] if elemnode['nt'] == 'CONST'
                                else elemnode['ch'])
                    for v in elements:
                        elem = v if elemnode['nt'] != 'CONST' else {
                            'nt':'CONST',
                            't':lslcommon.PythonType2LSL[type(v)],
                            'SEF':True,
                            'value':v}
                        top = {'nt':'CAST', 't':'list', 'SEF':True,
                               'ch':[elem]
                              } if top is None else {
                               'nt':'+', 't':'list', 'SEF':True,
                               'ch':[top, elem]
                              }
                        del elem
                    if top is not None:
                        parent[index] = top
                        nt = top['nt']
                        # Do another pass on the result
                        self.RecursiveLastPass(parent, index)
                    del top
                    return

    def LastPassPostOrder(self, parent, index):
        pass

    def RecursiveLastPass(self, parent, index):
        self.LastPassPreOrder(parent, index)

        if 'ch' in parent[index]:
            child = parent[index]['ch']
            idx = 0
            while idx < len(child):
                self.RecursiveLastPass(child, idx)
                idx += 1

        self.LastPassPostOrder(parent, index)

    def LastPass(self):
        self.globalmode = False

        tree = self.tree

        # Last optimizations pass
        for idx in xrange(len(tree)):
            if tree[idx]['nt'] == 'DECL':
                self.globalmode = True
                self.RecursiveLastPass(tree, idx)
                self.globalmode = False
            else:
                self.RecursiveLastPass(tree, idx)
    pass
