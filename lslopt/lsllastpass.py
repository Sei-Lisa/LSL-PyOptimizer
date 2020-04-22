#    (C) Copyright 2015-2020 Sei Lisa. All rights reserved.
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

from lslopt import lslcommon
from lslopt.lslcommon import nr
#from lslcommon import Vector, Quaternion
#import lslfuncs
#from lslfuncs import ZERO_VECTOR, ZERO_ROTATION
#import math
#from lslparse import warning
#from lslfuncopt import OptimizeFunc, OptimizeArgs, FuncOptSetup

class rec:
    def __init__(self, **init):
        for i in init:
            setattr(self, i, init[i])

class lastpass(object):
    def visible(self, scope):
        return scope in self.scopeStack
    def LastPassPreOrder(self, parent, index):
        node = parent[index]
        nt = node.nt
        child = node.ch

        if (nt != '{}' and nt != ';' and nt != 'LAMBDA' and nt != '@'
            and not self.inExpr
           ):
            # Node that generates code.
            # EXPR counts as a single statement for JUMP purposes.
            self.prevStmt = self.lastStmt
            self.lastStmt = rec(node=node, parent=parent, index=index)
            # These are all the labels that label the current statement
            self.lastLabels = self.curLabels
            self.curLabels = set()

        if nt == 'EXPR':
            self.inExpr = True
            return

        if (self.optlistadd and not self.globalmode
            and (nt == 'CONST' and node.t == 'list' or nt == 'LIST'
                 or nt == '+' and child[0].t == 'list' and
                 (child[1].nt == 'CONST' and child[1].t == 'list'
                  or child[1].nt == 'LIST')
                )
           ):
                # Perform these transformations if the list is SEF:
                # [a, b, ...] -> (list)a + b...
                # ListExpr + [a, b, ..]  ->  ListExpr + a + b...
                # (ListExpr doesn't need to be SEF for this to work)

                # This transformation makes it difficult to handle lists during
                # optimization, that's why it's done in a separate pass.

                # Make listnode be the LIST or CONST element.
                listnode = child[1] if nt == '+' else node

                # left is the leftmost element in the addition.
                # left = None means the first element of the list must be
                # turned into a cast within the loop.
                left = child[0] if nt == '+' else None
                if listnode.SEF:
                    for elem in (listnode.value if listnode.nt == 'CONST'
                                 else listnode.ch):
                        elemnode = nr(nt='CONST',
                            t=lslcommon.PythonType2LSL[type(elem)],
                            value=elem, SEF=True
                            ) if listnode.nt == 'CONST' else elem
                        left = (self.Cast(elemnode, 'list') if left is None
                                else nr(nt='+', t='list', SEF=True,
                                    ch=[left, elemnode]))
                        del elemnode
                    if left is not None: # it's none for empty lists
                        parent[index] = left
                        # Do another pass on the result
                        self.RecursiveLastPass(parent, index)
                    return

                del listnode, left

        if nt == 'FNDEF':
            # StChAreBad will be True if this is a user-defined function,
            # where state changes are considered bad.
            # BadStCh will be True if at least one state change statement
            # is found while monitoring state changes.
            self.subinfo['StChAreBad'] = hasattr(node, 'scope')
            self.BadStCh = False
            return

        if nt == 'IF':
            if len(child) == 2:
                # Don't monitor the children.
                self.subinfo['StChAreBad'] = False
            return

        if nt == 'DO':
            self.subinfo['StChAreBad'] = False
            return

        if nt == 'FOR':
            self.subinfo['StChAreBad'] = False
            return

        if nt == 'WHILE':
            self.subinfo['StChAreBad'] = False
            return

        if nt == 'STSW':
            if self.subinfo['StChAreBad']:
                # Found one.
                self.BadStCh = True
            return

        if nt == 'FNCALL':
            # lslrenamer can benefit from a list of used library functions,
            # so provide it.
            if 'Loc' not in self.symtab[0][node.name]:
                # system library function
                self.usedlibfuncs.add(node.name)

        if nt == 'JUMP':
            if self.lastLabels:
                # This jump is labeled. The jumps that go to any of its labels
                # might be changeable to the destination of this jump, if the
                # scope of the destination is visible from those jumps, and the
                # jump eliminated.
                # TODO: We need the positions of these jumps for this to work.
                # We could perhaps change 'ref' in the symbol to a list instead
                # of a counter, pointing to the jumps.
                pass

        if nt == '@':
            self.curLabels.add(node)
            if self.lastStmt.node.nt == 'JUMP':
                jump = self.lastStmt.node
                if jump.scope == node.scope and jump.name == node.name:
                    # Remove the jump
                    self.lastStmt.parent[self.lastStmt.index] = nr(nt=';')
                    assert self.symtab[node.scope][node.name]['ref'] > 0
                    self.symtab[node.scope][node.name]['ref'] -= 1

        if nt == '{}':
            self.scopeStack.append(node.scope)

    def LastPassPostOrder(self, parent, index):
        node = parent[index]
        nt = node.nt
        child = node.ch

        if nt == 'FNDEF':
            if hasattr(node, 'scope') and self.BadStCh:
                # There is at least one bad state change statement in the
                # function (must be the result of optimization).
                # Insert dummy IF(1){...} statement covering the whole function
                # (if it returs a value, insert a return statement too).
                child[0] = nr(nt='{}', t=None, scope=len(self.symtab), ch=[
                    nr(nt='IF', t=None, ch=[
                        nr(nt='CONST', t='integer', value=1, SEF=True),
                        child[0]
                    ])
                ])
                self.symtab.append({})
                child = node.ch
                if node.t is not None:
                    # Inserting a state switch in a function that returns a
                    # value must count as one of the dumbest things to do.
                    # We do as best as we can: add a return statement with the
                    # default value for the type.
                    child[0].ch.append(nr(nt='RETURN', t=None, ch=[
                            nr(nt='CONST', t=node.t, SEF=True,
                               value=lslcommon.LSLTypeDefaults[node.t]
                            )]
                        ))
            del self.BadStCh
            return

        if nt == 'EXPR':
            self.inExpr = False

        if nt == '{}':
            self.scopeStack.pop()

        if nt == 'WHILE' or nt == 'DO' or nt == 'FOR':
            self.prevStmt = self.lastStmt
            self.lastStmt = rec(node=node, parent=parent, index=index)

    def RecursiveLastPass(self, parent, index):
        subinfo = self.subinfo
        self.subinfo = subinfo.copy()
        self.LastPassPreOrder(parent, index)

        if parent[index].ch is not None:
            child = parent[index].ch
            idx = 0
            while idx < len(child):
                self.RecursiveLastPass(child, idx)
                idx += 1

        self.LastPassPostOrder(parent, index)
        self.subinfo = subinfo

    def LastPass(self):
        """Last optimizations pass"""

        self.globalmode = False

        tree = self.tree

        self.usedlibfuncs = set()

        self.scopeStack = [0]
        self.curLabels = set()
        self.lastLabels = set()
        self.prevStmt = None
        self.lastStmt = None
        self.inExpr = False

        # self.subinfo is subtree-local info.
        self.subinfo = {}
        for idx in xrange(len(tree)):
            if tree[idx].nt == 'DECL':
                self.globalmode = True
                self.RecursiveLastPass(tree, idx)
                self.globalmode = False
            else:
                self.RecursiveLastPass(tree, idx)

        assert len(self.scopeStack) == 1 and self.scopeStack[0] == 0
        return {'libfuncs':self.usedlibfuncs}
