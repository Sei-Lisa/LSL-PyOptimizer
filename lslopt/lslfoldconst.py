#    (C) Copyright 2015-2018 Sei Lisa. All rights reserved.
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

# Constant folding and simplification of expressions and statements.

import lslcommon
from lslcommon import Vector, Quaternion, warning, nr
import lslfuncs
from lslfuncs import ZERO_VECTOR, ZERO_ROTATION
import math
from lslfuncopt import OptimizeFunc, OptimizeArgs, FuncOptSetup

# TODO: Remove special handling of @ within IF,WHILE,FOR,DO

class foldconst(object):

    def isLocalVar(self, node):
        name = node.name
        scope = node.scope
        return (self.symtab[scope][name]['Kind'] == 'v'
                and 'Loc' not in self.symtab[scope][name])

    def GetListNodeLength(self, node):
        """Get the length of a list that is expressed as a CONST, LIST or CAST
        node, or False if it can't be determined.
        """
        assert node.t == 'list'
        nt = node.nt
        if nt == 'CAST':
            if node.ch[0].t == 'list':
                return self.GetListNodeLength(node.ch[0])
            return 1
        if nt == 'CONST': # constant list
            return len(node.value)
        if nt == 'LIST': # list constructor
            return len(node.ch)
        return False

    def GetListNodeElement(self, node, index):
        """Get an element of a list expressed as a CONST, LIST or CAST node.
        If the index is out of range, return False; otherwise the result can be
        either a node or a constant.
        """
        assert node.t == 'list'
        nt = node.nt
        if nt == 'CAST':
            # (list)list_expr should have been handled in CAST
            assert node.ch[0].t != 'list'

            if index == 0 or index == -1:
                return node.ch[0]
            return False
        if nt == 'CONST':
            try:
                return node.value[index]
            except IndexError:
                pass
            return False
        if nt == 'LIST':
            try:
                return node.ch[index]
            except IndexError:
                return False
        return False

    def ConstFromNodeOrConst(self, nodeOrConst):
        """Return the constant if the value is a node and represents a constant,
        or if the value is directly a constant, and False otherwise.
        """
        if type(nodeOrConst) == nr:
            if nodeOrConst.nt == 'CONST':
                return nodeOrConst.value
            return False
        return nodeOrConst

    def TypeFromNodeOrConst(self, nodeOrConst):
        """Return the LSL type of a node or constant."""
        if nodeOrConst is False:
            return False
        if type(nodeOrConst) == nr:
            return nodeOrConst.t
        return lslcommon.PythonType2LSL[type(nodeOrConst)]

    def FoldAndRemoveEmptyStmts(self, lst):
        """Utility function for elimination of useless expressions in FOR"""
        idx = 0
        while idx < len(lst):
            self.FoldTree(lst, idx)
            self.FoldStmt(lst, idx)
            # If eliminated, it must be totally removed. A ';' won't do.
            if lst[idx].nt == ';':
                del lst[idx]
            else:
                idx += 1

    def CompareTrees(self, node1, node2):
        """Try to compare two subtrees to see if they are equivalent.

        Returns True if they are."""
        # They MUST be SEF and stable.
        if not node1.SEF or not node2.SEF:
            return False
        if node1.t != node2.t:
            return False
        # It's not complete yet.
        nt1 = node1.nt
        if nt1 == node2.nt:
            if (nt1 == 'IDENT'
                and node1.name == node2.name
                and node1.scope == node2.scope
               ):
                return True
            if (nt1 == 'FNCALL'
                and node1.name == node2.name
                and 'uns' not in self.symtab[0][node1.name]
                and all(self.CompareTrees(node1.ch[i],
                                          node2.ch[i])
                        for i in xrange(len(node1.ch)))
               ):
                return True
            if (nt1 == 'CAST'
                and self.CompareTrees(node1.ch[0], node2.ch[0])
               ):
                return True
            if nt1 == 'CONST' and node1.value == node2.value:
                return True
            if (nt1 in ('!', '~', 'NEG')
                and self.CompareTrees(node1.ch[0], node2.ch[0])
               ):
                return True
            if (nt1 in self.binary_ops
                and self.CompareTrees(node1.ch[0], node2.ch[0])
                and self.CompareTrees(node1.ch[1], node2.ch[1])
               ):
                return True
            if ((nt1 in ('*', '^', '&', '|', '==')  # commutative
                 or nt1 == '+'
                 and node1.ch[0].t not in ('list', 'string')
                 and node2.ch[0].t not in ('list', 'string')
                )
                and self.CompareTrees(node1.ch[0], node2.ch[1])
                and self.CompareTrees(node1.ch[1], node2.ch[0])
               ):
                return True
        return False

    def FnSEF(self, node):
        '''Applied to function call nodes, return whether the node corresponds
        to a SEF function.
        '''
        assert node.nt == 'FNCALL'
        sym = self.symtab[0][node.name]
        return 'SEF' in sym and sym['SEF'] is True

    def FoldStmt(self, parent, index):
        """Simplify a statement."""
        node = parent[index]
        # If the statement is side-effect-free, remove it as it does nothing.
        if node.SEF:
            # When a statement is side-effect free, it does nothing except
            # wasting CPU, and can thus be removed without affecting the
            # program. But side effect freedom is propagated from the
            # constituents of the statement, e.g. function calls in expressions
            # or substatements in FOR, or even individual variables.
            #
            # Many library functions like llSameGroup or llGetVel() are
            # side-effect free. Many other functions like llSleep() or
            # llSetScale() are not. User functions may or may not be.
            #
            # Assignments do have side effects, except those of the form x = x.
            # Pre- and post-increment and decrement also have side effects.
            # Other unary and binary operators are side effect-free.
            parent[index] = nr(nt=';', t=None, SEF=True)
            return
        if node.nt == 'EXPR':
            node = node.ch[0]
        # Post-increments take more space than pre-increments.
        if node.nt in ('V++', 'V--'):
            node.nt = '++V' if node.nt == 'V++' else '--V';

        # Function calls are SEF if both the function and the args are SEF.
        # If the statement is a function call and the function is marked as SEF
        # at this point, it means the arguments are not SEF. Replace the node
        # in that case with a block of expressions.
        if (node.nt == 'FNCALL' and 'Loc' in self.symtab[0][node.name]
            and self.FnSEF(node)
           ):
            parent[index] = nr(nt='{}', t=None, ch=[
                nr(nt='EXPR', t=x.t, ch=[x]) for x in node.ch])
            self.FoldTree(parent, index)
            return

    def ExpandCondition(self, parent, index):
        """IF, FOR, WHILE and DO...WHILE conditions accept several types, not
        just integer. However, leaving them as-is generates longer code than if
        we expand them and let the optimizer optimize, for float, vector and
        rotation, and no matter the optimization in the case of list.
        """
        ctyp = parent[index].t
        # Under LSO, this would break the fact that 1-element lists count as
        # false, so we don't do it for LSO lists.
        if (ctyp in ('float', 'vector', 'rotation', 'string')
            or ctyp == 'list' and not lslcommon.LSO
           ):
            parent[index] = nr(nt='!=', t='integer', ch=[parent[index],
                nr(nt='CONST', t=ctyp, value=0.0 if ctyp == 'float'
                    else ZERO_VECTOR if ctyp == 'vector'
                    else ZERO_ROTATION if ctyp == 'rotation'
                    else u"" if ctyp == 'string'
                    else [])])
            parent[index].SEF = parent[index].ch[0].SEF

    def IsBool(self, node):
        """Some operators return 0 or 1, and that allows simplification of
        boolean expressions. This function returns whether we know for sure
        that the result is boolean.
        """
        nt = node.nt
        if (nt in ('<', '!', '>', '<=', '>=', '==', '||', '&&')
            or nt == '!=' and node.ch[0].t != 'list'
            or nt == '&' and any(self.IsBool(node.ch[i]) for i in (0, 1))
            or nt in ('|', '^', '*')
               and all(self.IsBool(node.ch[i]) for i in (0, 1))
            or nt == 'CONST' and node.t == 'integer' and node.value in (0, 1)
           ):
            return True

        if nt == 'FNCALL':
            sym = self.symtab[0][node.name]
            if (sym['Type'] == 'integer' and 'min' in sym and 'max' in sym
                and sym['min'] >= 0 and sym['max'] <= 1
               ):
                return True

        return False

    def IsAndBool(self, node):
        """For bitwise AND, in some cases we can relax the condition to this:
        when bit 0 is 0, all other bits are guaranteed to be 0 as well. That's
        the case of -bool, which is the only case we deal with here, but an
        important one because we generate it as an intermediate result in some
        operations.
        """
        return (node.nt == 'NEG' and self.IsBool(node.ch[0])
                or self.IsBool(node))

    def FoldCond(self, parent, index, ParentIsNegation = False):
        """When we know that the parent is interested only in the truth value
        of the node, we can perform further optimizations. This function deals
        with them.
        """
        node = parent[index]
        nt = node.nt
        if nt in ('CONST', 'IDENT', 'FLD'):
            if node.nt == 'CONST':
                node.t = 'integer'
                node.value = 1 if lslfuncs.cond(node.value) else 0
            return # Nothing to do if it's already simplified.
        child = node.ch

        if nt == 'FNCALL' and 'strlen' in self.symtab[0][node.name]:
            # llStringLength(expr)  ->  !(expr == "")
            # new node is SEF if the argument to llStringLength is
            node = nr(nt='==', t='integer', SEF=child[0].SEF,
                ch=[child[0],
                    nr(nt='CONST', t='string', value=u'', SEF=True)
                ])
            node = nr(nt='!', t='integer', ch=[node], SEF=child[0].SEF)
            parent[index] = node
            nt = '!'
            child = node.ch
            # fall through to keep optimizing if necessary

        if nt == '!':
            self.FoldCond(child, 0, True)

            if child[0].nt == '!':
                # bool(!!a) equals bool(a)
                parent[index] = child[0].ch[0]
                return

            if (child[0].nt == '==' and child[0].ch[0].t == 'integer'
                and child[0].ch[1].t == 'integer'
               ):
                # We have !(int == int). Replace with int ^ int or with int - 1
                node = parent[index] = child[0]  # remove the negation
                child = child[0].ch
                if (child[0].nt == 'CONST' and child[0].value == 1
                    or child[1].nt == 'CONST' and child[1].value == 1
                   ):
                    # a != 1  ->  a - 1  (which FoldTree will transform to ~-a)
                    node.nt = '-'
                else:
                    # This converts != to ^; FoldTree will simplify ^-1 to ~
                    # and optimize out ^0.
                    node.nt = '^'
                self.FoldTree(parent, index)
                return

            if (child[0].nt == '&'
                and any(child[0].ch[i].nt == '!'
                        and self.IsAndBool(child[0].ch[1-i]) for i in (0, 1))
               ):
                # We can remove at least one !
                child[0].nt = '|'
                for i in (0, 1):
                    child[0].ch[i] = nr(nt='!', t='integer',
                        ch=[child[0].ch[i]], SEF=child[0].ch[i].SEF)
                parent[index] = child[0]
                self.FoldTree(parent, index)
                self.FoldCond(parent, index)
                return

        if nt == 'NEG':
            # bool(-a) equals bool(a)
            parent[index] = child[0]
            self.FoldCond(parent, index, ParentIsNegation)
            return

        if nt in self.binary_ops and child[0].t == child[1].t == 'integer':
            if nt == '==':
                if (child[0].nt == 'CONST' and -1 <= child[0].value <= 1
                    or child[1].nt == 'CONST' and -1 <= child[1].value <= 1
                   ):
                    # Transform a==b into !(a-b) if either a or b are in [-1,1]
                    parent[index] = nr(nt='!', t='integer', ch=[node])
                    node.nt = '-'
                    self.FoldTree(parent, index)
                return

            if nt == '|':
                # In a boolean context, the operands count as booleans.
                self.FoldCond(child, 0)
                self.FoldCond(child, 1)

                # Deal with operands in any order
                a, b = 0, 1
                # Put constant in child[b] if present
                if child[b].nt != 'CONST':
                    a, b = 1, 0
                if child[b].nt == 'CONST' and child[b].value and child[a].SEF:
                    node = parent[index] = child[b]
                    node.value = -1
                    return
                del a, b

                # Specific optimization to catch a frequent bitwise test.
                # If b and c are constant powers of two:
                #   !(a & b) | !(a & c)  ->  ~(a|~(b|c))
                # e.g. if (a & 4  &&  a & 8)  ->  if (!~(a|-13))
                if (child[0].nt == '!' and child[0].ch[0].nt == '&'
                    and child[1].nt == '!' and child[1].ch[0].nt == '&'
                   ):
                    and1 = child[0].ch[0].ch
                    and2 = child[1].ch[0].ch
                    a, b, c, d = 0, 1, 0, 1
                    if and1[b].nt != 'CONST':
                        a, b = b, a
                    if and2[d].nt != 'CONST':
                        c, d = d, c
                    if and1[b].nt == and2[d].nt == 'CONST':
                        val1 = and1[b].value
                        val2 = and2[d].value
                        if (val1 and val2
                            # power of 2
                            and (val1 & (val1 - 1) & 0xFFFFFFFF) == 0
                            and (val2 & (val2 - 1) & 0xFFFFFFFF) == 0
                            and self.CompareTrees(and1[a], and2[c])
                           ):
                            # Check passed
                            child[0] = and1[a]
                            child[1] = and1[b]
                            child[1].value = ~(val1 | val2)
                            parent[index] = nr(nt='~', t='integer', ch=[node],
                                SEF=node.SEF)
                            self.FoldCond(parent, index, ParentIsNegation)
                            return
                        del val1, val2
                    del a, b, c, d, and1, and2

                # Absorb further flags, to allow chaining of &&
                # If ~r and s are constants, and s is a power of two:
                #   (!~(x|~r) && x&s)  ->  !~(x|(~r&~s))
                # This is implemented as:
                #   ~(x|~r) | !(x&s)  ->  ~(x|~(r|s))
                # since that's the intermediate result after conversion of &&.
                # a and b are going to be the children of the main |
                # a is going to be child that has the ~
                # b is the other child (with the !)
                # c is the child of ~ which has x
                # d is the child of ~ with the constant ~r
                # e is the child of ! which has x
                # f is the child of ! with the constant s
                a, b = 0, 1
                if child[a].nt != '~':
                   a, b = b, a
                c, d = 0, 1
                if child[a].nt == '~' and child[a].ch[0].nt == '|':
                    if child[a].ch[0].ch[d].nt != 'CONST':
                        c, d = d, c
                e, f = 0, 1
                if child[b].nt == '!' and child[b].ch[0].nt == '&':
                    if child[b].ch[0].ch[f].nt != 'CONST':
                        e, f = f, e
                # All pointers are ready to check applicability.
                if (child[a].nt == '~' and child[a].ch[0].nt == '|'
                    and child[b].nt == '!' and child[b].ch[0].nt == '&'
                   ):
                    ch1 = child[a].ch[0].ch
                    ch2 = child[b].ch[0].ch
                    if (ch1[d].nt == 'CONST' and ch2[f].nt == 'CONST'
                        and (ch2[f].value & (ch2[f].value - 1)
                             & 0xFFFFFFFF) == 0
                       ):
                        if self.CompareTrees(ch1[c], ch2[e]):
                            # We're in that case. Apply optimization.
                            parent[index] = child[a]
                            ch1[d].value &= ~ch2[f].value
                            return
                    del ch1, ch2

                del a, b, c, d, e, f


                # Check if the operands are a negation ('!') or can be inverted
                # without adding more than 1 byte and are boolean.
                # We only support '<' and some cases of '&' (are there more?)
                Invertible = [False, False]
                for a in (0, 1):
                    Invertible[a] = child[a].nt == '!'
                    if (child[a].nt == '<'
                        and child[a].ch[0].t == child[a].ch[1].t == 'integer'
                       ):
                        if (child[a].ch[0].nt == 'CONST'
                              and child[a].ch[0].value != 2147483647
                           or child[a].ch[1].nt == 'CONST'
                              and child[a].ch[1].value != int(-2147483648)
                           ):
                            Invertible[a] = True

                    # Deal with our optimization of a<0 -> a&0x80000000
                    # (see below)
                    if child[a].nt == '&' and (
                          child[a].ch[0].nt == 'CONST'
                          and child[a].ch[0].value == int(-2147483648)
                       or child[a].ch[1].nt == 'CONST'
                          and child[a].ch[1].value == int(-2147483648)
                       ):
                        Invertible[a] |= ParentIsNegation

                if (Invertible[0] or Invertible[1]) and ParentIsNegation:
                    # !(!a|b)  ->  a&-!b or a&!b
                    # This deals with the part after the first !, transforming
                    # it into (!a|!!b) so that the outer node can optimize the
                    # negated version to a simple &.
                    for a in (0, 1):
                        if not Invertible[a]:
                            child[a] = nr(nt='!', t='integer',
                                ch=[nr(nt='!', t='integer', ch=[child[a]])]
                            )
                            Invertible[a] = True

                if Invertible[0] and Invertible[1]:
                    # Both operands are negated, or negable.
                    # Make them a negation if they aren't already.
                    for a in (0, 1):
                        if child[a].nt == '<':
                            if child[a].ch[0].nt == 'CONST':
                                child[a].ch[0].value += 1
                            else:
                                child[a].ch[1].value -= 1
                            child[a].ch[0], child[a].ch[1] = \
                                child[a].ch[1], child[a].ch[0]
                            child[a] = nr(nt='!', t='integer', ch=[child[a]])
                        elif child[a].nt == '&':
                            child[a] = nr(nt='!', t='integer',
                                ch=[nr(nt='!', t='integer', ch=[child[a]])]
                            )
                            self.FoldTree(child[a].ch, 0)
                    # If they are boolean, the expression can be turned into
                    # !(a&b) which hopefully will have a ! uptree if it came
                    # from a '&&' and cancel out (if not, we still remove one
                    # ! so it's good). If one is bool, another transformation
                    # can be performed: !nonbool|!bool -> !(nonbool&-bool)
                    # which is still a gain.

                    # Deal with operands in any order
                    a, b = 0, 1
                    # Put the bool in child[b].ch[0].
                    if not self.IsBool(child[b].ch[0]):
                       a, b = 1, 0
                    if self.IsBool(child[b].ch[0]):
                        if not self.IsAndBool(child[a].ch[0]):
                            child[b].ch[0] = nr(nt='NEG', t='integer',
                                ch=[child[b].ch[0]])

                        node = parent[index] = nr(nt='!', t='integer',
                            ch=[nr(nt='&', t='integer',
                                   ch=[child[0].ch[0], child[1].ch[0]])
                            ], SEF=child[0].ch[0].SEF and child[1].ch[0].SEF)
                        # Fold the node we've just synthesized
                        self.FoldTree(parent, index)

                return

            if nt == '<' and child[0].t == child[1].t == 'integer':
                sym = None
                for a in (0, 1):
                    if child[a].nt == 'FNCALL':
                        sym = self.symtab[0][child[a].name]
                        break

                # cond(FNCALL < 0)  ->  cond(~FNCALL) if min == -1
                if (child[1].nt == 'CONST' and child[1].value == 0
                    and child[0].nt == 'FNCALL'
                    and 'min' in sym and sym['min'] == -1
                   ):
                    node = parent[index] = nr(nt='~', t='integer',
                        ch=[child[0]])
                    self.FoldTree(parent, index)
                    return

                # cond(FNCALL > -1)  ->  cond(!~FNCALL) if min == -1
                if (child[0].nt == 'CONST' and child[0].value == -1
                    and child[1].nt == 'FNCALL'
                    and 'min' in sym and sym['min'] == -1
                   ):
                    node = parent[index] = nr(nt='!', t='integer',
                        ch=[nr(nt='~', t='integer', ch=[child[1]])])
                    self.FoldTree(parent, index)
                    return

                # cond(FNCALL < 1)  ->  cond(!FNCALL) if min == 0
                if (child[1].nt == 'CONST' and child[1].value == 1
                    and child[0].nt == 'FNCALL'
                    and 'min' in sym and sym['min'] == 0
                   ):
                    node = parent[index] = nr(nt='!', t='integer',
                        ch=[child[0]])
                    self.FoldTree(parent, index)
                    return

                # cond(FNCALL > 0)  ->  cond(FNCALL) if min == 0
                if (child[0].nt == 'CONST' and child[0].value == 0
                    and child[1].nt == 'FNCALL'
                    and 'min' in sym and sym['min'] == 0
                   ):
                    node = parent[index] = child[1]
                    self.FoldTree(parent, index)
                    return

            if nt == '&':

                # Deal with operands in any order
                a, b = 0, 1
                # Put constant in child[b], if present
                if child[b].nt != 'CONST':
                    a, b = 1, 0
                if (child[b].nt == 'CONST'
                    and child[b].value == int(-2147483648)
                    and child[a].nt == 'FNCALL'
                   ):
                    sym = self.symtab[0][child[a].name]
                    if 'min' in sym and sym['min'] == -1:
                        node = parent[index] = nr(nt='~', t='integer',
                            ch=[child[a]])
                        self.FoldTree(parent, index)
                return

    def CopyNode(self, node):
        '''Deep copy of a node'''
        ret = node.copy()
        if ret.ch:
            ret.ch = [self.CopyNode(subnode) for subnode in ret.ch]
        return ret

    def FoldTree(self, parent, index):
        """Recursively traverse the tree to fold constants, changing it in
        place.

        Also optimizes away IF, WHILE, etc.
        """
        node = parent[index]
        nt = node.nt
        child = node.ch

        if nt == 'CONST':
            # Job already done. But mark as side-effect free.
            node.SEF = True
            return

        if nt == 'CAST':
            self.FoldTree(child, 0)
            node.SEF = child[0].SEF
            if child[0].nt == 'CONST':
                # Enable key constants. We'll typecast them back on output, but
                # this enables some optimizations.
                #if node.t != 'key': # key constants not possible

                    parent[index] = nr(nt='CONST', t=node.t, SEF=True,
                        value=lslfuncs.typecast(
                            child[0].value, lslcommon.LSLType2Python[node.t]))

            # Remove casts of a type to the same type (NOP in Mono)
            # This is not an optimization by itself, but it simplifies the job,
            # by not needing to look into nested casts like (key)((key)...)
            while node.nt == 'CAST' and child[0].t == node.t:
                parent[index] = node = child[0]
                if node.ch is None:
                    break
                child = node.ch

            return

        if nt == 'NEG':
            self.FoldTree(child, 0)
            node.SEF = child[0].SEF

            if child[0].nt == '+' and any(child[0].ch[i].nt == 'NEG'
                                          for i in (0, 1)):
                node = parent[index] = child[0]
                child = node.ch
                for a in (0, 1):
                    if child[a].nt == 'NEG':
                        child[a] = child[a].ch[0]
                    else:
                        child[a] = nr(nt='NEG', t=child[a].t, ch=[child[a]],
                            SEF=child[a].SEF)
                        self.FoldTree(child, a)
                return

            if child[0].nt == 'NEG':
                # Double negation: - - expr  ->  expr
                node = parent[index] = child[0].ch[0]
                child = node.ch
            elif child[0].nt == 'CONST':
                node = parent[index] = child[0]
                node.value = lslfuncs.neg(node.value)
                child = None

            if child and node.nt == 'NEG' and child[0].nt == '~':
                track = child[0].ch[0]
                const = 1
                while track.nt == 'NEG' and track.ch[0].nt == '~':
                    const += 1
                    track = track.ch[0].ch[0]
                if const > 2:
                    # -~-~-~expr  ->  expr+3
                    node = nr(nt='CONST', t='integer', SEF=True, value=const)
                    node = nr(nt='+', t='integer', ch=[node, track],
                        SEF=track.SEF)
                    parent[index] = node

            return

        if nt == '!':
            self.FoldTree(child, 0)
            self.FoldCond(child, 0, True)
            # !! does *not* cancel out (unless in cond)
            subexpr = child[0]
            snt = subexpr.nt

            node.SEF = subexpr.SEF
            if snt == 'CONST':
                node = parent[index] = subexpr
                node.value = int(not node.value)
                return
            if snt == '<':
                lop = subexpr.ch[0]
                rop = subexpr.ch[1]
                if (lop.nt == 'CONST' and lop.t == rop.t == 'integer'
                    and lop.value < 2147483647
                   ):
                    lop.value += 1
                    subexpr.ch[0], subexpr.ch[1] = subexpr.ch[1], subexpr.ch[0]
                    parent[index] = subexpr # remove the !
                    return
                if (rop.nt == 'CONST' and lop.t == rop.t == 'integer'
                    and rop.value > int(-2147483648)
                   ):
                    rop.value -= 1
                    subexpr.ch[0], subexpr.ch[1] = subexpr.ch[1], subexpr.ch[0]
                    parent[index] = subexpr # remove the !
                    return
            if snt == '&':
                a, b = 0, 1
                if subexpr.ch[b].nt != 'CONST':
                    a, b = 1, 0
                if (subexpr.ch[b].nt == 'CONST'
                    and subexpr.ch[b].value == int(-2147483648)
                   ):
                    # !(i & 0x80000000)  ->  -1 < i (because one of our
                    # optimizations can be counter-productive, see FoldCond)
                    subexpr.nt = '<'
                    subexpr.ch[b].value = -1
                    subexpr.ch = [subexpr.ch[b], subexpr.ch[a]]
                    parent[index] = subexpr
                    return
            if snt == '!=' or snt == '^' or snt == '-' or snt == '+':
                if snt == '+':
                    # Change !(x + y) -> -x == y, and make another pass
                    # to get rid of the signs where possible
                    subexpr.ch[0] = nr(nt='NEG', t='integer',
                        ch=[subexpr.ch[0]], SEF=subexpr.ch[0].SEF)

                subexpr.nt = '=='
                parent[index] = subexpr
                self.FoldTree(parent, index)
                return

            return

        if nt == '~':
            self.FoldTree(child, 0)
            subexpr = child[0]
            node.SEF = subexpr.SEF

            if child[0].nt == 'NEG':
                track = child[0].ch[0]
                const = -1
                while track.nt == '~' and track.ch[0].nt == 'NEG':
                    const -= 1
                    track = track.ch[0].ch[0]
                if const < -2:
                    # ~-~-~-expr  ->  expr + (-3)
                    node = nr(nt='CONST', t='integer', SEF=True, value=const)
                    node = nr(nt='+', t='integer', ch=[node, track],
                        SEF=track.SEF)
                    parent[index] = node
                    self.FoldTree(parent, index)
                    return

            if subexpr.nt == '~':
                # Double negation: ~~expr
                parent[index] = subexpr.ch[0]
            elif subexpr.nt == 'CONST':
                node = parent[index] = child[0]
                node.value = ~node.value
            return

        if nt in self.binary_ops:
            # RTL evaluation
            self.FoldTree(child, 1)
            self.FoldTree(child, 0)
            # Node is SEF if both sides are side-effect free.
            node.SEF = child[0].SEF and child[1].SEF

            optype = node.t
            lval = child[0]
            ltype = lval.t
            lnt = lval.nt
            rval = child[1]
            rtype = rval.t
            rnt = rval.nt

            if lnt == rnt == 'CONST':
                op1 = lval.value
                op2 = rval.value
                if nt == '+':
                    if ltype == rtype == 'string' and not self.addstrings:
                        return
                    result = lslfuncs.add(op1, op2)
                elif nt == '-':
                    result = lslfuncs.sub(op1, op2)
                elif nt == '*':
                    result = lslfuncs.mul(op1, op2)
                elif nt == '/':
                    try:
                        result = lslfuncs.div(op1, op2)
                    except lslfuncs.ELSLMathError:
                        return
                elif nt == '%':
                    try:
                        result = lslfuncs.mod(op1, op2)
                    except lslfuncs.ELSLMathError:
                        return
                elif nt == '<<':
                    result = lslfuncs.S32(op1 << (op2 & 31))
                elif nt == '>>':
                    result = lslfuncs.S32(op1 >> (op2 & 31))
                elif nt == '==' or nt == '!=':
                    result = lslfuncs.compare(op1, op2, Eq = (nt == '=='))
                elif nt in ('<', '<=', '>', '>='):
                    if nt in ('>', '<='):
                        result = lslfuncs.less(op2, op1)
                    else:
                        result = lslfuncs.less(op1, op2)
                    if nt in ('>=', '<='):
                        result = 1 - result
                elif nt == '|':
                    result = op1 | op2
                elif nt == '^':
                    result = op1 ^ op2
                elif nt == '&':
                    result = op1 & op2
                elif nt == '||':
                    result = int(bool(op1) or bool(op2))
                elif nt == '&&':
                    result = int(bool(op1) and bool(op2))
                else:
                    assert False, 'Internal error: Operator not found: ' + nt # pragma: no cover
                parent[index] = nr(nt='CONST', t=node.t, SEF=True, value=result)
                return

            # Simplifications for particular operands
            if nt == '-':
                if optype in ('vector', 'rotation'):
                    if lnt == 'CONST' and all(component == 0
                            for component in lval.value):
                        # Change <0,0,0[,0]>-expr  ->  -expr
                        parent[index] = nr(nt='NEG', t=node.t, ch=[rval],
                            SEF=rval.SEF)
                    elif rnt == 'CONST' and all(component == 0
                            for component in rval.value):
                        # Change expr-<0,0,0[,0]>  ->  expr
                        parent[index] = lval
                    return

                # Change - to + - for int/float
                nt = node.nt = '+'
                if child[1].nt == 'CONST':
                    rval.value = lslfuncs.neg(rval.value)
                else:
                    rnt = 'NEG'
                    rval = child[1] = nr(nt=rnt, t=rval.t, ch=[rval],
                        SEF=rval.SEF)
                    self.FoldTree(child, 1)
                    # rtype unchanged

                # Fall through to simplify it as '+'

            if nt == '+':
                # Tough one. Remove neutral elements for the various types,
                # and more.

                # expr + -expr  ->  0
                # -expr + expr  ->  0
                if (child[0].nt == 'NEG'
                    and self.CompareTrees(child[0].ch[0], child[1])
                    or child[1].nt == 'NEG'
                    and self.CompareTrees(child[1].ch[0], child[0])
                   ):
                    parent[index] = nr(nt='CONST', t='integer', value=0,
                        SEF=True)
                    return

                # Addition of integers, strings, and lists is associative.
                # Addition of floats, vectors and rotations would be, except
                # for FP precision.
                # TODO: associative addition of lists
                # Associative lists are trickier, because unlike the others,
                # the types of the operands may not be lists
                # so e.g. list+(integer+integer) != (list+integer)+integer.
                if optype == 'integer' or optype == 'string' and self.addstrings:
                    if lnt == '+' and rnt == 'CONST' and lval.ch[1].nt == 'CONST':
                        # (var + ct1) + ct2  ->  var + (ct1 + ct2)
                        child[1] = nr(nt='+', t=optype, ch=[lval.ch[1], rval],
                            SEF=True)
                        lval = child[0] = lval.ch[0]
                        lnt = lval.nt
                        ltype = lval.t
                        rtype = optype
                        # Fold the RHS again now that we have it constant
                        self.FoldTree(child, 1)
                        rval = child[1]
                        rnt = rval.nt

                if optype == 'list' and not (ltype == rtype == 'list'):
                    if lnt == 'CONST' and not lval.value:
                        # [] + nonlist  ->  (list)nonlist
                        parent[index] = self.Cast(rval, optype)
                        # node is SEF if rval is
                        parent[index].SEF = rval.SEF
                    return

                if optype in ('vector', 'rotation'):
                    # not much to do with vectors or quaternions either
                    if lnt == 'CONST' and all(x == 0 for x in lval.value):
                        # Change <0,0,0[,0]>+expr  ->  expr
                        parent[index] = rval
                    elif rnt == 'CONST' and all(x == 0 for x in rval.value):
                        # Change expr+<0,0,0[,0]>  ->  expr
                        parent[index] = lval
                    return

                # Can't be key, as no combo of addition operands returns key
                assert optype != 'key'

                if optype in ('string', 'float', 'list'):
                    # All these types evaluate to boolean False when they are
                    # the neutral addition element.
                    if lnt == 'CONST' and not lval.value:
                        # 0. + expr  ->  expr
                        # "" + expr  ->  expr
                        # [] + expr  ->  expr
                        parent[index] = self.Cast(rval, optype)
                        # node is SEF if rval is
                        parent[index].SEF = rval.SEF
                        return
                    if rnt == 'CONST' and not rval.value:
                        # expr + 0.  ->  expr
                        # expr + ""  ->  expr
                        # expr + []  ->  expr
                        parent[index] = self.Cast(lval, optype)
                        # node is SEF if lval is
                        parent[index].SEF = lval.SEF
                        return

                    if ltype == rtype == 'list':

                        if (rnt == 'LIST' and len(rval.ch) == 1
                            or rnt == 'CONST' and len(rval.value) == 1
                            or rnt == 'CAST'
                           ):
                            # list + (list)element  ->  list + element
                            # list + [element]  ->  list + element
                            while rnt == 'CAST' and rval.t == 'list':
                                # Remove nested typecasts
                                # e.g. list + (list)((list)x)  ->  list + x
                                rval = parent[index].ch[1] = rval.ch[0]
                                rnt = rval.nt
                            if (rnt == 'LIST' and len(rval.ch) == 1
                               and rval.ch[0].t != 'list'):
                                # Finally, remove [] wrapper if it's not
                                # list within list
                                rval = child[1] = rval.ch[0]
                                rnt = rval.nt
                            if rnt == 'CONST' and len(rval.value) == 1:
                                # list + [constant]  ->  list + constant
                                rval.value = rval.value[0]
                                rtype = rval.t = lslcommon.PythonType2LSL[
                                    type(rval.value)]
                            return

                        if (lnt == 'LIST' and len(lval.ch) == 1
                            or lnt == 'CONST' and len(lval.value) == 1
                            or lnt == 'CAST'
                           ):
                            # (list)element + list  ->  element + list
                            # [element] + list  ->  element + list
                            # (list)[element] + list  ->  element + list
                            while lnt == 'CAST' and lval.t == 'list':
                                # Remove nested typecasts
                                # e.g. (list)((list)x) + list  ->  x + list
                                lval = parent[index].ch[0] = lval.ch[0]
                                lnt = lval.nt
                            if (lnt == 'LIST' and len(lval.ch) == 1
                               and lval.ch[0].t != 'list'):
                                # Finally, remove [] wrapper if it's not
                                # list within list
                                lval = child[0] = lval.ch[0]
                                lnt = lval.nt
                            if lnt == 'CONST' and len(lval.value) == 1:
                                # [constant] + list  ->  constant + list
                                lval.value = lval.value[0]
                                ltype = lval.t = lslcommon.PythonType2LSL[
                                    type(lval.value)]
                            return

                    if optype == 'float' and rnt == 'CONST':
                        # Addition of floats is commutative.
                        # Put the constant first. May reduce stack.
                        lval, rval = child[0], child[1] = child[1], child[0]
                        lnt, rnt = rnt, lnt
                        ltype, rtype = rtype, ltype

                    if (self.addstrings and optype == 'string' and rnt == '+'
                        and rval.ch[0].nt == 'CONST' and lnt == 'CONST'
                       ):
                        # We have CONST + (CONST + expr) of strings.
                        # Apply associativity to merge both constants.

                        # Add the constants
                        child[0].value = lslfuncs.add(child[0].value,
                            rval.ch[0].value)
                        # Prune the expr and graft it as RHS
                        child[1] = rval.ch[1]
                        # Re-optimize this node to apply it recursively
                        return self.FoldTree(parent, index)

                    # Nothing else to do with addition of float, string or list
                    return

                # Must be two integers. This allows for a number of
                # optimizations. First the most obvious ones.
                assert optype == 'integer'  # just to make sure

                # Commutativity: place the constant first; may save stack and
                # it helps simplifying
                if rnt == 'CONST':
                    lval, rval = child[0], child[1] = child[1], child[0]
                    lnt, rnt = rnt, lnt
                    ltype, rtype = rtype, ltype

                if lnt == 'CONST' and lval.value == 0:
                    # 0 + x = x
                    parent[index] = rval
                    return

                if lnt == 'CONST' and rnt == '+' and rval.ch[0].nt == 'CONST':
                    # We have CONST + (CONST + expr)
                    # Apply associativity to merge both constants.

                    # Add the constants
                    lval.value = lslfuncs.add(lval.value, rval.ch[0].value)
                    # Prune the expr and graft it as RHS
                    child[1] = rval.ch[1]

                    # Re-optimize the result, to possibly apply -~ or ~- if
                    # appropriate.
                    return self.FoldTree(parent, index)

                while lnt == 'CONST' and rnt == 'NEG' and rval.ch[0].nt == '~':
                    lval.value += 1
                    child[1] = rval.ch[0].ch[0]
                    # rtype doesn't change
                    assert child[1].t == 'integer'
                    self.FoldTree(parent, index)
                    node = parent[index]
                    nt, child = node.nt, node.ch
                    if nt != '+':
                        return
                    lval, rval = child[0], child[1]
                    lnt, rnt = lval.nt, rval.nt
                    ltype, rtype = lval.t, rval.t

                while lnt == 'CONST' and rnt == '~' and rval.ch[0].nt == 'NEG':
                    lval.value -= 1
                    child[1] = rval.ch[0].ch[0]
                    # rtype doesn't change
                    assert child[1].t == 'integer'
                    self.FoldTree(parent, index)
                    node = parent[index]
                    nt, child = node.nt, node.ch
                    if nt != '+':
                        return
                    lval, rval = child[0], child[1]
                    lnt, rnt = lval.nt, rval.nt
                    ltype, rtype = lval.t, rval.t

                if lnt != 'CONST':
                    # Neither is const.
                    # The case expr - expr  ->  0 has been handled earlier
                    # because it's more general and applies to floats as well.

                    # -expr + -expr  ->  -(expr + expr) (saves 1 byte)
                    if lnt == rnt == 'NEG':
                        node = nr(nt='+', t=optype, ch=[lval.ch[0], rval.ch[0]],
                            SEF=lval.ch[0].SEF and rval.ch[0].SEF)
                        node = nr(nt='NEG', t=optype, ch=[node], SEF=node.SEF)
                        parent[index] = node
                        return

                    return

                RSEF = rval.SEF

                if lval.value == -1 or lval.value == -2:
                    if rnt == 'NEG': # Cancel the NEG
                        node = nr(nt='~', t=optype, ch=rval.ch, SEF=RSEF)
                    else: # Add the NEG
                        node = nr(nt='NEG', t=optype, ch=[rval], SEF=RSEF)
                        node = nr(nt='~', t=optype, ch=[node], SEF=RSEF)
                    if lval.value == -2:
                        node = nr(nt='NEG', t=optype, ch=[node], SEF=RSEF)
                        node = nr(nt='~', t=optype, ch=[node], SEF=RSEF)
                    parent[index] = node
                    return

                if lval.value == 1 or lval.value == 2:
                    if rnt == '~': # Cancel the ~
                        node = nr(nt='NEG', t=optype, ch=rval.ch, SEF=RSEF)
                    else:
                        node = nr(nt='~', t=optype, ch=[rval], SEF=RSEF)
                        node = nr(nt='NEG', t=optype, ch=[node], SEF=RSEF)
                    if lval.value == 2:
                        node = nr(nt='~', t=optype, ch=[node], SEF=RSEF)
                        node = nr(nt='NEG', t=optype, ch=[node], SEF=RSEF)
                    parent[index] = node
                    return

                # More than 2 becomes counter-productive.

                return

            if nt == '<<' and child[1].nt == 'CONST':
                # Transforming << into multiply saves some bytes.
                if child[1].value & 31:
                    # x << 3  -->  x * 8

                    # we have {<<, something, {CONST n}}
                    # we transform it into {*, something, {CONST n}}
                    nt = node.nt = '*'
                    child[1].value = lslfuncs.S32(1 << (child[1].value & 31))

                    # Fall through to optimize product

                else: # x << 0  -->  x
                    parent[index] = child[0]
                    return

            if (nt == '%' and child[1].nt == 'CONST'
                          and child[1].t == 'integer'
                          and abs(child[1].value) == 1):
                # a%1  ->  a&0
                # a%-1  ->  a&0
                # (SEF analysis performed below)
                nt = node.nt = '&'
                child[1].value = 0
                self.FoldTree(parent, index)
                return

            if nt in ('*', '/'):
                # Extract signs outside
                if child[0].nt == 'NEG' or child[1].nt == 'NEG':
                    a, b = 0, 1
                    if child[b].nt == 'NEG':
                        a, b = 1, 0
                    child[a] = child[a].ch[0]
                    parent[index] = node = nr(nt='NEG', t=node.t, ch=[node],
                        SEF = node.SEF)
                    # Fold the new expression
                    self.FoldTree(parent, index)
                    return

                # Deal with operands in any order
                a, b = 0, 1
                if child[a].nt == 'CONST' and child[a].t in ('float', 'integer'):
                    a, b = 1, 0

                if child[b].nt == 'CONST':
                    val = child[b].value

                    # Optimize out signs if possible.
                    # Note that (-intvar)*floatconst needs cornermath because
                    # -intvar could equal intvar if intvar = -2147483648,
                    # so the sign is a no-op and pushing it to floatconst would
                    # make the result be different.
                    if (child[a].nt == 'NEG'
                        and (self.cornermath
                            or child[a].t != 'integer'
                            or child[b].t != 'float')
                       ):
                        # Expression is of the form (-float)*const or (-float)/const or const/(-float)
                        if val != int(-2147483648) or child[a].t == 'integer': # can't be optimized otherwise
                            child[a] = child[a].ch[0] # remove NEG
                            child[b].value = val = -val

                    # Five optimizations corresponding to -2, -1, 0, 1, 2
                    # for product, and two for division:
                    # expr * 1  ->  expr
                    # expr * 0  ->  0  if side-effect free
                    # expr * -1  -> -expr
                    # ident * 2  ->  ident + ident (only if ident is local)
                    # ident * -2  ->  -(ident + ident) (only if ident is local)
                    # expr/1  ->  expr
                    # expr/-1  ->  -expr
                    if (nt == '*' and child[b].t in ('float', 'integer')
                           and val in (-2, -1, 0, 1, 2)
                        or nt == '/'
                           and b == 1 and val in (-1, 1)
                       ):
                        if val == 1:
                            parent[index] = child[a]
                            return
                        if val == 0:
                            if child[a].SEF:
                                parent[index] = child[b]
                            return
                        if val == -1:
                            # Note 0.0*-1 equals -0.0 in LSL, so this is safe
                            node = parent[index] = nr(nt='NEG', t=node.t,
                                ch=[child[a]], SEF=child[a].SEF)
                            return
                        # only -2, 2 remain
                        if child[a].nt == 'IDENT' and self.isLocalVar(child[a]):
                            child[b] = child[a].copy()
                            node.nt = '+'
                            if val == -2:
                                parent[index] = nr(nt='NEG', t=node.t,
                                    ch=[node], SEF=node.SEF)
                            return
                return

            if nt == '==':
                if child[0].t == child[1].t == 'integer':
                    # Deal with operands in any order
                    a, b = 0, 1
                    if child[b].nt != 'CONST':
                        a, b = 1, 0

                    # a == -1 (in any order)  ->  !~a,
                    # a == 0  ->  !a
                    # a == 1  ->  !~-a
                    if child[b].nt == 'CONST':
                        if child[b].value in (-1, 0, 1):
                            node = child[a]
                            if child[b].value == -1:
                                node = nr(nt='~', t='integer', ch=[node],
                                    SEF=node.SEF)
                            elif child[b].value == 1:
                                node = nr(nt='NEG', t='integer', ch=[node],
                                    SEF=node.SEF)
                                node = nr(nt='~', t='integer', ch=[node],
                                    SEF=node.SEF)
                            node = parent[index] = nr(nt='!', t='integer',
                                ch=[node], SEF=node.SEF)
                            # Can't delete
                # See https://docs.python.org/2/reference/simple_stmts.html#del
                            child = None
                            self.FoldTree(parent, index)
                            return

                    # -a == -b  ->  a == b with const variations.
                    # Note this changes the sign of two CONSTs but that case
                    # should not reach here, as those are resolved earlier.
                    if ((child[0].nt == 'NEG' or child[0].nt == 'CONST')
                        and
                        (child[1].nt == 'NEG' or child[1].nt == 'CONST')
                       ):
                        for a in (0, 1):
                            if child[a].nt == 'NEG':
                                child[a] = child[a].ch[0]  # remove sign
                            else:
                                child[a].value = lslfuncs.neg(
                                    child[a].value)


                if self.CompareTrees(child[0], child[1]):
                    # expr == expr  ->  1
                    parent[index] = nr(nt='CONST', t='integer', value=1,
                        SEF=True)
                    return
                return

            if nt in ('<=', '>=') or nt == '!=' and child[0].t != 'list':
                # Except for list != list, all these comparisons are compiled
                # as !(a>b) etc. so we transform them here in order to reduce
                # the number of cases to check.
                # a<=b  -->  !(a>b);  a>=b  -->  !(a<b);  a!=b  -->  !(a==b)
                node.nt = {'<=':'>', '>=':'<', '!=':'=='}[nt]
                parent[index] = nr(nt='!', t=node.t, ch=[node])
                self.FoldTree(parent, index)
                return

            if nt == '>' and (child[0].SEF and child[1].SEF
                or child[0].nt == 'CONST'
                or child[1].nt == 'CONST'
               ):
                # Invert the inequalities to avoid doubling the cases to check.
                # a>b  ->  b<a
                nt = node.nt = '<'
                child[1], child[0] = child[0], child[1]
                # fall through to check for '<'

            if nt == '<':
                # expr < expr  ->  0
                if self.CompareTrees(child[0], child[1]):
                    parent[index] = nr(nt='CONST', t='integer', value=0,
                        SEF=True)
                    return
                if child[0].t == child[1].t in ('integer', 'float'):
                    if (child[0].nt == 'CONST'
                        and child[1].nt == 'FNCALL'
                        and self.FnSEF(child[1])
                       ):
                        # CONST < FNCALL aka FNCALL > CONST
                        # when FNCALL.max <= CONST: always false
                        # when CONST < FNCALL.min: always true
                        if ('max' in self.symtab[0][child[1].name]
                            and not lslfuncs.less(child[0].value,
                                self.symtab[0][child[1].name]['max'])
                           ):
                            parent[index] = nr(nt='CONST', t='integer',
                                value=0, SEF=True)
                            return
                        if ('min' in self.symtab[0][child[1].name]
                            and lslfuncs.less(child[0].value,
                                self.symtab[0][child[1].name]['min'])
                           ):
                            parent[index] = nr(nt='CONST', t='integer',
                                value=1, SEF=True)
                            return
                    if (child[1].nt == 'CONST'
                        and child[0].nt == 'FNCALL'
                        and self.FnSEF(child[0])
                       ):
                        # FNCALL < CONST
                        # when CONST > FNCALL.max: always true
                        # when CONST <= FNCALL.min: always false
                        if ('max' in self.symtab[0][child[0].name]
                            and lslfuncs.less(
                                self.symtab[0][child[0].name]['max']
                                , child[1].value)
                           ):
                            parent[index] = nr(nt='CONST', t='integer',
                                value=1, SEF=True)
                            return
                        if ('min' in self.symtab[0][child[0].name]
                            and not lslfuncs.less(
                                self.symtab[0][child[0].name]['min'],
                                child[1].value)
                           ):
                            parent[index] = nr(nt='CONST', t='integer',
                                value=0, SEF=True)
                            return

                # Convert 2147483647<i and i<-2147483648 to i&0
                if (child[0].t == child[1].t == 'integer'
                    and (child[0].nt == 'CONST'
                           and child[0].value == 2147483647
                        or child[1].nt == 'CONST'
                           and child[1].value == int(-2147483648))
                   ):
                    a, b = 0, 1
                    # Put the constant in child[b]
                    if child[a].nt == 'CONST':
                        a, b = b, a
                    nt = node.nt = '&'
                    child[b].value = 0
                    # fall through to check for '&'
                else:
                    return

            if nt in ('&', '|'):
                # expr & expr  ->  expr
                # expr | expr  ->  expr
                if self.CompareTrees(child[0], child[1]):
                    parent[index] = child[0]
                    return

                # Deal with operands in any order
                a, b = 0, 1
                # Put constant in child[b]
                if child[b].nt != 'CONST':
                    a, b = 1, 0

                if child[b].nt == 'CONST':
                    val = child[b].value
                    if (nt == '|' and val == 0
                        or nt == '&'
                           and (val == -1
                                or val == 1 and self.IsBool(child[a]))
                       ):
                        # a|0  ->  a
                        # a&-1  ->  a
                        # a&1  ->  a if a is boolean
                        parent[index] = child[a]
                        return
                    if (nt == '|'
                        and (val == -1
                             or (val & 1) == 1 and self.IsBool(child[a]))
                        or nt == '&' and val == 0
                       ):
                        # a|-1  ->  -1 if a is SEF
                        # a|C  ->  C if bit 0 of C is 1 and a is bool and SEF
                        # a&0  ->  0 if a is SEF
                        if child[a].SEF:
                            parent[index] = child[b]

                # Apply boolean distributivity
                applied = False
                opposite = '&' if nt == '|' else '|'
                if child[0].nt == child[1].nt == opposite:
                    left = child[0].ch
                    right = child[1].ch
                    # Can't loop individually because we must break out of both
                    for c, d in ((0, 0), (0, 1), (1, 0), (1, 1)):
                        if self.CompareTrees(left[c], right[d]):
                            child[1].nt = nt
                            nt = node.nt = opposite
                            opposite = child[1].nt
                            right[d] = left[1 - c]
                            child[0] = left[c]
                            applied = True
                            break

                # Apply absorption, possibly after distributivity
                if child[0].nt == opposite or child[1].nt == opposite:
                    c = 0 if child[1].nt == opposite else 1
                    for d in (0, 1):
                        if (self.CompareTrees(child[c], child[1 - c].ch[d])
                            and child[1 - c].ch[1 - d].SEF
                           ):
                            node = parent[index] = child[c]
                            nt = node.nt
                            child = node.ch
                            applied = True
                            break

                if applied:
                    # Re-fold
                    self.FoldTree(parent, index)

                return

            if nt == '^':
                # expr ^ expr  ->  0
                if self.CompareTrees(child[0], child[1]):
                    parent[index] = nr(nt='CONST', t='integer', value=0,
                        SEF=True)
                    return
                a, b = 0, 1
                if child[a].nt == 'CONST':
                    a, b = 1, 0
                if child[b].nt == 'CONST' and child[b].value in (0, -1):
                    if child[b].value == 0:
                        parent[index] = child[a]
                    else:
                        node.nt = '~'
                        node.ch = [child[a]]
                return

            if nt == '||':
                # Expand to its equivalent a || b  ->  !!(a | b)
                node = nr(nt='|', t='integer', ch=[child[0], child[1]],
                    SEF=child[0].SEF and child[1].SEF)
                node = nr(nt='!', t='integer', ch=[node], SEF=node.SEF)
                node = nr(nt='!', t='integer', ch=[node], SEF=node.SEF)
                parent[index] = node
                # Make another pass with the substitution
                self.FoldTree(parent, index)
            elif nt == '&&':
                # Expand to its equivalent a && b  ->  !(!a | !b)
                orchildren = [
                    nr(nt='!', t='integer', ch=[child[0]], SEF=child[0].SEF),
                    nr(nt='!', t='integer', ch=[child[1]], SEF=child[1].SEF)
                ]
                node = nr(nt='|', t='integer', ch=orchildren,
                    SEF=child[0].SEF and child[1].SEF)
                node = nr(nt='!', t='integer', ch=[node], SEF=node.SEF)
                parent[index] = node
                # Make another pass with the substitution
                self.FoldTree(parent, index)

            return

        if nt in self.assign_ops:
            # Transform the whole thing into a regular assignment, as there are
            # no gains and it simplifies the optimization.

            # An assignment has no side effects only if it's of the form x = x.

            if nt != '=':
                # Replace the node with the expression alone...
                # e.g. a += b  ->  a + b
                node.nt = nt[:-1]

                # Linden Craziness: int *= float; is valid (but no other
                # int op= float is). It's actually performed as
                #    i = (integer)(i + (f));
                # This breaks equivalence of x op= y as x = x op (y) so we add
                # the explicit type cast here.
                if (nt == '*=' and child[0].t == 'integer'
                               and child[1].t == 'float'):
                    node.t = 'float' # Addition shall return float.
                    node = self.Cast(node, 'integer')

                # ... and wrap it in an assignment.
                child = [child[0].copy(), node]
                node = parent[index] = nr(nt='=', t=child[0].t, ch=child)

            # We have a regular assignment either way now. Simplify the RHS.
            self.FoldTree(node.ch, 1)
            chkequal = child[1].ch[0] if child[1].nt == '=' else child[1]
            if (child[0].nt == chkequal.nt == 'IDENT'
                  and chkequal.name == child[0].name
                  and chkequal.scope == child[0].scope
               or child[0].nt == chkequal.nt == 'FLD'
                  and chkequal.ch[0].name == child[0].ch[0].name
                  and chkequal.ch[0].scope == child[0].ch[0].scope
                  and chkequal.fld == child[0].fld
               ):
                parent[index] = child[1]
            return

        if nt == 'IDENT' or nt == 'FLD':
            node.SEF = True
            if self.globalmode:
                ident = child[0] if nt == 'FLD' else node
                # Resolve constant values so they can be optimized
                sym = self.symtab[ident.scope][ident.name]

                defn = self.tree[sym['Loc']]
                assert defn.name == ident.name

                # Assume we already were there
                if defn.ch:
                    val = defn.ch[0]
                    if val.nt != 'CONST' or ident.t == 'key':
                        return
                    val = val.copy()
                else:
                    val = nr(nt='CONST', t=defn.t,
                        value=self.DefaultValues[defn.t], SEF=True)
                if nt == 'FLD':
                    val = nr(nt='CONST', t='float',
                        value=val.value['xyzs'.index(node.fld)], SEF=True)
                parent[index] = val
            return

        if nt == 'FNCALL':
            name = node.name

            SEFargs = True
            CONSTargs = True
            for idx in xrange(len(child)-1, -1, -1):
                self.FoldTree(child, idx)
                # Function is not SEF if any argument is not SEF
                SEFargs = SEFargs and child[idx].SEF
                # Function is not a constant if any argument is not a constant
                CONSTargs = CONSTargs and child[idx].nt == 'CONST'

            sym = self.symtab[0][name]
            OptimizeArgs(node, sym)
            try:
                if 'Fn' in sym and (self.FnSEF(node) or lslcommon.IsCalc):
                    # It's side-effect free if the children are and the function
                    # is marked as SEF.
                    if SEFargs:
                        node.SEF = True
                    if CONSTargs:
                        # Call it
                        fn = sym['Fn']
                        args = [arg.value for arg in child]
                        assert len(args) == len(sym['ParamTypes'])

                        try:
                            # May raise ELSLCantCompute
                            if 'detect' in self.symtab[0][name]:
                                value = fn(*args,
                                           evsym=None if self.CurEvent is None
                                           else self.events[self.CurEvent])
                            else:
                                value = fn(*args)
                        finally:
                            del args

                        if not self.foldtabs:
                            generatesTabs = (
                                isinstance(value, unicode) and u'\t' in value
                                or type(value) == list
                                   and any(isinstance(x, unicode)
                                           and u'\t' in x for x in value)
                                )
                            if generatesTabs:
                                if self.warntabs:
                                    warning(u"Can't optimize call to %s"
                                        u" because it would generate a tab"
                                        u" character (you can force the "
                                        u" optimization with the 'foldtabs'"
                                        u" option, or disable this warning by"
                                        u" disabling the 'warntabs' option)."
                                        % name.decode('utf8'))
                                raise lslfuncs.ELSLCantCompute()
                        # Replace with a constant
                        parent[index] = nr(nt='CONST', t=node.t, value=value,
                            SEF=True)
                        return

                elif SEFargs and 'SEF' in self.symtab[0][name]:
                    # The function is marked as SEF in the symbol table, and the
                    # arguments are all side-effect-free. The result is SEF.
                    node.SEF = True

            except lslfuncs.ELSLCantCompute:
                # Don't transform the tree if function is not computable
                pass

            # At this point, we have resolved whether the function is SEF,
            # or whether the function resolves to a constant.
            OptimizeFunc(self, parent, index)

            return

        if nt == 'PRINT':
            self.FoldTree(child, 0)
            # PRINT is considered to have side effects. If it's there, assume
            # there's a reason.
            return

        if nt == 'EXPR':
            self.FoldTree(child, 0)
            node.SEF = child[0].SEF
            return

        if nt == 'FNDEF':
            # FIXME: Fix SEFness of UDFs
            # A return statement does have side effects for the current
            # function, as removing it would change its behaviour drastically.
            # However, when seen from the outside, that does not make the
            # function as a whole have side effects: if all nodes except
            # return statements are SEF, the function is SEF.

            # CurEvent is needed when folding llDetected* function calls
            if hasattr(node, 'scope'):
                # function definition
                self.CurEvent = None
            else:
                # event definition
                self.CurEvent = node.name
            self.FoldTree(child, 0)

            # Test if the event is SEF and does nothing, and remove it if so.
            if (not hasattr(node, 'scope') and child[0].SEF
                and 'SEF' in self.events[node.name]
               ):
                # Delete ourselves.
                del parent[index]
                return

            # Delete trailing bare RETURNs.
            # TODO: This works, but analysis of code paths is DCR's thing
            # and this is incomplete, e.g. x(){{return;}} is not detected.
            while child[0].ch:
                last = child[0].ch[-1]
                if last.nt != 'RETURN' or last.ch:
                    break
                del child[0].ch[-1]
            if child[0].SEF:
                node.SEF = True
                if node.name in self.symtab[0]:
                    # Mark the symbol table entry if it's not an event.
                    self.symtab[0][node.name]['SEF'] = True
            return

        if nt in ('VECTOR', 'ROTATION', 'LIST'):
            isconst = True
            issef = True
            for idx in xrange(len(child)):
                self.FoldTree(child, idx)
                isconst = isconst and child[idx].nt == 'CONST'
                issef = issef and child[idx].SEF

            if isconst:
                value = [x.value for x in child]
                if nt == 'VECTOR':
                    value = Vector([lslfuncs.ff(x) for x in value])
                elif nt == 'ROTATION':
                    value = Quaternion([lslfuncs.ff(x) for x in value])
                parent[index] = nr(nt='CONST', t=node.t, value=value, SEF=True)
                return
            node.SEF = issef
            return

        if nt == 'STDEF':
            for idx in xrange(len(child) - 1, -1, -1):
                self.FoldTree(child, idx)
            if not child:
                # All events removed - add a dummy timer()
                child.append(nr(nt='FNDEF', t=None, name='timer',
                              pscope=0, ptypes=[], pnames=[],
                              ch=[nr(nt='{}', t=None, ch=[])]
                             ))
            return

        if nt == '{}':
            # Remove SEF statements, and mark as SEF if it ends up empty
            idx = 0
            nchild = len(child)
            while idx < nchild:
                self.FoldTree(child, idx)
                self.FoldStmt(child, idx)
                if child[idx].SEF:
                    # SEF statements can be removed
                    del child[idx]
                    nchild -= 1
                else:
                    idx += 1
            # Make another pass to remove JUMPs to the next statement
            changed = True  # Allow entering the loop
            while changed:
                changed = False
                idx = 0
                while idx < nchild:
                    advance = 1
                    if child[idx].nt == 'JUMP':
                        idx2 = idx + 1
                        while idx2 < nchild:
                            # Search for a label that is the destination of
                            # this JUMP, skipping other labels
                            if child[idx2].nt != '@':
                                break
                            if (child[idx].scope == child[idx2].scope
                                and child[idx].name == child[idx2].name
                               ):
                                sym = self.symtab[child[idx].scope]
                                sym = sym[child[idx].name]
                                # remove the JUMP
                                del child[idx]
                                advance = 0
                                changed = True
                                idx2 -= 1  # it has scrolled
                                nchild -= 1
                                # remove reference to label
                                assert(sym['ref'])
                                sym['ref'] -= 1
                                if sym['ref'] == 0:
                                    # No longer referenced - delete label too
                                    del child[idx2]
                                    nchild -= 1
                                    break
                            idx2 += 1
                        del idx2
                    idx += advance

            # We're SEF if we're empty, as we've removed all SEF statements
            node.SEF = nchild == 0
            return

        if nt == 'IF':
            self.ExpandCondition(child, 0)
            self.FoldTree(child, 0)
            self.FoldCond(child, 0)
            if child[0].nt == 'CONST':
                # We might be able to remove one of the branches.
                if lslfuncs.cond(child[0].value):
                    self.FoldTree(child, 1)
                    self.FoldStmt(child, 1)
                    parent[index] = child[1]
                    return
                elif len(child) == 3:
                    self.FoldTree(child, 2)
                    self.FoldStmt(child, 2)
                    parent[index] = child[2]
                    return
                else:
                    # No ELSE branch, replace the statement with an empty one.
                    parent[index] = nr(nt=';', t=None, SEF=True)
                    return
            else:
                self.FoldTree(child, 1)
                self.FoldStmt(child, 1)
                if len(child) > 2:
                    self.FoldTree(child, 2)
                    self.FoldStmt(child, 2)
                    # Check if it makes sense to swap if and else branches
                    if not child[2].SEF:
                        # Check if we can gain something by negating the
                        # expression.
                        # Swap 'if' and 'else' branch when the condition has
                        # a '!' prefix
                        if child[0].nt == '!':
                            child[0] = child[0].ch[0]
                            child[1], child[2] = child[2], child[1]
                        # Swap them if condition is '==' with integer operands
                        if (child[0].nt == '=='
                            and child[0].ch[0].t
                                == child[0].ch[1].t == 'integer'
                           ):
                            child[0].nt = '^'
                            child[1], child[2] = child[2], child[1]
                    # Re-test just in case we swapped in the previous check.
                    if child[2].SEF:
                        # no point in "... else ;" - remove else branch
                        del child[2]
                if child[1].SEF:
                    # if (X) ;  ->  X;
                    if len(child) == 2:
                        parent[index] = nr(nt='EXPR', t=child[0].t,
                            ch=[child[0]])
                        # It has been promoted to statement. Fold it as such.
                        # (Will remove it if SEF)
                        self.FoldStmt(parent, index)
                        return

                    # If type(X) != Key, then:
                    # if (X) ; else {stuff}  ->  if (!X) {stuff}
                    # (being careful with labels again)
                    if (child[0].t != 'key'
                        and (child[2].nt != 'IF'
                             or len(child[2].ch) == 3
                             or child[2].ch[1].nt != '@')
                       ):
                        # We've already converted all other types to equivalent
                        # comparisons
                        assert child[0].t == 'integer'
                        child[0] = nr(nt='!', t='integer', ch=[child[0]])
                        del child[1]
                        self.FoldTree(child, 0)
                        self.FoldCond(child, 0)

            if all(subnode.SEF for subnode in child):
                node.SEF = True
            return

        if nt == 'WHILE':
            # Loops are not considered side-effect free. If the expression is
            # TRUE, it's definitely not SEF. If it's FALSE, it will be optimized
            # out anyway. Otherwise we just don't know if it may be infinite,
            # even if every component is SEF.

            if not child[1].SEF:

                self.ExpandCondition(child, 0)
                self.FoldTree(child, 0)
                self.FoldCond(child, 0)
                if child[0].nt == 'CONST':
                    # See if the whole WHILE can be eliminated.
                    if not lslfuncs.cond(child[0].value):
                        # Whole statement can be removed.
                        parent[index] = nr(nt=';', t=None, SEF=True)
                        return
                self.FoldTree(child, 1)
                self.FoldStmt(child, 1)
                return

            # It does nothing - Turn it into a do..while
            nt = node.nt = 'DO'
            child[0], child[1] = child[1], child[0]

            # Fall through to optimize as DO..WHILE

        if nt == 'DO':
            self.FoldTree(child, 0) # This one is always executed.
            self.FoldStmt(child, 0)
            self.ExpandCondition(child, 1)
            self.FoldTree(child, 1)
            self.FoldCond(child, 1)
            # See if the latest part is a constant.
            if child[1].nt == 'CONST':
                if not lslfuncs.cond(child[1].value):
                    # Only one go. Replace with the statement(s).
                    parent[index] = child[0]
            return

        if nt == 'FOR':
            assert child[0].nt == 'EXPRLIST'
            assert child[2].nt == 'EXPRLIST'
            self.FoldAndRemoveEmptyStmts(child[0].ch)

            self.ExpandCondition(child, 1) # Condition.
            self.FoldTree(child, 1)
            self.FoldCond(child, 1)
            if child[1].nt == 'CONST':
                # FOR is delicate. It can have multiple expressions at start.
                # And if there is more than one, these expressions will need a
                # new block, which means new scope, which is dangerous.
                # They are expressions, no declarations or labels allowed, thus
                # no new identifiers may be created in the new scope, but it
                # still feels dodgy.
                if lslfuncs.cond(child[1].value):
                    # Endless loop. Traverse the loop and the iterator.
                    self.FoldTree(child, 3)
                    self.FoldStmt(child, 3)
                    self.FoldAndRemoveEmptyStmts(child[2].ch)
                else:
                    # Convert expression list to code block.
                    exprlist = []
                    for expr in child[0].ch:
                        # Fold into expression statements.
                        exprlist.append(nr(nt='EXPR', t=expr.t, ch=[expr]))
                    if (exprlist or child[2].ch) and child[3].nt == '@':
                        # Corner case. We can't optimize this to one single
                        # statement, so we leave it as-is.
                        self.FoldTree(child, 3)
                        self.FoldStmt(child, 3)
                        self.FoldAndRemoveEmptyStmts(child[2].ch)
                        return

                    # returns type None, as FOR does
                    if exprlist:
                        # We're in the case where there are expressions. If any
                        # remain, they are not SEF (or they would have been
                        # removed earlier) so don't mark this node as SEF.
                        parent[index] = nr(nt='{}', t=None, ch=exprlist)
                    else:
                        if child[3].nt == '@':
                            # Corner case. The label is in the same scope as
                            # this statement, so it must be preserved. Also,
                            # jumping inside the loop would execute the
                            # iterator, so we fold it.
                            self.FoldAndRemoveEmptyStmts(child[2].ch)
                            if not child[2].ch:
                                # if there's something in the 2nd list,
                                # preserve the whole statement, otherwise
                                # replace it with the label
                                parent[index] = child[3]
                        else:
                            parent[index] = nr(nt=';', t=None, SEF=True)
                    return
            else:
                self.FoldTree(child, 3)
                self.FoldStmt(child, 3)
                self.FoldAndRemoveEmptyStmts(child[2].ch)
            return

        if nt == 'RETURN':
            if child:
                self.FoldTree(child, 0)
            return

        if nt == 'DECL':
            if child:
                # Check if child is a simple_expr. If it is, then we keep the
                # original attached to the folded node to use it in the output.
                if getattr(child[0], 'Simple', False):
                    orig = self.CopyNode(child[0])
                    del orig.Simple  # presence of orig in child will be enough
                    self.FoldTree(child, 0)
                    child[0].orig = orig
                else:
                    self.FoldTree(child, 0)
                # Remove assignment if integer zero.
                if (node.t == 'integer' and child[0].nt == 'CONST'
                    and not child[0].value
                   ):
                    node.ch = None
                    return
            else:
                # Add assignment if vector, rotation or float.
                if node.t in ('float', 'vector', 'rotation'):
                    typ = node.t
                    node.ch = [nr(nt='CONST', t=typ, SEF=True,
                        value=0.0 if typ == 'float'
                            else ZERO_VECTOR if typ == 'vector'
                            else ZERO_ROTATION)]
            # Declarations always have side effects.
            return

        if nt == 'STSW':
            # State switch always has side effects.
            return

        if nt == 'SUBIDX':
            # Recurse to every child. It's SEF if all children are.
            idx = 0
            issef = True
            while idx < len(child):
                self.FoldTree(child, idx)
                issef = issef and child[idx].SEF
                idx += 1
            node.SEF = issef
            return

        if nt == ';':
            node.SEF = True
            return

        if nt == '@':
            # SEF if there are no JUMPs jumping to it
            node.SEF = not self.symtab[node.scope][node.name]['ref']
            return

        if nt in ('JUMP', 'V++', 'V--', '--V', '++V', 'LAMBDA'):
            # Except LAMBDA, these all have side effects, as in, can't be
            # eliminated as statements.
            # LAMBDA can't be eliminated without scrolling Loc's.
            return

        assert False, 'Internal error: This should not happen, node type = ' \
            + nt # pragma: no cover

    def IsValidGlobalIdOrConst(self, node):
        # nan can't be represented as a simple constant; all others are valid
        return not (node.nt == 'CONST' and node.t == 'float'
                    and math.isnan(node.value))

    def IsValidGlobalConstant(self, decl):
        if decl.ch is None:
            return True
        expr = decl.ch[0]
        if expr.nt in ('CONST', 'IDENT'):
            return self.IsValidGlobalIdOrConst(expr)
        if expr.nt not in ('VECTOR', 'ROTATION', 'LIST'):
            return False
        return all(elem.nt in ('CONST', 'IDENT')
                   and self.IsValidGlobalIdOrConst(elem)
                   for elem in expr.ch)

    def FoldScript(self, warningpass = True):
        """Optimize the symbolic table symtab in place. Requires a table of
        predefined functions for folding constants.
        """
        self.globalmode = False

        tree = self.tree
        self.CurEvent = None

        FuncOptSetup()

        # Constant folding pass. It does some other optimizations along the way.
        for idx in xrange(len(tree)):
            if tree[idx].nt == 'DECL':
                self.globalmode = True
                self.FoldTree(tree, idx)
                self.globalmode = False
                if warningpass and not self.IsValidGlobalConstant(tree[idx]):
                    warning(u"Expression in globals doesn't resolve to a simple constant.")
            else:
                self.FoldTree(tree, idx)
