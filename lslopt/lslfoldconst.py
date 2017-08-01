#    (C) Copyright 2015-2016 Sei Lisa. All rights reserved.
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
import lslfuncs
import math
from lslparse import warning
from lslfuncparams import OptimizeParams

class foldconst(object):

    def isLocalVar(self, node):
        name = node['name']
        scope = node['scope']
        return self.symtab[scope][name]['Kind'] == 'v' \
            and 'Loc' not in self.symtab[scope][name]

    def FoldAndRemoveEmptyStmts(self, lst):
        """Utility function for elimination of useless expressions in FOR"""
        idx = 0
        while idx < len(lst):
            self.FoldTree(lst, idx)
            self.FoldStmt(lst, idx)
            # If eliminated, it must be totally removed. A ';' won't do.
            if lst[idx]['nt'] == ';':
                del lst[idx]
            else:
                idx += 1

    def DoesSomething(self, node):
        """Tell if a subtree does something or is just empty statements
        (a pure combination of ';' and '{}')

        Not to be confused with lslparse.does_something which includes labels,
        and applies to a block's statement list, not to a node.
        """
        if node['nt'] != ';':
            if node['nt'] == '{}':
                for subnode in node['ch']:
                    if self.DoesSomething(subnode):
                        return True
            else:
                return True
        return False

    def FoldStmt(self, parent, index):
        """Simplify a statement."""
        node = parent[index]
        if node['nt'] == 'EXPR':
            node = node['ch'][0]
        # If the statement is side-effect-free, remove it as it does nothing.
        if 'SEF' in node:
            # Side-effect free means that a statement does nothing except
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
            # Type casts do not add side effects. Neither do binary operators.
            parent[index] = {'nt':';', 't':None, 'SEF': True}
            return
        # Post-increments take more space than pre-increments.
        if node['nt'] in ('V++', 'V--'):
            node['nt'] = '++V' if node['nt'] == 'V++' else '--V';

        # Function calls are SEF if both the function and the args are SEF.
        # If the statement is a function call and the function is marked as SEF
        # at this point, it means the arguments are not SEF. Replace the node
        # in that case with a block.
        if node['nt'] == 'FNCALL' and 'SEF' in self.symtab[0][node['name']] and 'Loc' in self.symtab[0][node['name']]:
            parent[index] = {'nt':'{}', 't':None, 'ch':
                [{'nt':'EXPR','t':x['t'],'ch':[x]} for x in node['ch']]}
            self.FoldTree(parent, index)
            return

    def IsBool(self, node):
        """Some operators return 0 or 1, and that allows simplification of
        boolean expressions. This function returns whether we know for sure
        that the result is boolean.
        """
        nt = node['nt']
        if nt in ('<', '!', '>', '<=', '>=', '==', '||', '&&') \
           or nt == '!=' and node['ch'][0]['t'] != 'list' \
           or nt == '&' and (self.IsBool(node['ch'][0]) or self.IsBool(node['ch'][1])) \
           or nt in ('|', '^', '*') and self.IsBool(node['ch'][0]) and self.IsBool(node['ch'][1]) \
           or nt == 'CONST' and node['t'] == 'integer' and node['value'] in (0, 1):
            return True

        if nt == 'FNCALL':
            sym = self.symtab[0][node['name']]
            if sym['Type'] == 'integer' and 'min' in sym and 'max' in sym \
               and sym['min'] == 0 and sym['max'] == 1:
                return True

        return False

    def FoldCond(self, parent, index, ParentIsNegation = False):
        """When we know that the parent is interested only in the truth value
        of the node, we can perform further optimizations. This function deals
        with them.
        """
        node = parent[index]
        nt = node['nt']
        if nt in ('CONST', 'IDENT', 'FLD'):
            if node['nt'] == 'CONST':
                node['t'] = 'integer'
                node['value'] = 1 if lslfuncs.cond(node['value']) else 0
            return # Nothing to do if it's already simplified.
        child = node['ch'] if 'ch' in node else None

        if nt == '!' and child[0]['nt'] == '!':
            # bool(!!a) equals bool(a)
            parent[index] = child[0]['ch'][0]
            return

        if nt == 'NEG':
            # bool(-a) equals bool(a)
            parent[index] = child[0]
            return

        if nt in self.binary_ops and child[0]['t'] == child[1]['t'] == 'integer':
            if nt == '!=':
                if child[0]['nt'] == 'CONST' and child[0]['value'] == 1 \
                   or child[1]['nt'] == 'CONST' and child[1]['value'] == 1:
                    # a != 1  ->  a - 1  (which FoldTree will transform to ~-a)
                    node['nt'] = '-'
                else:
                    # This converts != to ^; FoldTree will simplify ^-1 to ~
                    # and optimize out ^0.
                    node['nt'] = '^'
                self.FoldTree(parent, index)
                return

            if nt == '==':
                if child[0]['nt'] == 'CONST' and -1 <= child[0]['value'] <= 1 \
                   or child[1]['nt'] == 'CONST' and -1 <= child[1]['value'] <= 1:
                    # Transform a==b into !(a-b) if either a or b are in [-1, 1]
                    parent[index] = {'nt':'!', 't':'integer', 'ch':[node]}
                    node['nt'] = '-'
                    self.FoldTree(parent, index)
                return

            if nt == '|':
                # In a boolean context, the operands count as booleans.
                self.FoldCond(child, 0)
                self.FoldCond(child, 1)

                # Deal with operands in any order
                a, b = 0, 1
                # Put constant in child[b] if present
                if child[b]['nt'] != 'CONST':
                    a, b = 1, 0
                if child[b]['nt'] == 'CONST' and child[b]['value'] and 'SEF' in child[a]:
                    parent[index] = child[b]
                    child[b]['value'] = -1
                    return

                # Check if the operands are a negation ('!') or can be inverted
                # without adding more than 1 byte and are boolean.
                # We only support '<' and some cases of '&' (are there more?)
                Invertible = [False, False]
                for a in (0, 1):
                    Invertible[a] = child[a]['nt'] == '!'
                    if child[a]['nt'] == '<' \
                       and child[a]['ch'][0]['t'] == child[a]['ch'][1]['t'] == 'integer':
                        if child[a]['ch'][0]['nt'] == 'CONST' \
                           and child[a]['ch'][0]['value'] != 2147483647 \
                           or child[a]['ch'][1]['nt'] == 'CONST' \
                           and child[a]['ch'][1]['value'] != int(-2147483648):
                            Invertible[a] = True

                    # Deal with our optimization of a<0 -> a&0x80000000 (see below)
                    if child[a]['nt'] == '&' and (
                       child[a]['ch'][0]['nt'] == 'CONST' and child[a]['ch'][0]['value'] == int(-2147483648)
                       or child[a]['ch'][1]['nt'] == 'CONST' and child[a]['ch'][1]['value'] == int(-2147483648)
                       ):
                        Invertible[a] |= ParentIsNegation

                if (Invertible[0] or Invertible[1]) and ParentIsNegation:
                    # !(!a|b)  ->  a&-!b or a&!b
                    # This deals with the part after the first !, transforming
                    # it into (!a|!!b) so that the outer node can optimize the
                    # negated version to a simple &.
                    for a in (0, 1):
                        if not Invertible[a]:
                            child[a] = {'nt':'!', 't':'integer',
                                'ch':[{'nt':'!', 't':'integer', 'ch':[child[a]]}]
                            }
                            Invertible[a] = True

                if Invertible[0] and Invertible[1]:
                    # Both operands are negated, or negable.
                    # Make them a negation if they aren't already.
                    for a in (0, 1):
                        if child[a]['nt'] == '<':
                            if child[a]['ch'][0]['nt'] == 'CONST':
                                child[a]['ch'][0]['value'] += 1
                            else:
                                child[a]['ch'][1]['value'] -= 1
                            child[a]['ch'][0], child[a]['ch'][1] = \
                                child[a]['ch'][1], child[a]['ch'][0]
                            child[a] = {'nt':'!','t':'integer','ch':[child[a]]}
                        elif child[a]['nt'] == '&':
                            child[a] = {'nt':'!', 't':'integer',
                                'ch':[{'nt':'!', 't':'integer', 'ch':[child[a]]}]
                            }
                            self.FoldTree(child[a]['ch'], 0)
                    # If they are boolean, the expression can be turned into
                    # !(a&b) which hopefully will have a ! uptree if it came
                    # from a '&&' and cancel out (if not, we still remove one
                    # ! so it's good). If one is bool, another transformation
                    # can be performed: !nonbool|!bool -> !(nonbool&-bool)
                    # which is still a gain.

                    # Deal with operands in any order
                    a, b = 0, 1
                    # Put the bool in child[b]['ch'][0].
                    if not self.IsBool(child[b]['ch'][0]):
                       a, b = 1, 0
                    if self.IsBool(child[b]['ch'][0]):
                        if not self.IsBool(child[a]['ch'][0]):
                            child[b]['ch'][0] = {'nt':'NEG','t':'integer',
                                'ch':[child[b]['ch'][0]]}

                        node = parent[index] = {'nt':'!', 't':'integer',
                            'ch':[{'nt':'&','t':'integer',
                                  'ch':[child[0]['ch'][0],
                                        child[1]['ch'][0]]
                                  }]
                        }
                        # Fold the node we've just synthesized
                        # (this deals with SEF)
                        self.FoldTree(parent, index)

                return

            # This optimization turns out to be counter-productive for some
            # common cases, e.g. it turns i >= 0 into !(i & 0x80000000)
            # instead of the more optimal (integer)-1 < i. So we revert it
            # where appropriate (in FoldTree, case '!', and above, case '|').
            if nt == '<':
                if child[1]['nt'] == 'CONST' and child[1]['value'] == 0:
                    nt = node['nt'] = '&'
                    child[1]['value'] = int(-2147483648)
                    # Fall through to check & 0x80000000

            if nt == '&':

                # Deal with operands in any order
                a, b = 0, 1
                # Put constant in child[b], if present
                if child[b]['nt'] != 'CONST':
                    a, b = 1, 0
                if child[b]['nt'] == 'CONST' and child[b]['value'] == int(-2147483648) \
                   and child[a]['nt'] == 'FNCALL':
                    sym = self.symtab[0][child[a]['name']]
                    if 'min' in sym and sym['min'] == -1:
                        node = parent[index] = {'nt':'~', 't':'integer',
                            'ch':[child[a]]}
                        self.FoldTree(parent, index)
                return

    def CopyNode(self, node):
        '''This is mainly for simple_expr so no need to go deeper than 1 level
        '''
        ret = node.copy()
        if 'ch' in ret:
            new = []
            for subnode in ret['ch']:
                new.append(self.CopyNode(subnode))
            ret['ch'] = new
        return ret

    def FoldTree(self, parent, index):
        """Recursively traverse the tree to fold constants, changing it in
        place.

        Also optimizes away IF, WHILE, etc.
        """
        node = parent[index]
        nt = node['nt']
        child = node['ch'] if 'ch' in node else None

        if nt == 'CONST':
            # Job already done. But mark as side-effect free.
            node['SEF'] = True
            return

        if nt == 'CAST':
            self.FoldTree(child, 0)
            if 'SEF' in child[0]:
                node['SEF'] = True
            if child[0]['nt'] == 'CONST':
                # Enable key constants. We'll typecast them back on output, but
                # this enables some optimizations.
                #if node['t'] != 'key': # key constants not possible

                    parent[index] = {'nt':'CONST', 't':node['t'], 'SEF':True,
                        'value':lslfuncs.typecast(
                            child[0]['value'], self.LSL2PythonType[node['t']])}
            return

        if nt == 'NEG':
            self.FoldTree(child, 0)

            if child[0]['nt'] == '+' and (child[0]['ch'][0]['nt'] == 'NEG'
                                          or child[0]['ch'][1]['nt'] == 'NEG'):
                node = parent[index] = child[0]
                child = node['ch']
                for a in (0, 1):
                    if child[a]['nt'] == 'NEG':
                        child[a] = child[a]['ch'][0]
                    else:
                        child[a] = {'nt':'NEG','t':child[a]['t'],'ch':[child[a]]}
                        self.FoldTree(child, a)
                return

            if child[0]['nt'] == 'NEG':
                # Double negation: - - expr  ->  expr
                node = parent[index] = child[0]['ch'][0]
                child = node['ch'] if 'ch' in node else None
            elif child[0]['nt'] == 'CONST':
                node = parent[index] = child[0]
                node['value'] = lslfuncs.neg(node['value'])
                child = None
            elif 'SEF' in child[0]:
                # propagate Side Effect Free flag
                node['SEF'] = True

            if child and node['nt'] == 'NEG' and child[0]['nt'] == '~':
                track = child[0]['ch'][0]
                const = 1
                while track['nt'] == 'NEG' and track['ch'][0]['nt'] == '~':
                    const += 1
                    track = track['ch'][0]['ch'][0]
                if const > 2:
                    # -~-~-~expr  ->  expr+3
                    node = {'nt':'CONST', 't':'integer', 'SEF':True, 'value':const}
                    node = {'nt':'+', 't':'integer', 'ch':[node, track]}
                    if 'SEF' in track:
                        node['SEF'] = True
                    parent[index] = node

            return

        if nt == '!':
            self.FoldTree(child, 0)
            self.FoldCond(child, 0, True)
            # !! does *not* cancel out (unless in cond)
            subexpr = child[0]
            snt = subexpr['nt']
            if 'SEF' in subexpr:
                node['SEF'] = True
            if subexpr['nt'] == 'CONST':
                node = parent[index] = subexpr
                node['value'] = int(not node['value'])
                return
            if snt == '<':
                lop = subexpr['ch'][0]
                rop = subexpr['ch'][1]
                if lop['nt'] == 'CONST' and lop['t'] == rop['t'] == 'integer' \
                   and lop['value'] < 2147483647:
                    lop['value'] += 1
                    subexpr['ch'][0], subexpr['ch'][1] = subexpr['ch'][1], subexpr['ch'][0]
                    parent[index] = subexpr # remove !
                    return
                if rop['nt'] == 'CONST' and lop['t'] == rop['t'] == 'integer' \
                   and rop['value'] > int(-2147483648):
                    rop['value'] -= 1
                    subexpr['ch'][0], subexpr['ch'][1] = subexpr['ch'][1], subexpr['ch'][0]
                    parent[index] = subexpr # remove !
                    return
            if snt == '&':
                a, b = 0, 1
                if subexpr['ch'][b]['nt'] != 'CONST':
                    a, b = 1, 0
                if subexpr['ch'][b]['nt'] == 'CONST' and subexpr['ch'][b]['value'] == int(-2147483648):
                    # !(i & 0x80000000)  ->  -1 < i (because one of our
                    # optimizations can be counter-productive, see FoldCond)
                    subexpr['nt'] = '<'
                    subexpr['ch'][b]['value'] = -1
                    subexpr['ch'] = [subexpr['ch'][b], subexpr['ch'][a]]
                    parent[index] = subexpr
                    return
            return

        if nt == '~':
            self.FoldTree(child, 0)
            subexpr = child[0]
            if 'SEF' in subexpr:
                node['SEF'] = True
            if subexpr['nt'] == '~':
                # Double negation: ~~expr
                parent[index] = subexpr['ch'][0]
            elif subexpr['nt'] == 'CONST':
                node = parent[index] = child[0]
                node['value'] = ~node['value']
            return

        if nt in self.binary_ops:
            # RTL evaluation
            self.FoldTree(child, 1)
            self.FoldTree(child, 0)
            if 'SEF' in child[0] and 'SEF' in child[1]:
                # Propagate SEF flag if both sides are side-effect free.
                node['SEF'] = True

            optype = node['t']
            lval = child[0]
            ltype = lval['t']
            lnt = lval['nt']
            rval = child[1]
            rtype = rval['t']
            rnt = rval['nt']

            if lnt == rnt == 'CONST':
                op1 = lval['value']
                op2 = rval['value']
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
                        result = 1-result
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
                parent[index] = {'nt':'CONST', 't':node['t'], 'SEF':True, 'value':result}
                return

            # Simplifications for particular operands
            if nt == '-':
                if optype in ('vector', 'rotation'):
                    if lnt == 'CONST' and all(component == 0 for component in lval['value']):
                        # Change <0,0,0[,0]>-expr  ->  -expr
                        parent[index] = {'nt':'NEG', 't':node['t'], 'ch':[rval]}
                        if 'SEF' in rval:
                            parent[index]['SEF'] = True
                    elif rnt == 'CONST' and all(component == 0 for component in rval['value']):
                        # Change expr-<0,0,0[,0]>  ->  expr
                        parent[index] = lval
                    return

                # Change - to + - for int/float
                nt = node['nt'] = '+'
                if child[1]['nt'] == 'CONST':
                    rval['value'] = lslfuncs.neg(rval['value'])
                else:
                    rnt = 'NEG'
                    RSEF = 'SEF' in rval
                    rval = child[1] = {'nt':rnt, 't':rval['t'], 'ch':[rval]}
                    self.FoldTree(child, 1)
                    if RSEF:
                        rval['SEF'] = True
                    # rtype unchanged

                # Fall through to simplify it as '+'

            if nt == '+':
                # Tough one. Remove neutral elements for the diverse types,
                # and more.

                # Addition of integers, strings, and lists is associative.
                # Addition of floats, vectors and rotations would be, except
                # for FP precision.
                # TODO: associative addition of lists
                # Associative lists are trickier, because unlike the others,
                # the types of the operands may not be lists
                # so e.g. list+(integer+integer) != (list+integer)+integer.
                if optype == 'integer' or optype == 'string' and self.addstrings:
                    if lnt == '+' and rnt == 'CONST' and lval['ch'][1]['nt'] == 'CONST':
                        # (var + ct1) + ct2  ->  var + (ct1 + ct2)
                        child[1] = {'nt': '+', 't': optype, 'ch':[lval['ch'][1], rval], 'SEF':True}
                        lval = child[0] = lval['ch'][0]
                        lnt = lval['nt']
                        ltype = lval['t']
                        rtype = optype
                        # Fold the RHS again now that we have it constant
                        self.FoldTree(child, 1)
                        rval = child[1]
                        rnt = rval['nt']

                if optype == 'list' and not (ltype == rtype == 'list'):
                    if lnt == 'CONST' and not lval['value']:
                        # [] + nonlist  ->  (list)nonlist
                        parent[index] = self.Cast(rval, optype)
                    return

                if optype in ('vector', 'rotation'):
                    # not much to do with vectors or quaternions either
                    if lnt == 'CONST' and all(x == 0 for x in lval['value']):
                        # Change <0,0,0[,0]>+expr  ->  expr
                        parent[index] = rval
                    elif rnt == 'CONST' and all(x == 0 for x in rval['value']):
                        # Change expr+<0,0,0[,0]>  ->  expr
                        parent[index] = lval
                    return

                # Can't be key, as no combo of addition operands returns key
                # All these types evaluate to boolean False when they are
                # the neutral addition element.
                if optype in ('string', 'float', 'list'):
                    if lnt == 'CONST' and not lval['value']:
                        # 0. + expr  ->  expr
                        # "" + expr  ->  expr
                        # [] + expr  ->  expr
                        parent[index] = self.Cast(rval, optype)
                    elif rnt == 'CONST' and not rval['value']:
                        # expr + 0.  ->  expr
                        # expr + ""  ->  expr
                        # expr + []  ->  expr
                        parent[index] = self.Cast(lval, optype)
                    return

                # Must be two integers. This allows for a number of
                # optimizations. First the most obvious ones.

                if lnt == 'CONST' and lval['value'] == 0:
                    parent[index] = rval
                    return

                if rnt == 'CONST' and rval['value'] == 0:
                    parent[index] = lval
                    return

                if lnt != 'CONST' != rnt:
                    # Neither is const. Two chances to optimize.
                    # 1. -expr + -expr  ->  -(expr + expr) (saves 1 byte)
                    # 2. lvalue + -lvalue  ->  0
                    # There may be other possibilities for optimization,
                    # e.g. (type)ident + -(type)ident but we only do lvalues
                    # here. Note these are integers, no NaN involved.
                    # TODO: Compare the subtrees if they are SEF. If they are
                    # the same subtree, they can cancel out.
                    if lnt == rnt == 'NEG':
                        node = {'nt':'+', 't':optype, 'ch':[lval['ch'][0], rval['ch'][0]]}
                        SEF = 'SEF' in lval['ch'][0] and 'SEF' in rval['ch'][0]
                        if SEF:
                            node['SEF'] = True
                        node = {'nt':'NEG', 't':optype, 'ch':[node]}
                        if SEF:
                            node['SEF'] = True
                        parent[index] = node
                        return

                    if lnt == 'NEG':
                        # Swap to treat always as expr + -expr for simplicity.
                        lnt, lval, rnt, rval = rnt, rval, lnt, lval
                    if lnt == 'IDENT' and rnt == 'NEG' and rval['ch'][0]['nt'] == 'IDENT' \
                       and lval['name'] == rval['ch'][0]['name']:
                        # Replace with 0
                        parent[index] = {'nt':'CONST', 'SEF': True, 't':optype, 'value':0}

                    return

                if lnt == '+' and (lval['ch'][0]['nt'] == 'CONST'
                                   or lval['ch'][1]['nt'] == 'CONST'):
                    # We have expr + const + const or const + expr + const.
                    # Addition of integers mod 2^32 is associative and
                    # commutative, so constants can be merged.
                    if lval['ch'][0]['nt'] == 'CONST':
                        rval['value'] = lslfuncs.S32(rval['value'] + lval['ch'][0]['value'])
                        lval = child[0] = lval['ch'][1]
                    else:
                        rval['value'] = lslfuncs.S32(rval['value'] + lval['ch'][1]['value'])
                        lval = child[0] = lval['ch'][0]
                    lnt = lval['nt']

                if rnt == '+' and (rval['ch'][0]['nt'] == 'CONST'
                                   or rval['ch'][1]['nt'] == 'CONST'):
                    # const + (expr + const) or const + (const + expr)
                    # same as above, join them
                    # FIXME: Isn't this covered by the associative sum above?

                    pass # TODO: implement const + (expr + const) or const + (const + expr)

                if rnt == 'CONST':
                    # Swap the vars to deal with const in lval always
                    lval, lnt, rval, rnt = rval, rnt, lval, lnt
                RSEF = 'SEF' in rval

                if lval['value'] == -1 or lval['value'] == -2:
                    if rnt == 'NEG': # Cancel the NEG
                        node = {'nt':'~', 't':optype, 'ch':rval['ch']}
                        if RSEF:
                            node['SEF'] = True
                    else: # Add the NEG
                        node = {'nt':'NEG', 't':optype, 'ch':[rval]}
                        if RSEF:
                            node['SEF'] = True
                        node = {'nt':'~', 't':optype, 'ch':[node]}
                        if RSEF:
                            node['SEF'] = True
                    if lval['value'] == -2:
                        node = {'nt':'NEG', 't':optype, 'ch':[node]}
                        if RSEF:
                            node['SEF'] = True
                        node = {'nt':'~', 't':optype, 'ch':[node]}
                        if RSEF:
                            node['SEF'] = True
                    parent[index] = node
                    return

                if lval['value'] == 1 or lval['value'] == 2:
                    if rnt == '~': # Cancel the ~
                        node = {'nt':'NEG', 't':optype, 'ch':rval['ch']}
                        if RSEF:
                            node['SEF'] = True
                    else:
                        node = {'nt':'~', 't':optype, 'ch':[rval]}
                        if RSEF:
                            node['SEF'] = True
                        node = {'nt':'NEG', 't':optype, 'ch':[node]}
                        if RSEF:
                            node['SEF'] = True
                    if lval ['value'] == 2:
                        node = {'nt':'~', 't':optype, 'ch':[node]}
                        if RSEF:
                            node['SEF'] = True
                        node = {'nt':'NEG', 't':optype, 'ch':[node]}
                        if RSEF:
                            node['SEF'] = True
                    parent[index] = node
                    return

                # More than 2 becomes counter-productive.

                return

            if nt == '<<' and child[1]['nt'] == 'CONST':
                # Transforming << into multiply saves some bytes.
                if child[1]['value'] & 31:
                    # x << 3  -->  x * 8

                    # we have {<<, something, {CONST n}}
                    # we transform it into {*, something, {CONST n}}
                    nt = node['nt'] = '*'
                    child[1]['value'] = 1 << (child[1]['value'] & 31)

                    # Fall through to optimize product

                else: # x << 0  -->  x
                    parent[index] = child[0]
                    return

            if nt == '%' \
               and child[1]['nt'] == 'CONST' \
               and child[1]['t'] == 'integer' \
               and abs(child[1]['value']) == 1:
                # a%1  ->  a&0
                # a%-1  ->  a&0
                # (SEF analysis performed below)
                nt = node['nt'] = '&'
                child[1]['value'] = 0


            if nt in ('*', '/'):
                # Extract signs outside
                if child[0]['nt'] == 'NEG' or child[1]['nt'] == 'NEG':
                    a, b = 0, 1
                    if child[b]['nt'] == 'NEG':
                        a, b = 1, 0
                    child[a] = child[a]['ch'][0]
                    parent[index] = node = {'nt':'NEG', 't':node['t'], 'ch':[node]}
                    if 'SEF' in node['ch'][0]:
                        node['SEF'] = True
                    # Fold the new expression
                    self.FoldTree(parent, index)
                    return

                # Deal with operands in any order
                a, b = 0, 1
                if child[a]['nt'] == 'CONST' and child[a]['t'] in ('float', 'integer'):
                    a, b = 1, 0

                if child[b]['nt'] == 'CONST':
                    val = child[b]['value']

                    # Optimize out signs if possible.
                    # Note that (-intvar)*floatconst needs cornermath because
                    # -intvar could equal intvar if intvar = -2147483648,
                    # so the sign is a no-op and pushing it to floatconst would
                    # make the result be different.
                    if child[a]['nt'] == 'NEG' \
                       and (self.cornermath
                            or child[a]['t'] != 'integer'
                            or child[b]['t'] != 'float'
                       ):
                        # Expression is of the form (-float)*const or (-float)/const or const/(-float)
                        if val != int(-2147483648) or child[a]['t'] == 'integer': # can't be optimized otherwise
                            child[a] = child[a]['ch'][0] # remove NEG
                            child[b]['value'] = val = -val

                    # Five optimizations corresponding to -2, -1, 0, 1, 2
                    # for product, and two for division:
                    # expr * 1  ->  expr
                    # expr * 0  ->  0  if side-effect free
                    # expr * -1  -> -expr
                    # ident * 2  ->  ident + ident (only if ident is local)
                    # ident * -2  ->  -(ident + ident) (only if ident is local)
                    # expr/1  ->  expr
                    # expr/-1  ->  -expr
                    if nt == '*' and child[b]['t'] in ('float', 'integer') \
                       and val in (-2, -1, 0, 1, 2) \
                       or nt == '/' and b == 1 and val in (-1, 1):
                        if val == 1:
                            parent[index] = child[a]
                            return
                        if val == 0:
                            if 'SEF' in child[a]:
                                parent[index] = child[b]
                            return
                        if val == -1:
                            # Note 0.0*-1 equals -0.0 in LSL, so this is safe
                            node = parent[index] = {'nt':'NEG', 't':node['t'], 'ch':[child[a]]}
                            if 'SEF' in child[a]:
                                node['SEF'] = True
                            return
                        # only -2, 2 remain
                        if child[a]['nt'] == 'IDENT' and self.isLocalVar(child[a]):
                            child[b] = child[a].copy()
                            node['nt'] = '+'
                            if val == -2:
                                parent[index] = {'nt':'NEG', 't':node['t'], 'ch':[node]}
                                if 'SEF' in node:
                                    parent[index]['SEF'] = True
                            return
                return

            if nt in ('<=', '>=') or nt == '!=' and child[0]['t'] != 'list':
                # Except for list != list, all these comparisons are compiled
                # as !(a>b) etc. so we transform them here in order to reduce
                # the number of cases to check.
                # a<=b  -->  !(a>b);  a>=b  -->  !(a<b);  a!=b  -->  !(a==b)
                node['nt'] = {'<=':'>', '>=':'<', '!=':'=='}[nt]
                parent[index] = {'nt':'!', 't':node['t'], 'ch':[node]}
                self.FoldTree(parent, index)
                return

            if nt == '>':
                # Invert the inequalities to avoid doubling the cases to check.
                # a>b  -->   b<a
                nt = node['nt'] = '<'
                child[1], child[0] = child[0], child[1]
                # fall through to check for '<'

            if nt == '<':
                # Convert 2147483647<i and i<-2147483648 to i&0
                if child[0]['t'] == child[1]['t'] == 'integer' \
                   and (child[0]['nt'] == 'CONST' and child[0]['value'] == 2147483647
                        or child[1]['nt'] == 'CONST' and child[1]['value'] == int(-2147483648)):
                    a, b = 0, 1
                    # Put the constant in child[b]
                    if child[a]['nt'] == 'CONST':
                        a, b = 1, 0
                    nt = node['nt'] = '&'
                    child[b]['value'] = 0
                    # fall through to check for '&'
                else:
                    return

            if nt in ('&', '|'):
                # Deal with operands in any order
                a, b = 0, 1
                # Put constant in child[b]
                if child[b]['nt'] != 'CONST':
                    a, b = 1, 0

                if child[b]['nt'] == 'CONST':
                    val = child[b]['value']
                    if nt == '|' and val == 0 or nt == '&' and (val == -1 or val == 1 and self.IsBool(child[a])):
                        # a|0  ->  a
                        # a&-1  ->  a
                        # a&1  ->  a if a is boolean
                        parent[index] = child[a]
                        return
                    if nt == '|' and (val == -1 or val == 1 and self.IsBool(child[a])) or nt == '&' and val == 0:
                        # a|-1  ->  -1 if a is SEF
                        # a|1  ->  1 if a is bool and SEF
                        # a&0  ->  0 if a is SEF
                        if 'SEF' in child[a]:
                            parent[index] = child[b]
                return

            if nt == '^':
                a, b = 0, 1
                if child[a]['nt'] == 'CONST':
                    a, b = 1, 0
                if child[b]['nt'] == 'CONST' and child[b]['value'] in (0, -1):
                    if child[b]['value'] == 0:
                        parent[index] = child[a]
                    else:
                        node['nt'] = '~'
                        node['ch'] = [child[a]]
                return

            if nt == '&&' or nt == '||':
                SEF = 'SEF' in node
                if nt == '||':
                    parent[index] = node = {'nt':'!', 't':'integer', 'ch':[
                        {'nt':'!', 't':'integer', 'ch':[
                            {'nt':'|',  't':'integer', 'ch':[child[0], child[1]]}
                        ]}]}
                    if SEF:
                        node['SEF'] = node['ch'][0]['SEF'] = node['ch'][0]['ch'][0]['SEF'] = True
                else:
                    parent[index] = node = {'nt':'!', 't':'integer', 'ch':[
                        {'nt':'|', 't':'integer', 'ch':[
                            {'nt':'!',  't':'integer', 'ch':[child[0]]}
                            ,
                            {'nt':'!',  't':'integer', 'ch':[child[1]]}
                        ]}]}
                    if SEF:
                        node['SEF'] = node['ch'][0]['SEF'] = True
                    if 'SEF' in node['ch'][0]['ch'][0]['ch'][0]:
                        node['ch'][0]['ch'][0]['SEF'] = True
                    if 'SEF' in node['ch'][0]['ch'][1]['ch'][0]:
                        node['ch'][0]['ch'][1]['SEF'] = True
                # Make another pass with the substitution
                self.FoldTree(parent, index)
                return

            return

        if nt in self.assign_ops:
            # Transform the whole thing into a regular assignment, as there are
            # no gains and it simplifies the optimization.

            # An assignment has no side effects only if it's of the form x = x.

            if nt != '=':
                # Replace the node with the expression alone
                # e.g. a += b  ->  a + b
                node['nt'] = nt[:-1]

                # Linden Craziness: int *= float; is valid (but no other
                # int op= float is). It's actually performed as
                #    i = (integer)(i + (f));
                # This breaks equivalence of x op= y as x = x op (y) so we add
                # the explicit type cast here.
                if nt == '*=' and child[0]['t'] == 'integer' and child[1]['t'] == 'float':
                    node['t'] = 'float' # Addition shall return float.
                    node = self.Cast(node, 'integer')

                # And wrap it in an assignment.
                child = [child[0].copy(), node]
                node = parent[index] = {'nt':'=', 't':child[0]['t'], 'ch':child}

            # We have a regular assignment either way now. Simplify the RHS.
            self.FoldTree(node['ch'], 1)
            if child[0]['nt'] == child[1]['nt'] == 'IDENT' \
               and child[1]['name'] == child[0]['name'] \
               and child[1]['scope'] == child[0]['scope'] \
               or child[0]['nt'] == child[1]['nt'] == 'FLD' \
               and child[1]['ch'][0]['name'] == child[0]['ch'][0]['name'] \
               and child[1]['ch'][0]['scope'] == child[0]['ch'][0]['scope'] \
               and child[1]['fld'] == child[0]['fld']:
                node['SEF'] = True
            self.FoldStmt(parent, index)
            return

        if nt == 'IDENT' or nt == 'FLD':
            node['SEF'] = True
            if self.globalmode:
                ident = child[0] if nt == 'FLD' else node
                # Resolve constant values so they can be optimized
                sym = self.symtab[ident['scope']][ident['name']]

                defn = self.tree[sym['Loc']]
                assert defn['name'] == ident['name']

                # Assume we already were there
                if 'ch' in defn:
                    val = defn['ch'][0]
                    if val['nt'] != 'CONST' or ident['t'] == 'key':
                        return
                    val = val.copy()
                else:
                    val = {'nt':'CONST', 't':defn['t'],
                           'value':self.DefaultValues[defn['t']]}
                if nt == 'FLD':
                    val = {'nt':'CONST', 't':'float',
                           'value':val['value']['xyzs'.index(node['fld'])]}
                parent[index] = val
            return

        if nt == 'FNCALL':
            SEFargs = True
            CONSTargs = True
            for idx in xrange(len(child)-1, -1, -1):
                self.FoldTree(child, idx)
                # Function is not SEF if any argument is not SEF
                if 'SEF' not in child[idx]:
                    SEFargs = False
                # Function is not a constant if any argument is not a constant
                if child[idx]['nt'] != 'CONST':
                    CONSTargs = False

            sym = self.symtab[0][node['name']]
            OptimizeParams(node, sym)
            if 'Fn' in sym and ('SEF' in sym and sym['SEF'] or lslcommon.IsCalc):
                # It's side-effect free if the children are and the function
                # is marked as SEF.
                if SEFargs:
                    node['SEF'] = True
                if CONSTargs:
                    # Call it
                    fn = sym['Fn']
                    args = [arg['value'] for arg in child]
                    argtypes = sym['ParamTypes']
                    assert(len(args) == len(argtypes))
                    for argnum in range(len(args)):
                        # Adapt types of params
                        if argtypes[argnum] == 'string':
                            args[argnum] = lslfuncs.fs(args[argnum])
                        elif argtypes[argnum] == 'key':
                            args[argnum] = lslfuncs.fk(args[argnum])
                        elif argtypes[argnum] == 'float':
                            args[argnum] = lslfuncs.ff(args[argnum])
                        elif argtypes[argnum] == 'vector':
                            args[argnum] = lslfuncs.v2f(args[argnum])
                        elif argtypes[argnum] == 'quaternion':
                            args[argnum] = lslfuncs.q2f(args[argnum])
                        elif argtypes[argnum] == 'list':
                            # ensure vectors and quaternions passed to
                            # functions have only float components
                            assert type(args[argnum]) == list
                            # make a shallow copy
                            args[argnum] = args[argnum][:]
                            for i in range(len(args[argnum])):
                                if type(args[argnum][i]) == lslcommon.Quaternion:
                                    args[argnum][i] = lslfuncs.q2f(args[argnum][i])
                                elif type(args[argnum][i]) == lslcommon.Vector:
                                    args[argnum][i] = lslfuncs.v2f(args[argnum][i])
                    del argtypes
                    try:
                        if node['name'][:10] == 'llDetected':
                            value = fn(*args, event=self.CurEvent)
                        else:
                            value = fn(*args)
                    except lslfuncs.ELSLCantCompute:
                        # Don't transform the tree if function is not computable
                        return

                    del args
                    if not self.foldtabs:
                        generatesTabs = (
                            isinstance(value, unicode) and '\t' in value
                            or type(value) == list and any(isinstance(x, unicode) and '\t' in x for x in value)
                            )
                        if generatesTabs:
                            if self.warntabs:
                                warning(u"Can't optimize call to %s because it would generate a tab character (you can force the optimization with the 'foldtabs' option, or disable this warning by disabling the 'warntabs' option)." % node['name'].decode('utf8'))
                            return
                    parent[index] = {'nt':'CONST', 't':node['t'], 'value':value, 'SEF':True}
                elif self.optlistlength and node['name'] == 'llGetListLength':
                    # Convert llGetListLength(expr) to (expr != [])
                    node = {'nt':'CONST', 't':'list', 'value':[]}
                    parent[index] = node = {'nt':'!=', 't':'list',
                                            'ch':[child[0], node]}
                elif (node['name'] == 'llDumpList2String'
                      and child[1]['nt'] == 'CONST'
                      and child[1]['t'] in ('string', 'key')
                      and child[1]['value'] == u""):
                    # Convert llDumpList2String(expr, "") to (string)(expr)
                    node['nt'] = 'CAST'
                    del child[1]
                    del node['name']
            elif SEFargs and 'SEF' in self.symtab[0][node['name']]:
                # The function is marked as SEF in the symbol table, and the
                # arguments are all side-effect-free. The result is SEF.
                node['SEF'] = True
            return

        if nt == 'PRINT':
            self.FoldTree(child, 0)
            # PRINT is considered to have side effects. If it's there, assume
            # there's a reason.
            return

        if nt == 'EXPR':
            self.FoldTree(child, 0)
            if 'SEF' in child[0]:
                node['SEF'] = True
            return

        if nt == 'FNDEF':
            # used when folding llDetected* function calls
            if 'scope' in node:
                # function definition
                self.CurEvent = None
            else:
                # event definition
                self.CurEvent = node['name']
            self.FoldTree(child, 0)
            # TODO: This works, but analysis of code paths is DCR's thing
            # and this is incomplete, e.g. x(){{return;}} is not detected.
            while 'ch' in child[0] and child[0]['ch']:
                last = child[0]['ch'][-1]
                if last['nt'] != 'RETURN' or 'ch' in last:
                    break
                del child[0]['ch'][-1]
            if 'SEF' in child[0]:
                node['SEF'] = True
                if node['name'] in self.symtab[0]:
                    # Mark the symbol table entry if it's not an event.
                    self.symtab[0][node['name']]['SEF'] = True
            return

        if nt in ('VECTOR', 'ROTATION', 'LIST'):
            isconst = True
            issef = True
            for idx in xrange(len(child)):
                self.FoldTree(child, idx)
                if child[idx]['nt'] != 'CONST':
                    isconst = False
                if 'SEF' not in child[idx]:
                    issef = False
            if isconst:
                value = [elem['value'] for elem in child]
                if nt == 'VECTOR':
                    value = lslfuncs.Vector([lslfuncs.ff(x) for x in value])
                elif nt == 'ROTATION':
                    value = lslfuncs.Quaternion([lslfuncs.ff(x) for x in value])
                parent[index] = {'nt':'CONST', 'SEF':True, 't':node['t'],
                    'value':value}
                return
            if issef:
                node['SEF'] = True
            return

        if nt == 'STDEF':
            for idx in xrange(len(child)):
                self.FoldTree(child, idx)
            return

        if nt == '{}':
            idx = 0
            issef = True
            while idx < len(child):
                self.FoldTree(child, idx)
                self.FoldStmt(child, idx)
                if 'SEF' not in child[idx]:
                    issef = False
                if child[idx]['nt'] == ';' \
                     or child[idx]['nt'] == '{}' and not child[idx]['ch']:
                    del child[idx]
                else:
                    if 'StSw' in child[idx]:
                        node['StSw'] = True
                    idx += 1
            if issef:
                node['SEF'] = True
            return

        if nt == 'IF':
            # TODO: Swap IF/ELSE if both present and cond starts with !
            self.FoldTree(child, 0)
            self.FoldCond(child, 0)
            if child[0]['nt'] == 'CONST':
                # We might be able to remove one of the branches.
                if child[0]['value']:
                    self.FoldTree(child, 1)
                    # If it has a state switch, the if() must be preserved
                    # (but the else branch may be removed).
                    if 'StSw' in child[1]:
                        # TODO: Get rid of StSw craziness and make another pass
                        # to put them under conditionals if present (if bald
                        # state switches are present, it means they are the
                        # result of optimization so they must be wrapped in an
                        # IF statement). The current approach leaves unnecessary
                        # IFs behind.
                        if len(child) == 3 and child[2]['nt'] != '@':
                            del child[2] # Delete ELSE if present
                            return
                    else:
                        self.FoldStmt(child, 1)
                        if len(child) == 3 and child[2]['nt'] == '@':
                            # Corner case. The label is in the same scope as
                            # this statement, so it must be preserved just in
                            # case it's jumped to.
                            return
                        parent[index] = child[1]
                        return
                elif len(child) == 3:
                    self.FoldTree(child, 2)
                    self.FoldStmt(child, 2)
                    if child[1]['nt'] == '@':
                        # Corner case. The label is in the same scope as this
                        # statement, so it must be preserved just in case it's
                        # jumped to.
                        if not self.DoesSomething(child[2]):
                            del child[2]
                        return
                    parent[index] = child[2]
                    return
                else:
                    # No ELSE branch, replace the statement with an empty one.
                    if child[1]['nt'] == '@':
                        # Corner case. The label is in the same scope as this
                        # statement, so it must be preserved just in case it's
                        # jumped to.
                        parent[index] = child[1]
                        return
                    parent[index] = {'nt':';', 't':None, 'SEF':True}
                    return
            else:
                self.FoldTree(child, 1)
                self.FoldStmt(child, 1)
                if len(child) > 2:
                    self.FoldTree(child, 2)
                    self.FoldStmt(child, 2)
                    if not self.DoesSomething(child[2]):
                        # no point in "... else ;" - remove else branch
                        del child[2]
            if all('SEF' in subnode for subnode in child):
                node['SEF'] = True
            return

        if nt == 'WHILE':
            # Loops are not considered side-effect free. If the expression is
            # TRUE, it's definitely not SEF. If it's FALSE, it will be optimized
            # anyway. Otherwise we just don't know if it may be infinite, even
            # if every component is SEF.

            self.FoldTree(child, 0)
            self.FoldCond(child, 0)
            if child[0]['nt'] == 'CONST':
                # See if the whole WHILE can be eliminated.
                if child[0]['value']:
                    # Endless loop which must be kept.
                    # Recurse on the statement.
                    self.FoldTree(child, 1)
                    self.FoldStmt(child, 1)
                else:
                    if child[1]['nt'] == '@':
                        # Corner case. The label is in the same scope as this
                        # statement, so it must be preserved just in case it's
                        # jumped to.
                        parent[index] = child[1]
                    else:
                        # Whole statement can be removed.
                        parent[index] = {'nt':';', 't':None, 'SEF':True}
                    return
            else:
                self.FoldTree(child, 1)
                self.FoldStmt(child, 1)
            return

        if nt == 'DO':
            self.FoldTree(child, 0) # This one is always executed.
            self.FoldStmt(child, 0)
            self.FoldTree(child, 1)
            self.FoldCond(child, 1)
            # See if the latest part is a constant.
            if child[1]['nt'] == 'CONST':
                if not child[1]['value']:
                    # Only one go. Replace with the statement(s).
                    parent[index] = child[0]
            return

        if nt == 'FOR':
            assert child[0]['nt'] == 'EXPRLIST'
            assert child[2]['nt'] == 'EXPRLIST'
            self.FoldAndRemoveEmptyStmts(child[0]['ch'])

            self.FoldTree(child, 1) # Condition.
            self.FoldCond(child, 1)
            if child[1]['nt'] == 'CONST':
                # FOR is delicate. It can have multiple expressions at start.
                # And if there is more than one, these expressions will need a
                # new block, which means new scope, which is dangerous.
                # They are expressions, no declarations or labels allowed, thus
                # no new identifiers may be created in the new scope, but it
                # still feels dodgy.
                if child[1]['value']:
                    # Endless loop. Traverse the loop and the iterator.
                    self.FoldTree(child, 3)
                    self.FoldStmt(child, 3)
                    self.FoldAndRemoveEmptyStmts(child[2]['ch'])
                else:
                    # Convert expression list to code block.
                    exprlist = []
                    for expr in child[0]['ch']:
                        # Fold into expression statements.
                        exprlist.append({'nt':'EXPR', 't':expr['t'], 'ch':[expr]})
                    if (exprlist or child[2]['ch']) and child[3]['nt'] == '@':
                        # Corner case. We can't optimize this to one single
                        # statement, so we leave it as-is.
                        self.FoldTree(child, 3)
                        self.FoldStmt(child, 3)
                        self.FoldAndRemoveEmptyStmts(child[2]['ch'])
                        return

                    # returns type None, as FOR does
                    if exprlist:
                        # We're in the case where there are expressions. If any
                        # remain, they are not SEF (or they would have been
                        # removed earlier) so don't mark this node as SEF.
                        parent[index] = {'nt':'{}', 't':None, 'ch':exprlist}
                    else:
                        if child[3]['nt'] == '@':
                            # Corner case. The label is in the same scope as
                            # this statement, so it must be preserved. Also,
                            # jumping inside the loop would execute the
                            # iterator, so we fold it.
                            self.FoldAndRemoveEmptyStmts(child[2]['ch'])
                            if not child[2]['ch']:
                                # if there's something in the 2nd list,
                                # preserve the whole statement, otherwise
                                # replace it with the label
                                parent[index] = child[3]
                        else:
                            parent[index] = {'nt':';', 't':None, 'SEF': True}
                    return
            else:
                self.FoldTree(child, 3)
                self.FoldStmt(child, 3)
                self.FoldAndRemoveEmptyStmts(child[2]['ch'])
            return

        if nt == 'RETURN':
            if child:
                self.FoldTree(child, 0)
            return

        if nt == 'DECL':
            if child:
                # Check if child is a simple_expr. If it is, then we keep the
                # original attached to the folded node to use it in the output.
                if child[0].pop('Simple', False):
                    orig = self.CopyNode(child[0])
                    self.FoldTree(child, 0)
                    child[0]['orig'] = orig
                else:
                    self.FoldTree(child, 0)
                # Remove assignment if integer zero.
                if node['t'] == 'integer' and child[0]['nt'] == 'CONST' \
                   and not child[0]['value']:
                    del node['ch']
                    child = None
                    return
            else:
                # Add assignment if vector, rotation or float.
                if node['t'] in ('float', 'vector', 'rotation'):
                    typ = node['t']
                    node['ch'] = [{'nt':'CONST', 't':typ, 'SEF': True,
                        'value': 0.0 if typ == 'float' else
                                 lslfuncs.ZERO_VECTOR if typ == 'vector' else
                                 lslfuncs.ZERO_ROTATION}]
            # Declarations always have side effects.
            return

        if nt == 'STSW':
            # State switch always has side effects.
            node['StSw'] = True
            return

        if nt == 'SUBIDX':
            # Recurse to every child. It's SEF if all children are.
            idx = 0
            issef = True
            while idx < len(child):
                self.FoldTree(child, idx)
                if 'SEF' not in child[idx]:
                    issef = False
                idx += 1
            if issef:
                node['SEF'] = True
            return

        if nt == ';':
            node['SEF'] = True
            return

        if nt in ('JUMP', '@', 'V++', 'V--', '--V', '++V', 'LAMBDA'):
            # Except LAMBDA, these all have side effects, as in, can't be
            # eliminated as statements.
            # LAMBDA can't be eliminated without scrolling Loc's.
            return

        assert False, 'Internal error: This should not happen, node type = ' \
            + nt # pragma: no cover

    def IsValidGlobalIdOrConst(self, node):
        # nan can't be represented as a simple constant; all others are valid
        return not (node['nt'] == 'CONST' and node['t'] == 'float'
                    and math.isnan(node['value']))

    def IsValidGlobalConstant(self, decl):
        if 'ch' not in decl:
            return True
        expr = decl['ch'][0]
        if expr['nt'] in ('CONST', 'IDENT'):
            return self.IsValidGlobalIdOrConst(expr)
        if expr['nt'] not in ('VECTOR', 'ROTATION', 'LIST'):
            return False
        return all(elem['nt'] in ('CONST', 'IDENT')
                   and self.IsValidGlobalIdOrConst(elem)
                   for elem in expr['ch'])

    def FoldScript(self, warningpass = True):
        """Optimize the symbolic table symtab in place. Requires a table of
        predefined functions for folding constants.
        """
        self.globalmode = False

        tree = self.tree
        self.CurEvent = None

        # Constant folding pass. It does some other optimizations along the way.
        for idx in xrange(len(tree)):
            if tree[idx]['nt'] == 'DECL':
                self.globalmode = True
                self.FoldTree(tree, idx)
                self.globalmode = False
                if warningpass and not self.IsValidGlobalConstant(tree[idx]):
                    warning(u"Expression in globals doesn't resolve to a simple constant.")
            else:
                self.FoldTree(tree, idx)
