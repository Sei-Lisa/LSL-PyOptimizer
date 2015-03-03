
import lslfuncs
from lslparse import warning

class foldconst(object):

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

    def IsBool(self, node):
        """Some operators return 0 or 1, and that allows simplification of
        boolean expressions. This function returns whether we know for sure
        that the result is boolean.
        """
        return False # TODO: implement IsBool
        # Ideas include some functions like llSameGroup and llDetectedGroup.

    def FoldCond(self, parent, index):
        """When we know that the parent is interested only in the truth value
        of the node, we can perform further optimizations. This function deals
        with them.
        """
        node = parent[index]
        nt = node['nt']
        if nt in ('CONST', 'IDENT', 'FLD'):
            if node['nt'] == 'CONST':
                node['t'] = 'integer'
                node['value'] = -1 if lslfuncs.cond(node['value']) else 0
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
            elif nt == '==':
                if child[0]['nt'] == 'CONST' and -1 <= child[0]['value'] <= 1 \
                   or child[1]['nt'] == 'CONST' and -1 <= child[1]['value'] <= 1:
                    # Transform a==b into !(a-b) if either a or b are in [-1, 1]
                    parent[index] = {'nt':'!', 't':'integer', 'ch':[node]}
                    node['nt'] = '-'
                    self.FoldTree(parent, index)

            if nt == '|':
                #TODO: simplify !!a|b or a|!!b -> a|b
                a, b = 0, 1
                if child[a]['nt'] == 'CONST':
                    a, b = 1, 0
                if child[b]['nt'] == 'CONST' and child[b]['value'] and 'SEF' in child[a]:
                    parent[index] = child[b]
                    child[b]['value'] = 1
                    return
            # TODO: on bitwise OR, detect if both operands are negated.
            # If so treat it as an AND, checking if the operands are boolean
            # to try to simplify them to an expression of the form !(a&b)
            # when possible. Or if one is bool and the other is not, to an
            # expression of the form !(a&-b) (if b is bool).

            # TODO: Convert bool(x < 0) to bool(x & 0x80000000)


    def CopyNode(self, node):
        # This is mainly for simple_expr so no need to go deeper than 1 level.
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
            if child[0]['nt'] == 'CONST' and (node['t'] != 'list' or self.globalmode):
                # Enable key constants. We'll typecast them back on output, but
                # this enables some optimizations.
                #if node['t'] != 'key': # key constants not possible

                    parent[index] = {'nt':'CONST', 't':node['t'], 'SEF':True,
                        'value':lslfuncs.typecast(
                            child[0]['value'], self.LSL2PythonType[node['t']])}
            return

        if nt == 'NEG':
            self.FoldTree(child, 0)
            if child[0]['nt'] == 'NEG':
                # Double negation: - - expr  -->  expr
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
            self.FoldCond(child, 0)
            # !! does *not* cancel out (unless in cond), but !!! can be simplified to !
            subexpr = child[0]
            if 'SEF' in subexpr:
                node['SEF'] = True
            if subexpr['nt'] == '!' and subexpr['ch'][0]['nt'] == '!':
                # Simplify !!! to !
                subexpr = child[0] = subexpr['ch'][0]['ch'][0]
            if subexpr['nt'] == 'CONST':
                node = parent[index] = subexpr
                node['value'] = int(not node['value'])
            # TODO: Missing comparison optimization
                # !(i>const) to i<(const+1) if no overflow (4 variants)
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

                    pass # TODO: implement

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
                    # Do we need parentheses for *? It depends on x
                    # e.g. x+3<<3 needs parentheses when converted to (x+3)*8
                    # We can have {<< {<< x y} 3} -> (x << y) * 8 but we can't
                    # have e.g. {<< {& x y} 3}; there will be explicit
                    # parentheses here always, so we don't need to worry.

                    # we have {<<, something, {CONST n}}
                    # we transform it into {*, something, {CONST n}}
                    node['nt'] = '*'
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
                        if val != -2147483648 or child[a]['t'] == 'integer': # can't be optimized otherwise
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
                        if child[a]['nt'] == 'IDENT' and 'Local' in self.symtab[child[a]['scope']][child[a]['name']]:
                            child[b] = child[a].copy()
                            node['nt'] = '+'
                            if val == -2:
                                parent[index] = {'nt':'NEG', 't':node['t'], 'ch':[node]}
                                if 'SEF' in node:
                                    parent[index]['SEF'] = True
                            return
                return

            if nt in ('<=', '>=') or nt == '!=' \
                                    and child[0]['t'] in ('float', 'integer') \
                                    and child[1]['t'] in ('float', 'integer'):
                SEF = 'SEF' in node
                node['nt'] = {'<=':'>', '>=':'<', '!=':'=='}[nt]
                node = parent[index] = {'nt':'!', 't':node['t'], 'ch':[node]}
                if SEF:
                    node['SEF'] = True
                # Fold the new node
                self.FoldTree(parent, index)
                return

            if nt in ('<', '>'):
                # i>2147483647 to FALSE if SEF, otherwise convert to a&0
                # i<-2147483648 to FALSE if SEF, otherwise convert to a&0
                a, b = 0, 1
                if child[a]['nt'] == 'CONST':
                    a,b = 1,0
                if child[b]['nt'] == 'CONST' and child[a]['t'] == child[b]['t'] == 'integer' \
                   and (nt == '>' and child[b]['value'] == 2147483647
                        or nt == '<' and child[b]['value'] == -2147483648):
                    if 'SEF' in child[a]:
                        parent[index] = node = child[b]
                        node['value'] = 0
                        return
                    nt = node['nt'] = '&'
                    child[b]['value'] = 0
                    # fall through to check for '&'

            if nt in ('&', '|'):
                # Deal with operands in any order
                a, b = 0, 1
                if child[a]['nt'] == 'CONST' and child[a]['t'] in ('float', 'integer'):
                    a, b = 1, 0

                if child[b]['nt'] == 'CONST':
                    val = child[b]['value']
                    if val == 0 and nt == '|' or val == -1 and nt == '&':
                        # a|0  ->  a
                        # a&-1  ->  a
                        parent[index] = child[a]
                        return
                    if val == -1 and nt == '|' or val == 0 and nt == '&':
                        # a|-1  ->  -1 if a is SEF
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

            # TODO: Find some way to convert keys to "" e.g. llListen("", NULL_KEY, "")
            # TODO: Find some way to convert PI to 4 in llSensor[Repeat]
            if 'Fn' in self.symtab[0][node['name']]:
                # Guaranteed to be side-effect free if the children are.
                if SEFargs:
                    node['SEF'] = True
                if CONSTargs:
                    # Call it
                    fn = self.symtab[0][node['name']]['Fn']
                    try:
                        if node['name'][:10] == 'llDetected':
                            value = fn(*tuple(arg['value'] for arg in child),
                                       event=self.CurEvent)
                        else:
                            value = fn(*tuple(arg['value'] for arg in child))
                        if not self.foldtabs and isinstance(value, unicode) and '\t' in value:
                            warning('Tab in function result and foldtabs option not used.')
                            return
                        parent[index] = {'nt':'CONST', 't':node['t'], 'value':value}
                    except lslfuncs.ELSLCantCompute:
                        # Don't transform the tree if function is not computable
                        pass
                elif node['name'] == 'llGetListLength' and child[0]['nt'] == 'IDENT':
                    # Convert llGetListLength(ident) to (ident != [])
                    node = {'nt':'CONST', 't':'list', 'value':[]}
                    parent[index] = node = {'nt':'!=', 't':'list', 'ch':[child[0], node]}
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
            if 'SEF' in child[0]:
                node['SEF'] = True
                if node['name'] in self.symtab[0]:
                    # Mark the symbol table entry if it's not an event.
                    self.symtab[0][node['name']]['SEF'] = True
            return

        if nt in ('VECTOR', 'ROTATION', 'LIST'):
            isconst = True
            issef = True
            for idx in xrange(len(child)-1, -1, -1):
                self.FoldTree(child, idx)
                if child[idx]['nt'] != 'CONST':
                    isconst = False
                if 'SEF' not in child[idx]:
                    issef = False
            if isconst:
                value = [elem['value'] for elem in child]
                if nt == 'VECTOR':
                    value = lslfuncs.Vector([lslfuncs.ff(x) for x in value])
                    parent[index] = {'nt':'CONST', 'SEF':True, 't':node['t'],
                        'value':value}
                    return
                elif nt == 'ROTATION':
                    value = lslfuncs.Quaternion([lslfuncs.ff(x) for x in value])
                    parent[index] = {'nt':'CONST', 'SEF':True, 't':node['t'],
                        'value':value}
                    return
            if nt == 'LIST' and len(child) == 1:
                node = parent[index] = self.Cast(child[0], 'list')
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
                     or nt == '{}' and child[idx]['nt'] == '{}' and not child[idx]['ch']:
                    del child[idx]
                else:
                    if 'StSw' in child[idx]:
                        node['StSw'] = True
                    idx += 1
            if issef:
                node['SEF'] = True
            return

        if nt == 'IF':
            # TODO: Swap IF/ELSE if both present and nonempty and cond starts with !
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
                        if len(child) == 3:
                            del child[2] # Delete ELSE if present
                            return
                    else:
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
                    parent[index] = {'nt':';', 't':None, 'SEF':True}
                    return
            else:
                self.FoldTree(child, 1)
                self.FoldStmt(child, 1)
                if len(child) > 2:
                    self.FoldTree(child, 2)
                    self.FoldStmt(child, 2)
                    if child[2]['nt'] == ';' \
                       or child[2]['nt'] == '{}' and not child[2]['ch']:
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
                    # Can be removed.
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
                # no new identifiers, but it still feels uneasy.
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
                    # returns type None, as FOR does
                    if exprlist:
                        # We're in the case where there are expressions. If any
                        # remain, they are not SEF (or they would have been
                        # removed earlier) so don't mark this node as SEF.
                        parent[index] = {'nt':'{}', 't':None, 'ch':exprlist}
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

    def IsValidGlobalConstant(self, decl):
        if 'ch' not in decl:
            return True
        expr = decl['ch'][0]
        if expr['nt'] in ('CONST', 'IDENT'):
            return True
        if expr['nt'] not in ('VECTOR', 'ROTATION', 'LIST'):
            return False
        return all(elem['nt'] in ('CONST', 'IDENT') for elem in expr['ch'])

    def FoldScript(self):
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
                if not self.IsValidGlobalConstant(tree[idx]):
                    warning('Expression does not resolve to a single constant.')
            else:
                self.FoldTree(tree, idx)
