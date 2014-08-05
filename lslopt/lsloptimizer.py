
import lslfuncs
from lslparse import warning

from lslrenamer import renamer
from lsldeadcode import deadcode

class optimizer(renamer, deadcode):

    # Default values per type when declaring variables
    DefaultValues = {'integer': 0, 'float': 0.0, 'string': u'',
        'key': lslfuncs.Key(u''), 'vector': lslfuncs.ZERO_VECTOR,
        'rotation': lslfuncs.ZERO_ROTATION, 'list': []
        }

    # explicitly exclude assignments
    binary_ops = frozenset(('+','-','*','/','%','<<','>>','<','<=','>','>=',
        '==','!=','|','^','&','||','&&'))
    assign_ops = frozenset(('=','+=','-=','*=','/=','%=','&=','|=','^=','<<=','>>='))
    LSL2PythonType = {'integer':int, 'float':float, 'string':unicode, 'key':lslfuncs.Key,
        'vector':lslfuncs.Vector, 'rotation':lslfuncs.Quaternion, 'list':list}

    ignored_stmts = frozenset(('V++','V--','--V','++V',';','STSW','JUMP','@'))

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
        """If the statement is a constant or an identifier, remove it as it does
        nothing.
        """
        # Ideally this should consider side effect analysis of the whole thing.
        node = parent[index]
        if node['nt'] == 'EXPR':
            node = node['ch'][0]
        if node['nt'] in ('CONST', 'IDENT', 'FLD'):
            parent[index] = {'nt':';','t':None}

    def FoldCond(self, parent, index):
        """When we know that the parent is interested only in the truth value
        of the node, we can perform further optimizations. This function deals
        with them.
        """
        if parent[index]['nt'] in ('CONST', 'IDENT', 'FIELD'):
            return # Nothing to do if it's already simplified.
        # TODO: Implement FoldCond

    def Cast(self, value, newtype):
        # Return a CAST node if the types are not equal, otherwise the
        # value unchanged
        if value['t'] == newtype:
            return value
        return {'nt':'CAST', 't':newtype, 'ch':[value]}

    def CopyNode(self, node):
        # This is mainly for simple_expr so not a big deal.
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
        if node is None: return # Deleted statement
        nt = node['nt']
        child = node['ch'] if 'ch' in node else None

        if nt == 'CONST':
            # Job already done
            return

        if nt == 'CAST':
            self.FoldTree(child, 0)
            if child[0]['nt'] == 'CONST':
                # Enable key constants. We'll typecast them back on output, but
                # this enables some optimizations.
                #if node['t'] != 'key': # key constants not possible

                    parent[index] = {'nt':'CONST', 't':node['t'],
                        'value':lslfuncs.typecast(
                            child[0]['value'], self.LSL2PythonType[node['t']])}
            return

        if nt == 'NEG':
            self.FoldTree(child, 0)
            while child[0]['nt'] == '()' and child[0]['ch'][0]['nt'] == 'NEG':
                child[0] = child[0]['ch'][0] # Remove parentheses
            if child[0]['nt'] == 'NEG':
                # Double negation: - - expr
                parent[index] = child[0]['ch'][0]
            elif child[0]['nt'] == 'CONST':
                node = parent[index] = child[0]
                node['value'] = lslfuncs.neg(node['value'])
            return

        if nt == '!':
            self.FoldTree(child, 0)
            self.FoldCond(child, 0)
            # !! does *not* cancel out, but !!! can be simplified to !
            subexpr = child[0]
            while subexpr['nt'] == '()' and subexpr['ch'][0]['nt'] in ('()', '~', '!', '++V', '--V'):
                subexpr = child[0] = subexpr['ch'][0] # Remove parentheses
            if subexpr['nt'] == '!' and subexpr['ch'][0]['nt'] == '!':
                # Simplify !!! to !
                subexpr = child[0] = subexpr['ch'][0]['ch'][0]
            if subexpr['nt'] == 'CONST':
                node = parent[index] = subexpr
                node['value'] = int(not node['value'])
            return

        if nt == '~':
            self.FoldTree(child, 0)
            subexpr = child[0]
            while subexpr['nt'] == '()' and subexpr['ch'][0]['nt'] in ('()', '~', '!', '++V', '--V'):
                subexpr = child[0] = subexpr['ch'][0] # Remove parentheses
            if subexpr['nt'] == '~':
                # Double negation: ~~expr
                parent[index] = subexpr['ch'][0]
            elif subexpr['nt'] == 'CONST':
                node = parent[index] = child[0]
                node['value'] = ~node['value']
            return

        if nt == '()':
            self.FoldTree(child, 0)
            if child[0]['nt'] in ('()', 'CONST', 'VECTOR', 'ROTATION', 'LIST',
               'IDENT', 'FIELD', 'V++', 'V--', 'FUNCTION', 'PRINT'):
                # Child is an unary postfix expression (highest priority);
                # parentheses are redundant and can be removed safely. Not
                # strictly an optimization but it helps keeping the output
                # tidy-ish a bit. It's not done in general (e.g. (a * b) + c
                # does not need parentheses but these are not eliminated). Only
                # cases like (3) or (myvar++) are simplified.
                parent[index] = child[0]
            return

        if nt in self.binary_ops:
            # RTL evaluation
            self.FoldTree(child, 1)
            self.FoldTree(child, 0)
            if child[0]['nt'] == child[1]['nt'] == 'CONST':
                op1 = child[0]['value']
                op2 = child[1]['value']
                if nt == '+':
                    result = lslfuncs.add(op1, op2)
                elif nt == '-':
                    result = lslfuncs.sub(op1, op2)
                elif nt == '*':
                    result = lslfuncs.mul(op1, op2)
                elif nt == '/':
                    result = lslfuncs.div(op1, op2)
                elif nt == '%':
                    result = lslfuncs.mod(op1, op2)
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
                parent[index] = {'nt':'CONST', 't':node['t'], 'value':result}
                return

            # Simplifications for particular operands
            optype = node['t']
            lval = child[0]
            ltype = lval['t']
            lnt = lval['nt']
            rval = child[1]
            rtype = rval['t']
            rnt = rval['nt']
            if nt == '-':
                if optype in ('vector', 'rotation'):
                    if lnt == 'CONST' and all(component == 0 for component in lval['value']):
                        # Change <0,0,0[,0]>-expr  ->  -expr
                        parent[index] = {'nt':'NEG', 't':node['t'], 'ch':[rval]}
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
                    rval = child[1] = {'nt':rnt, 't':rval['t'], 'ch':[rval]}
                    # rtype unchanged

                # Fall through to simplify it as '+'

            if nt == '+':
                # Tough one. Remove neutral elements for the diverse types,
                # and more.
                if optype == 'list' and not (ltype == rtype == 'list'):
                    # Nothing to do with list + nonlist or nonlist + list.
                    # FIXME: Not true. (list)"string" is a 5 byte saving vs.
                    # [] + "string". Activating explicitcast forces the
                    # conversion [] + (list)"string"  ->  (list)"string" which
                    # is what we want here, but it is a loss for other types.
                    # Further analysis needed.
                    return

                if optype in ('vector', 'rotation'):
                    # not much to do with vectors or quaternions either
                    if lnt == 'CONST' and all(component == 0 for component in lval['value']):
                        # Change <0,0,0[,0]>+expr  ->  expr
                        parent[index] = rval
                    elif rnt == 'CONST' and all(component == 0 for component in rval['value']):
                        # Change expr+<0,0,0[,0]>  ->  expr
                        parent[index] = lval
                    return

                # Can't be key, as no combo of addition operands returns key
                # All these types evaluate as boolean False when they are
                # the neutral addition element.
                if optype in ('string', 'float', 'list'):
                    if lnt == 'CONST' and not lval['value']:
                        # 0 + expr  ->  expr
                        # "" + expr  ->  expr
                        # [] + expr  ->  expr
                        parent[index] = self.Cast(rval, optype)
                    elif rnt == 'CONST' and not rval['value']:
                        # expr + 0  ->  expr
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

                # Remove parentheses if they enclose a NEG, to unhide their
                # operators. Precedence rules allow us.
                if lnt == '()' and lval['ch'][0]['nt'] == 'NEG':
                    # (-expr) + expr  ->  -expr + expr
                    lval = child[0] = lval['ch'][0]
                if rnt == '()' and rval['ch'][0]['nt'] == 'NEG':
                    # expr + (-expr)  ->  expr + -expr
                    rval = child[1] = rval['ch'][0]

                if lnt != 'CONST' != rnt:
                    # Neither is const. Two chances to optimize.
                    # 1. -expr + -expr  ->  -(expr + expr) (saves 1 byte)
                    # 2. lvalue + -lvalue  ->  0
                    # There may be other possibilities for optimization,
                    # e.g. (type)ident + -(type)ident but we only do lvalues
                    # here. Note these are integers, no NaN involved.
                    if lnt == rnt == 'NEG':
                        node = {'nt':'+', 't':optype, 'ch':[lval['ch'][0], rval['ch'][0]]}
                        node = {'nt':'()', 't':optype, 'ch':[node]}
                        parent[index] = {'nt':'NEG', 't':optype, 'ch':[node]}
                        return

                    if lnt == 'NEG':
                        # Swap to treat always as expr + -expr for simplicity.
                        lnt, lval, rnt, rval = rnt, rval, lnt, lval
                    if lnt == 'IDENT' and rnt == 'NEG' and rval['ch'][0]['nt'] == 'IDENT' \
                       and lval['name'] == rval['ch'][0]['name']:
                        # Replace with 0
                        parent[index] = {'nt':'CONST', 't':optype, 'value':0}

                    return

                if rnt == 'CONST':
                    # Swap the vars to deal with const in lval always
                    lval, lnt, rval, rnt = rval, rnt, lval, lnt
                if lval['value'] == -1:
                    if rnt == 'NEG':
                        parent[index] = {'nt':'~', 't':optype, 'ch':rval['ch']}
                    else:
                        parent[index] = {'nt':'~', 't':optype,
                            'ch':[{'nt':'NEG', 't':optype, 'ch':[rval]}]}
                    return

                if lval['value'] == -2:
                    if rnt == 'NEG': # Cancel the NEG
                        node = {'nt':'~', 't':optype, 'ch':rval['ch']}
                        node = {'nt':'NEG', 't':optype, 'ch':[node]}
                        parent[index] = {'nt':'~', 't':optype, 'ch':[node]}
                    else: # Add the NEG
                        node = {'nt':'NEG', 't':optype, 'ch':[rval]}
                        node = {'nt':'~', 't':optype, 'ch':[node]}
                        node = {'nt':'NEG', 't':optype, 'ch':[node]}
                        parent[index] = {'nt':'~', 't':optype, 'ch':[node]}
                    return

                if lval['value'] == 1:
                    parent[index] = {'nt':'NEG', 't':optype,
                        'ch':[{'nt':'~', 't':optype, 'ch':[rval]}]}
                    return

                if lval['value'] == 2:
                    node = {'nt':'NEG', 't':optype,
                        'ch':[{'nt':'~', 't':optype, 'ch':[rval]}]}
                    parent[index] = {'nt':'NEG', 't':optype,
                        'ch':[{'nt':'~', 't':optype, 'ch':[node]}]}
                    return

                # More than 2 becomes counter-productive.

                return

            elif nt == '<<' and child[1]['nt'] == 'CONST':
                # Transforming << into multiply saves some bytes.
                if child[1]['value'] & 31:
                    # x << 3  -->  x * 8
                    # Do we need parentheses for *? It depends on x
                    # e.g. x+3<<3 needs parentheses when converted to (x+3)*8
                    # We can have {<< {<< x y} 3} -> (x << y) * 8 but we can't
                    # have e.g. {<< {& x y} 3}; there will be explicit
                    # parentheses here always, so we don't need to worry.

                    # Operands with priority between * (not included) and <<
                    # (included).
                    if child[0]['nt'] in ('+', '-', 'NEG', '<<', '>>'):
                        child[0] = {'nt':'()', 't':child[0]['t'], 'ch':[child[0]]}
                    # we have {<<, something, {CONST n}}, transform into {*, something, {CONST n}}
                    node['nt'] = '*'
                    child[1]['value'] = 1<<(child[1]['value'] & 31)
                else: # x << 0  -->  x
                    parent[index] = child[0]
            else:
                pass # TODO: Eliminate redundancy (x*1, x*-1, x|0, x&-1, etc.)
                    # Include != to ^ and || to | and maybe && to &
                    # Note some cases e.g. x*0 can't be optimized away without side-effect analysis.
                    # But some cases like %1 can be turned into *0 to save bytes.
                    # Turn also % (power of 2) into & mask (oops, nope, negative doesn't work)
                    # Maybe turn != -1 into ~ in if()'s.
            return

        if nt in self.assign_ops:
            # Transform the whole thing into a regular assignment, as there are
            # no gains and it simplifies the optimization.

            if nt != '=':
                # Replace the node with the expression alone
                child[1] = {'nt':'()', 't':child[1]['t'], 'ch':[child[1]]}
                node['nt'] = nt[:-1]

                # Linden Craziness: i *= f; is valid (but no other i op= f is).
                # It's actually performed as i = (integer)(i + (f)). This breaks
                # regular equivalence of x op= y as x = x op (y) so we add
                # the type cast here.
                if nt == '*=' and child[0]['t'] == 'integer' and child[1]['t'] == 'float':
                    node['t'] = 'float' # Addition shall return float.
                    node = self.Cast(node, 'integer')

                # And wrap it in an assignment.
                node = parent[index] = {'nt':'=', 't':child[0]['t'], 'ch':[child[0].copy(), node]}

            # We have a regular assignment either way now. Simplify the RHS.
            self.FoldTree(node['ch'], 1)
            return

        if nt == 'IDENT' or nt == 'FLD':
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
            for idx in xrange(len(child)-1, -1, -1):
                self.FoldTree(child, idx)
            if 'Fn' in self.symtab[0][node['name']]:
                if all(arg['nt'] == 'CONST' for arg in child):
                    # Call it
                    fn = self.symtab[0][node['name']]['Fn']
                    value = fn(*tuple(arg['value'] for arg in child))
                    if not self.foldtabs and isinstance(value, unicode) and '\t' in value:
                        warning('WARNING: Tab in function result and foldtabs option not used.')
                        return
                    parent[index] = {'nt':'CONST', 't':node['t'], 'value':value}
                elif node['name'] == 'llGetListLength' and child[0]['nt'] == 'IDENT':
                    # Convert llGetListLength(ident) to (ident != [])
                    node = {'nt':'CONST', 't':'list', 'value':[]}
                    node = {'nt':'!=', 't':'list', 'ch':[child[0], node]}
                    parent[index] = {'nt':'()', 't':'list', 'ch':[node]}
            return

        if nt in ('PRINT', 'EXPR'):
            self.FoldTree(child, 0)
            return

        if nt in ('VECTOR', 'ROTATION', 'LIST'):
            isconst = True
            for idx in xrange(len(child)-1, -1, -1):
                self.FoldTree(child, idx)
                if child[idx]['nt'] != 'CONST':
                    isconst = False
            if isconst:
                value = [elem['value'] for elem in child]
                if nt == 'VECTOR':
                    value = lslfuncs.Vector([lslfuncs.ff(x) for x in value])
                elif nt == 'ROTATION':
                    value = lslfuncs.Quaternion([lslfuncs.ff(x) for x in value])
                parent[index] = {'nt':'CONST', 't':node['t'], 'value':value}
            return

        if nt == 'STDEF':
            for idx in xrange(len(child)):
                self.FoldTree(child, idx)
            return

        if nt in ('{}', 'FNDEF'):
            idx = 0
            while idx < len(child):
                self.FoldTree(child, idx)
                self.FoldStmt(child, idx)
                if child[idx]['nt'] == ';' \
                     or nt == '{}' and child[idx]['nt'] == '{}' and not child[idx]['ch']:
                    del child[idx]
                else:
                    if 'StSw' in child[idx]:
                        node['StSw'] = True
                    idx += 1
            return

        if nt == 'IF':
            self.FoldTree(child, 0)
            self.FoldCond(child, 0)
            if child[0]['nt'] == 'CONST':
                # We might be able to remove one of the branches.
                if lslfuncs.cond(child[0]['value']):
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
                        if len(child) > 2:
                            del child[2] # Delete ELSE if present
                        child[0].update({'t':'integer', 'value':-1})
                    else:
                        self.FoldStmt(child, 1)
                        parent[index] = child[1]
                elif len(child) > 2:
                    self.FoldTree(child, 2)
                    self.FoldStmt(child, 2)
                    parent[index] = child[2]
                else:
                    # No ELSE branch, replace the statement with an empty one.
                    parent[index] = {'nt':';', 't':None}
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
            return

        if nt == 'WHILE':
            self.FoldTree(child, 0)
            self.FoldCond(child, 0)
            if child[0]['nt'] == 'CONST':
                # See if the whole WHILE can be eliminated.
                if lslfuncs.cond(child[0]['value']):
                    # Endless loop which must be kept.
                    # First, replace the constant.
                    child[0].update({'t':'integer', 'value':-1})
                    # Recurse on the statement.
                    self.FoldTree(child, 1)
                    self.FoldStmt(child, 1)
                else:
                    # Can be removed.
                    parent[index] = {'nt':';', 't':None}
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
                if lslfuncs.cond(child[1]['value']):
                    # Endless loop. Replace the constant.
                    child[1].update({'t':'integer', 'value':-1})
                else:
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
                # They are expressions, no declarations or labels allowed, but
                # it feels creepy.
                if lslfuncs.cond(child[1]['value']):
                    # Endless loop. Just replace the constant and traverse the rest.
                    child[1].update({'t':'integer', 'value':-1})
                    self.FoldTree(child, 3)
                    self.FoldStmt(child, 3)
                    self.FoldAndRemoveEmptyStmts(child[2]['ch'])
                elif child[0]['ch']:
                    # Convert expression list to code block.
                    exprlist = []
                    for expr in child[0]['ch']:
                        # Fold into expression statements.
                        exprlist.append({'nt':'EXPR', 't':expr['t'], 'ch':[expr]})
                    # returns type None, as FOR does
                    parent[index] = {'nt':'{}', 't':None, 'ch':exprlist}
                else:
                    parent[index] = {'nt':';', 't':None}
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
                # original attached to the folded node and use it in the output.
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
            else:
                # Add assignment if vector, rotation or float.
                if node['t'] in ('float', 'vector', 'rotation'):
                    typ = node['t']
                    node['ch'] = [{'nt':'CONST', 't':typ, 'value':
                        0.0 if typ == 'float' else
                        lslfuncs.ZERO_VECTOR if typ == 'vector' else
                        lslfuncs.ZERO_ROTATION}]
            return

        if nt == 'STSW':
            node['StSw'] = True
            return

        if nt in self.ignored_stmts:
            return

        assert False, 'Internal error: This should not happen,' \
            ' node type = ' + nt # pragma: no cover

    def IsValidGlobalConstant(self, decl):
        if 'ch' not in decl:
            return True
        expr = decl['ch'][0]
        if expr['nt'] in ('CONST', 'IDENT'):
            return True
        if expr['nt'] not in ('VECTOR', 'ROTATION', 'LIST'):
            return False
        return all(elem['nt'] in ('CONST', 'IDENT') for elem in expr['ch'])

    def optimize(self, treesymtab, options = ('optimize',)):
        """Optimize the symbolic table symtab in place. Requires a table of
        predefined functions for folding constants.
        """

        if 'optimize' not in options:
            return treesymtab

        self.foldtabs = 'foldtabs' in options

        self.shrinknames = 'shrinknames' in options

        tree, symtab = self.tree, self.symtab = treesymtab

        self.globalmode = False

        # Constant folding pass. It does some other optimizations along the way.
        for idx in xrange(len(tree)):
            if tree[idx]['nt'] == 'DECL':
                self.globalmode = True
                self.FoldTree(tree, idx)
                self.globalmode = False
                if not self.IsValidGlobalConstant(tree[idx]):
                    warning('WARNING: Expression does not resolve to a single constant.')
            else:
                self.FoldTree(tree, idx)

        if 'shrinknames' in options:
            self.ShrinkNames()

        #self.RemoveDeadCode()

        treesymtab = (self.tree, self.symtab)
        del self.tree
        del self.symtab
        return treesymtab
