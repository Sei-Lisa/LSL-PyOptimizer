
import lslfuncs
from lslparse import warning

class optimizer(object):

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
        if parent[index]['nt'] in ('CONST', 'IDENT', 'FIELD'):
            parent[index] = {'nt':';','t':None}

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
            if child[0]['nt'] == 'CONST':
                node = parent[index] = child[0]
                node['value'] = lslfuncs.neg(node['value'])
            return

        if nt == '!':
            self.FoldTree(child, 0)
            if child[0]['nt'] == 'CONST':
                node = parent[index] = child[0]
                node['value'] = int(not node['value'])
            return

        if nt == '~':
            self.FoldTree(child, 0)
            if child[0]['nt'] == 'CONST':
                node = parent[index] = child[0]
                node['value'] = ~node['value']
            return

        if nt == '()':
            self.FoldTree(child, 0)
            if child[0]['nt'] in ('CONST', 'VECTOR', 'ROTATION', 'LIST',
               'IDENT', 'FIELD', 'V++', 'V--', 'FUNCTION', 'PRINT'):
                # Child is an unary postfix expression; parentheses are
                # redundant and can be removed safely. Not strictly an
                # optimization but it helps keep the output tidy-ish a bit.
                # It's not done in general (e.g. (a * b) + c does not need
                # parentheses but these are not eliminated). Only the cases
                # like (myvar) are simplified.
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
                    result = int(op1 or op2)
                elif nt == '&&':
                    result = int(op1 and op2)
                else:
                    raise Exception(u'Internal error: Operator not found: ' + nt.decode('utf8')) # pragma: no cover
                parent[index] = {'nt':'CONST', 't':node['t'], 'value':result}
            elif nt == '-' and child[0]['t'] in ('integer', 'float') \
                             and child[1]['t'] in ('integer', 'float'):
                # Change - to + - for int/float
                if child[1]['nt'] == 'CONST':
                    if child[1]['value'] == 0:
                        parent[index] = child[0]
                    else:
                        node['nt'] = '+'
                        child[1]['value'] = lslfuncs.neg(child[1]['value'])
                #TODO: Implement to transform 0-x into -x: elif child[0]['nt'] == 'CONST':
                else:
                    node['nt'] = '+'
                    child[1] = {'nt':'NEG', 't':child[1]['t'], 'ch':[child[1]]}
            elif nt == '<<' and child[1]['nt'] == 'CONST':
                # Transforming << into multiply saves some bytes.
                if child[1]['value'] & 31:
                    # x << 3  -->  x * 8
                    # Do we need parentheses for *? It depends on x
                    # e.g. x+3<<3 needs parentheses when converted to (x+3)*8
                    if child[0]['nt'] in ('+', '-', 'NEG'): # operands with priority between * and << #TODO: CHECK
                        child[0] = {'nt':'()', 't':child[0]['t'], 'ch':[child[0]]}
                    # we have {<<, something, {CONST n}}, transform into {*, something, {CONST n}}
                    node['nt'] = '*'
                    child[1]['value'] = 1<<(child[1]['value'] & 31)
                else: # x << 0  -->  x
                    parent[index] = child[0]
            else:
                pass # TODO: Eliminate redundancy (x+0, x*1, x*-1, v+ZERO_VECTOR, perhaps x-1=~-x, etc.)
                    # Include != to ^  and || to | and maybe && to &
                    # Note some cases e.g. x*0 can't be optimized away without side-effect analysis.
                    # But some cases like %1 can be turned into *0 to save bytes.
                    # Turn also % (power of 2) into & mask (oops, nope, negative doesn't work)
                    # Maybe turn != -1 into ~ in if()'s.
            return

        if nt in self.assign_ops:
            # TODO: Eliminate redundant operations, e.g. a += 0; etc.
            # Consider also e.g. x -= 1 or x -= a transforming it into +=.
            # Actually just consider transforming the whole thing into a
            # regular assignment, as there are no gains and it simplifies the
            # optimization.
            self.FoldTree(child, 1)
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
                    if val['nt'] != 'CONST' or ident['t'] in ('list', 'key'):
                        return
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
            if 'fn' in self.symtab[0][node['name']]:
                fn = self.symtab[0][node['name']]['fn']
                if all(arg['nt'] == 'CONST' for arg in child):
                    # Call it
                    value = fn(*tuple(arg['value'] for arg in child))
                    if not self.foldtabs and isinstance(value, unicode) and '\t' in value:
                        warning('WARNING: Tab in function result and foldtabs option not used.')
                        return
                    parent[index] = {'nt':'CONST', 't':node['t'], 'value':value}
            return

        if nt == 'PRINT':
            # useless but who knows
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

        if nt in ('{}', 'FNDEF', 'STDEF'):
            for idx in xrange(len(child)):
                self.FoldTree(child, idx)
                self.FoldStmt(child, idx)
            return

        if nt == 'IF':
            self.FoldTree(child, 0)
            if child[0]['nt'] == 'CONST':
                # We can remove one of the branches safely.
                if lslfuncs.cond(child[0]['value']):
                    self.FoldTree(child, 1)
                    parent[index] = child[1]
                    self.FoldStmt(child, 1)
                elif len(child) > 2:
                    self.FoldTree(child, 2)
                    parent[index] = child[2]
                    self.FoldStmt(child, 2)
                else:
                    # No ELSE branch, replace the statement with an empty one.
                    parent[index] = {'nt':';', 't':None}
            else:
                self.FoldTree(child, 1)
                self.FoldStmt(child, 1)
                if len(child) > 2:
                    self.FoldTree(child, 2)
                    self.FoldStmt(child, 2)
            return

        if nt == 'WHILE':
            self.FoldTree(child, 0)
            if child[0]['nt'] == 'CONST':
                # See if the whole WHILE can be eliminated.
                if lslfuncs.cond(child[0]['value']):
                    # Endless loop which must be kept.
                    # First, replace the constant.
                    child[0].update({'t':'integer', 'value':1})
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
            # See if the latest part is a constant.
            if child[1]['nt'] == 'CONST':
                if lslfuncs.cond(child[1]['value']):
                    # Endless loop. Replace the constant.
                    child[1].update({'t':'integer', 'value':1})
                else:
                    # Only one go. Replace with the statement(s).
                    parent[index] = child[0]
            return

        if nt == 'FOR':
            assert child[0]['nt'] == 'EXPRLIST'
            assert child[2]['nt'] == 'EXPRLIST'
            self.FoldAndRemoveEmptyStmts(child[0]['ch'])

            self.FoldTree(child, 1) # Condition.
            if child[1]['nt'] == 'CONST':
                # FOR is delicate. It can have multiple expressions at start.
                # And if there is more than one, these expressions will need a
                # new block, which means new scope, which is dangerous.
                # They are expressions, no declarations or labels allowed, but
                # it feels creepy.
                if lslfuncs.cond(child[1]['value']):
                    # Endless loop. Just replace the constant and traverse the rest.
                    child[1].update({'t':'integer', 'value':1})
                    self.FoldAndRemoveEmptyStmts(child[2]['ch'])
                    self.FoldTree(child, 3)
                    self.FoldStmt(child, 3)
                elif len(child[0]['ch']) > 1:
                    parent[index] = {'nt':'{}', 't':None, 'ch':child[0]['ch']}
                elif child[0]['ch']:
                    parent[index] = child[0]['ch'][0]
                else:
                    parent[index] = {'nt':';', 't':None}
            else:
                self.FoldAndRemoveEmptyStmts(child[2]['ch'])
                self.FoldTree(child, 3)
                self.FoldStmt(child, 3)
            return

        if nt == 'RETURN':
            if child:
                self.FoldTree(child, 0)
            return

        if nt == 'DECL':
            # The expression code is elsewhere.
            if child:
                self.FoldTree(child, 0)
                # TODO: Remove assignment if integer zero.
            else:
                # TODO: Add assignment if vector, rotation or float.
                pass
            return

        if nt in self.ignored_stmts:
            return

        assert False, 'Internal error: This should not happen, node type = ' + nt # pragma: no cover

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
            return

        self.foldtabs = 'foldtabs' in options

        # TODO: Add option to handle local jumps properly.

        tree, symtab = self.tree, self.symtab = treesymtab

        self.globalmode = False

        # Constant folding pass. It does some other optimizations along the way.
        for idx in xrange(len(tree)):
            if tree[idx]['nt'] == 'DECL':
                self.globalmode = True
                self.FoldTree(tree, idx)
                self.globalmode = False
                if not self.IsValidGlobalConstant(tree[idx]):
                    warning('WARNING: Expression does not collapse to a single constant.')
            else:
                self.FoldTree(tree, idx)

        treesymtab = (self.tree, self.symtab)
        del self.tree
        del self.symtab
        return treesymtab
