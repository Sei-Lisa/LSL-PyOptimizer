
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

    ignored_stmts = frozenset(('V++','V--','--V','++V',';','STATE','JUMP','@'))

    def FoldAndRemoveEmptyStmts(self, lst):
        """Utility function for elimination of useless expressions in FOR"""
        idx = 0
        while idx < len(lst):
            self.FoldTree(lst, idx)
            self.FoldStmt(lst, idx)
            # If eliminated, it must be totally removed. A ';' won't do.
            if lst[idx]['node'] == ';':
                del lst[idx]
            else:
                idx += 1

    def FoldStmt(self, parent, index):
        """If the statement is a constant or an identifier, remove it as it does
        nothing.
        """
        # Ideally this should consider side effect analysis of the whole thing.
        if parent[index]['node'] in ('CONST', 'IDENT', 'FIELD'):
            parent[index] = {'node':';','type':None}

    def FoldTree(self, parent, index):
        """Recursively traverse the tree to fold constants, changing it in
        place.

        Also optimizes away IF, WHILE, etc.
        """
        code = parent[index]
        if code is None: return # Deleted statement
        node = code['node']
        child = code['br'] if 'br' in code else None

        if node == 'CONST':
            # Job already done
            return

        if node == 'CAST':
            self.FoldTree(child, 0)
            if child[0]['node'] == 'CONST':
                # Enable key constants. We'll typecast them back on output, but
                # this enables some optimizations.
                #if code['type'] != 'key': # key constants not possible

                    parent[index] = {'node':'CONST', 'type':code['type'],
                        'value':lslfuncs.typecast(
                            child[0]['value'], self.LSL2PythonType[code['type']])}
            return

        if node == 'NEG':
            self.FoldTree(child, 0)
            if child[0]['node'] == 'CONST':
                code = parent[index] = child[0]
                code['value'] = lslfuncs.neg(code['value'])
            return

        if node == '!':
            self.FoldTree(child, 0)
            if child[0]['node'] == 'CONST':
                code = parent[index] = child[0]
                code['value'] = int(not code['value'])
            return

        if node == '~':
            self.FoldTree(child, 0)
            if child[0]['node'] == 'CONST':
                code = parent[index] = child[0]
                code['value'] = ~code['value']
            return

        if node == '()':
            self.FoldTree(child, 0)
            if child[0]['node'] in ('CONST', 'VECTOR', 'ROTATION', 'LIST',
               'IDENT', 'FIELD', 'V++', 'V--', 'FUNCTION', 'PRINT'):
                # Child is an unary postfix expression; parentheses are
                # redundant and can be removed safely. Not strictly an
                # optimization but it helps keep the output tidy-ish a bit.
                # It's not done in general (e.g. (a * b) + c does not need
                # parentheses but these are not eliminated). Only the cases
                # like (myvar) are simplified.
                parent[index] = child[0]
            return

        if node in self.binary_ops:
            # RTL evaluation
            self.FoldTree(child, 1)
            self.FoldTree(child, 0)
            if child[0]['node'] == child[1]['node'] == 'CONST':
                op = node
                op1 = child[0]['value']
                op2 = child[1]['value']
                if op == '+':
                    result = lslfuncs.add(op1, op2)
                elif op == '-':
                    result = lslfuncs.sub(op1, op2)
                elif op == '*':
                    result = lslfuncs.mul(op1, op2)
                elif op == '/':
                    result = lslfuncs.div(op1, op2)
                elif op == '%':
                    result = lslfuncs.mod(op1, op2)
                elif op == '<<':
                    result = lslfuncs.S32(op1 << (op2 & 31))
                elif op == '>>':
                    result = lslfuncs.S32(op1 >> (op2 & 31))
                elif op == '==' or op == '!=':
                    result = lslfuncs.compare(op1, op2, Eq = (op == '=='))
                elif op in ('<', '<=', '>', '>='):
                    if op in ('>', '<='):
                        result = lslfuncs.less(op2, op1)
                    else:
                        result = lslfuncs.less(op1, op2)
                    if op in ('>=', '<='):
                        result = 1-result
                elif op == '|':
                    result = op1 | op2
                elif op == '^':
                    result = op1 ^ op2
                elif op == '&':
                    result = op1 & op2
                elif op == '||':
                    result = int(op1 or op2)
                elif op == '&&':
                    result = int(op1 and op2)
                else:
                    raise Exception(u'Internal error: Operator not found: ' + op.decode('utf8')) # pragma: no cover
                parent[index] = {'node':'CONST', 'type':code['type'], 'value':result}
            elif node == '-' and child[0]['type'] in ('integer', 'float') \
                             and child[1]['type'] in ('integer', 'float'):
                # Change - to + - for int/float
                if child[1]['node'] == 'CONST':
                    if child[1]['value'] == 0:
                        parent[index] = child[0]
                    else:
                        code['node'] = '+'
                        child[1]['value'] = lslfuncs.neg(child[1]['value'])
                #TODO: Implement to transform 0-x into -x: elif child[0]['node'] == 'CONST':
                else:
                    code['node'] = '+'
                    child[1] = {'node':'NEG', 'type':child[1]['type'], 'br':[child[1]]}
            elif node == '<<' and child[1]['node'] == 'CONST':
                # Transforming << into multiply saves some bytes.
                if child[1]['value'] & 31:
                    # x << 3  -->  x * 8
                    # Do we need parentheses for *? It depends on x
                    # e.g. x+3<<3 needs parentheses when converted to (x+3)*8
                    if child[0]['node'] in ('+', '-', 'NEG'): # operands with priority between * and << #TODO: CHECK
                        child[0] = {'node':'()', 'type':child[0]['type'], 'br':[child[0]]}
                    # we have {<<, something, {CONST n}}, transform into {*, something, {CONST n}}
                    code['node'] = '*'
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

        if node in self.assign_ops:
            # TODO: Eliminate redundant operations, e.g. a += 0; etc.
            # Consider also e.g. x -= 1 or x -= a transforming it into +=.
            # Actually just consider transforming the whole thing into a
            # regular assignment, as there are no gains and it simplifies the
            # optimization.
            self.FoldTree(child, 1)
            return

        if node == 'IDENT' or node == 'FLD':
            if self.globalmode:
                ident = code if node == 'IDENT' else child[0]
                # Resolve constant values so they can be optimized
                sym = self.symtab[ident['scope']][ident['name']]

                defn = self.tree[sym['Loc']]
                assert defn['name'] == ident['name']

                # Assume we already were there
                if 'br' in defn:
                    val = defn['br'][0]
                    if val['node'] != 'CONST' or ident['type'] in ('list', 'key'):
                        return
                else:
                    val = {'node':'CONST', 'type':defn['type'],
                           'value':self.DefaultValues[defn['type']]}
                if node == 'FLD':
                    val = {'node':'CONST', 'type':'float',
                        'value':val['value']['xyzs'.index(code['fld'])]}
                parent[index] = val
            return

        if node == 'FNCALL':
            for idx in xrange(len(child)-1, -1, -1):
                self.FoldTree(child, idx)
            if code['name'] in self.symtab[0]:
                fn = self.symtab[0][code['name']]['Loc']
                if fn is not None and type(fn) != int and all(arg['node'] == 'CONST' for arg in child):
                    # Call it
                    value = fn(*tuple(arg['value'] for arg in child))
                    if not self.foldtabs and isinstance(value, unicode) and '\t' in value:
                        warning('WARNING: Tab in function result and foldtabs option not used.')
                        return
                    parent[index] = {'node':'CONST', 'type':code['type'], 'value':value}
            return

        if node == 'PRINT':
            # useless but who knows
            self.FoldTree(child, 0)
            return

        if node in ('VECTOR', 'ROTATION', 'LIST'):
            isconst = True
            for idx in xrange(len(child)-1, -1, -1):
                self.FoldTree(child, idx)
                if child[idx]['node'] != 'CONST':
                    isconst = False
            if isconst:
                value = [elem['value'] for elem in child]
                if node == 'VECTOR':
                    value = lslfuncs.Vector([lslfuncs.ff(x) for x in value])
                elif node == 'ROTATION':
                    value = lslfuncs.Quaternion([lslfuncs.ff(x) for x in value])
                parent[index] = {'node':'CONST', 'type':code['type'], 'value':value}
            return

        if node in ('{}', 'FNDEF', 'STATEDEF'):
            for idx in xrange(len(child)):
                self.FoldTree(child, idx)
                self.FoldStmt(child, idx)
            return

        if node == 'IF':
            self.FoldTree(child, 0)
            if child[0]['node'] == 'CONST':
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
                    parent[index] = {'node':';', 'type':None}
            else:
                self.FoldTree(child, 1)
                self.FoldStmt(child, 1)
                if len(child) > 2:
                    self.FoldTree(child, 2)
                    self.FoldStmt(child, 2)
            return

        if node == 'WHILE':
            self.FoldTree(child, 0)
            if child[0]['node'] == 'CONST':
                # See if the whole WHILE can be eliminated.
                if lslfuncs.cond(child[0]['value']):
                    # Endless loop which must be kept.
                    # First, replace the constant.
                    child[0].update({'type':'integer', 'value':1})
                    # Recurse on the statement.
                    self.FoldTree(child, 1)
                    self.FoldStmt(child, 1)
                else:
                    # Can be removed.
                    parent[index] = {'node':';', 'type':None}
            else:
                self.FoldTree(child, 1)
                self.FoldStmt(child, 1)
            return

        if node == 'DO':
            self.FoldTree(child, 0) # This one is always executed.
            self.FoldStmt(child, 0)
            self.FoldTree(child, 1)
            # See if the latest part is a constant.
            if child[1]['node'] == 'CONST':
                if lslfuncs.cond(child[1]['value']):
                    # Endless loop. Replace the constant.
                    child[1].update({'type':'integer', 'value':1})
                else:
                    # Only one go. Replace with the statement(s).
                    parent[index] = child[0]
            return

        if node == 'FOR':
            assert child[0]['node'] == 'EXPRLIST'
            assert child[2]['node'] == 'EXPRLIST'
            self.FoldAndRemoveEmptyStmts(child[0]['br'])

            self.FoldTree(child, 1) # Condition.
            if child[1]['node'] == 'CONST':
                # FOR is delicate. It can have multiple expressions at start.
                # And if there is more than one, these expressions will need a
                # new block, which means new scope, which is dangerous.
                # They are expressions, no declarations or labels allowed, but
                # it feels creepy.
                if lslfuncs.cond(child[1]['value']):
                    # Endless loop. Just replace the constant and traverse the rest.
                    child[1].update({'type':'integer', 'value':1})
                    self.FoldAndRemoveEmptyStmts(child[2]['br'])
                    self.FoldTree(child, 3)
                    self.FoldStmt(child, 3)
                elif len(child[0]['br']) > 1:
                    parent[index] = {'node':'{}', 'type':None, 'br':child[0]['br']}
                elif child[0]['br']:
                    parent[index] = child[0]['br'][0]
                else:
                    parent[index] = {'node':';', 'type':None}
            else:
                self.FoldAndRemoveEmptyStmts(child[2]['br'])
                self.FoldTree(child, 3)
                self.FoldStmt(child, 3)
            return

        if node == 'RETURN':
            if child:
                self.FoldTree(child, 0)
            return

        if node == 'DECL':
            # The expression code is elsewhere.
            if child:
                self.FoldTree(child, 0)
                # TODO: Remove assignment if integer zero.
            else:
                # TODO: Add assignment if vector, rotation or float.
                pass
            return

        if node in self.ignored_stmts:
            return

        raise Exception('Internal error: This should not happen, node = ' + node) # pragma: no cover

    def IsValidGlobalConstant(self, decl):
        if 'br' not in decl:
            return True
        expr = decl['br'][0]
        if expr['node'] in ('CONST', 'IDENT'):
            return True
        if expr['node'] not in ('VECTOR', 'ROTATION', 'LIST'):
            return False
        return all(elem['node'] in ('CONST', 'IDENT') for elem in expr['br'])

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
            if tree[idx]['node'] == 'DECL':
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
