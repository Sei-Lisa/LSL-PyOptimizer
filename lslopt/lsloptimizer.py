
import lslfuncs
from lslparse import S, warning

CONSTANT = S['CONSTANT']

class optimizer(object):

    # explicitly exclude assignments
    binary_ops = frozenset(('+','-','*','/','%','<<','>>','<','<=','>','>=',
        '==','!=','|','^','&','||','&&'))
    assign_ops = frozenset(('=','+=','-=','*=','/=','%=','&=','|=','^=','<<=','>>='))
    LSL2PythonType = {'integer':int, 'float':float, 'string':unicode, 'key':lslfuncs.Key,
        'vector':lslfuncs.Vector, 'rotation':lslfuncs.Quaternion, 'list':list}

    ignored_stmts = frozenset(('V++','V--','--V','++V',';','STATE','JUMP','@'))

    def FoldAndRemoveEmptyStmts(self, lst):
        """Utility function for elimination of useless expressions in FOR"""
        x = 0
        while x < len(lst):
            self.FoldTree(lst[x])
            self.FoldStmt(lst[x])
            # If eliminated, it must be totally removed. A ';' won't do.
            if lst[x][0] == ';':
                del lst[x]
            else:
                x += 1

    def FoldStmt(self, code):
        """If the statement is a constant or an identifier, remove it as it does
        nothing.
        """
        # Ideally this should consider side effect analysis of the whole thing.
        if code[0] in (CONSTANT, 'IDENT', 'FIELD'):
            code[:] = [S[';'], None]
        else:
            code[:] = code

    def FoldTree(self, code):
        """Recursively traverse the tree to fold constants, changing it in
        place.

        Also optimizes away IF, WHILE, etc.
        """
        while code[0] == 'EXPR':
            if type(code) == tuple:
                # just enter
                code = code[2]
            else:
                # unfold
                code[:] = code[2]

        code0 = code[0]

        if code0 == CONSTANT:
            # Job already done
            return

        if code0 == 'CAST':
            self.FoldTree(code[2])
            if code[2][0] == CONSTANT:
                # Enable key constants. We'll typecast them back on output, but
                # this enables some optimizations.
                #if code[1] != 'key': # key constants not possible

                    code[:] = [CONSTANT, code[1], lslfuncs.typecast(code[2][2], self.LSL2PythonType[code[1]])]
            return

        if code0 == 'NEG':
            self.FoldTree(code[2])
            if code[2][0] == CONSTANT:
                code[:] = [CONSTANT, code[1], lslfuncs.neg(code[2][2])]
            return

        if code0 == '!':
            self.FoldTree(code[2])
            if code[2][0] == CONSTANT:
                code[:] = [CONSTANT, code[1], int(not code[2][2])]
            return

        if code0 == '~':
            self.FoldTree(code[2])
            if code[2][0] == CONSTANT:
                code[:] = [CONSTANT, code[1], ~code[2][2]]
            return

        if code0 == '()':
            self.FoldTree(code[2])
            if code[2][0] in (CONSTANT, 'VECTOR', 'ROTATION', 'LIST',
               'IDENT', 'FIELD', 'V++', 'V--', 'FUNCTION', 'PRINT'):
                # Child is an unary postfix expression; parentheses can be
                # removed safely.
                code[:] = code[2]
            return

        if code0 in self.binary_ops:
            # RTL evaluation
            self.FoldTree(code[3])
            self.FoldTree(code[2])
            if code[2][0] == code[3][0] == CONSTANT:
                op = code0
                op1 = code[2][2]
                op2 = code[3][2]
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
                    result = lslfuncs.compare(op1, op2, op == '==')
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
                code[:] = [CONSTANT, code[1], result]
            elif code[0] == '-' and code[2][1] in ('integer', 'float') and code[3][1] in ('integer', 'float'):
                # Change - to + - for int/float
                if code[3][0] == CONSTANT:
                    if code[3][2] == 0:
                        code[:] = code[2]
                    else:
                        code[0] = S['+']
                        code[3][2] = lslfuncs.neg(code[3][2])
                else:
                    code[:] = [S['+'], code[1], code[2], [S['NEG'], code[3][1], code[3]]]
            elif code[0] == '<<' and code[3][0] == CONSTANT:
                # Transforming << into multiply saves some bytes.
                if code[2][0] in ('+', '-', 'NEG'): # operands with priority between * and <<
                    code[2] = [S['()'], code[2][1], code[2]]
                if not (code[3][2] & 31):
                    code[:] = code[2]
                else:
                    code[:] = [S['*'], code[1], code[2], [CONSTANT, 'integer', 1<<(code[3][2] & 31)]]
            else:
                pass # TODO: Eliminate redundancy (x+0, x*1, x*-1, v+ZERO_VECTOR, perhaps x-1=~-x, etc.)
                    # Include != to ^  and || to | and maybe && to &
                    # Note some cases e.g. x*0 can't be optimized away without side-effect analysis.
                    # But some cases like %1 can be turned into *0 to save bytes.
                    # Turn also % (power of 2) into & mask (oops, nope, negative doesn't work)
                    # Maybe turn != -1 into ~ in if()'s.
            return

        if code0 in self.assign_ops:
            # TODO: Eliminate redundant operations, e.g. a += 0; etc.
            # Consider also e.g. x -= 1 or x -= a transforming it into +=.
            self.FoldTree(code[3])
            return

        if code0 == 'IDENT':
            if self.globalmode:
                val = self.symtab[code[3]][code[2]][2]
                if val is not None:
                    if type(val) == tuple:
                        # Infinite recursion is prevented at the parser level, by
                        # not allowing forward globals in global var definitions.
                        self.FoldTree(val)
                        if val[0] != 'EXPR' or val[2][0] != CONSTANT:
                            return
                        val = val[2][2]
                    if code[1] != 'key' and val is not None:
                        code[:] = [CONSTANT, code[1], val]
            return

        if code0 == 'FUNCTION':
            for x in code[3][::-1]:
                self.FoldTree(x)
            if code[2] in self.functions and self.functions[code[2]][2] is not None:
                for x in code[3]:
                    if x[0] != CONSTANT:
                        break
                else:
                    # Call it
                    val = self.functions[code[2]][2](*tuple(x[2] for x in code[3]))
                    if not self.foldtabs and isinstance(val, unicode) and '\t' in val:
                        warning('WARNING: Tab in function result and foldtabs option not used.')
                        return
                    code[:] = [CONSTANT, code[1], val]
            return

        if code0 == 'PRINT':
            # useless but who knows
            self.FoldTree(code[2])
            return

        if code0 in ('VECTOR', 'ROTATION', 'LIST'):
            isconst = True
            for x in code[:1:-1]:
                self.FoldTree(x)
                if x[0] != CONSTANT:
                    isconst = False
            if isconst:
                value = [x[2] for x in code[2:]]
                if code0 == 'VECTOR':
                    value = lslfuncs.Vector([lslfuncs.ff(x) for x in value])
                elif code0 == 'ROTATION':
                    value = lslfuncs.Quaternion([lslfuncs.ff(x) for x in value])
                code[:] = [CONSTANT, code[1], value]
            return

        if code0 == 'FIELD':
            if self.globalmode:
                # We can fold a global vector or rotation field as they are
                # constant, but that involves resolving the symbols that aren't
                # already.
                assert code[2][0] == 'IDENT' # that should be granted
                glob = self.symtab[code[2][3]][code[2][2]]
                origin = glob[2]
                if type(origin) == tuple:
                    # We have to do this due to not processing globals in order.
                    self.FoldTree(origin)
                    # Unfold constant expression
                    if origin[0] != 'EXPR' or origin[2][0] != CONSTANT:
                        return
                    origin = origin[2][2]
                    self.symtab[code[2][3]][code[2][2]] = glob[:2] + (origin,) + glob[3:]
                if type(origin) not in (lslfuncs.Vector, lslfuncs.Quaternion):
                    # Precondition not met
                    return # pragma: no cover
                code[:] = [CONSTANT, 'float', lslfuncs.ff(origin['xyzs'.index(code[3])])]
            return

        if code0 == '{}':
            for x in code[2:]:
                self.FoldTree(x)
                self.FoldStmt(x)
            return

        if code0 == 'IF':
            self.FoldTree(code[2])
            if code[2][0] == CONSTANT:
                # We can remove one of the branches safely.
                if lslfuncs.cond(code[2][2]):
                    self.FoldTree(code[3])
                    code[:] = code[3]
                    self.FoldStmt(code)
                elif len(code) > 4:
                    self.FoldTree(code[4])
                    code[:] = code[4]
                    self.FoldStmt(code)
                else:
                    # No ELSE branch, replace the statement with an empty one.
                    code[:] = [S[';'], None]
            else:
                self.FoldTree(code[3])
                self.FoldStmt(code[3])
                if len(code) > 4:
                    self.FoldTree(code[4])
                    self.FoldStmt(code[4])
            return

        if code0 == 'WHILE':
            self.FoldTree(code[2])
            if code[2][0] == CONSTANT:
                # See if the whole WHILE can be eliminated.
                if lslfuncs.cond(code[2][2]):
                    # Endless loop which must be kept.
                    # First, replace the constant.
                    code[2][1:2] = [S['integer'], 1]
                    # Recurse on the statement.
                    self.FoldTree(code[3])
                    self.FoldStmt(code[3])
                else:
                    # Can be removed.
                    code[:] = [S[';'], None]
            else:
                self.FoldTree(code[3])
                self.FoldStmt(code[3])
            return

        if code0 == 'DO':
            self.FoldTree(code[2]) # This one is always executed.
            self.FoldStmt(code[2])
            self.FoldTree(code[3])
            # See if the latest part is a constant.
            if code[3][0] == CONSTANT:
                if lslfuncs.cond(code[3][2]):
                    # Endless loop. Replace the constant.
                    code[3][1:2] = [S['integer'], 1]
                else:
                    # Only one go. Replace with the statement(s).
                    code[:] = code[2]
            return

        if code0 == 'FOR':
            self.FoldAndRemoveEmptyStmts(code[2])

            self.FoldTree(code[3]) # Condition.
            if code[3][0] == CONSTANT:
                # FOR is delicate. It can have multiple expressions at start.
                # And if there is more than one, these expressions will need a
                # new block, which means new scope, which is dangerous.
                # They are expressions, no declarations or labels allowed, but
                # it feels creepy.
                if lslfuncs.cond(code[3][2]):
                    # Endless loop. Just replace the constant and traverse the rest.
                    code[3][1:2] = [S['integer'], 1]
                    self.FoldAndRemoveEmptyStmts(code[4])
                    self.FoldTree(code[5])
                    self.FoldStmt(code[5])
                elif len(code[2]) > 1:
                    code[:] = [S['{}'], None] + code[2]
                elif code[2]:
                    code[:] = code[2][0]
                else:
                    code[:] = [S[';'], None]
            else:
                self.FoldAndRemoveEmptyStmts(code[4])
                self.FoldTree(code[5])
                self.FoldStmt(code[5])
            return

        if code0 == 'RETURN':
            if code[2] is not None:
                self.FoldTree(code[2])
            return

        if code0 == 'DECL':
            # The expression code is elsewhere.
            expr = self.symtab[code[3]][code[2]][2]
            # Irrelevant if list or string or key.
            if expr is not None:
                self.FoldTree(expr)
                # TODO: Remove assignment if integer zero.
            else:
                # TODO: Add assignment if vector, rotation or float.
                pass
            return

        if code0 in self.ignored_stmts:
            return

        raise Exception('Internal error: This should not happen, node = ' + code0) # pragma: no cover

    def IsValidGlobalConstant(self, value):
        if value[0] == 'EXPR':
            value = value[2]
        if value[0] not in ('VECTOR', 'ROTATION', 'LIST'):
            return False
        return all(x[0] in (CONSTANT, 'IDENT') for x in value[2:])

    def optimize(self, symtab, functions, options = ('optimize',)):
        """Optimize the symbolic table symtab in place. Requires a table of
        predefined functions for folding constants.
        """

        if 'optimize' not in options:
            return

        self.foldtabs = 'foldtabs' in options

        # TODO: Add option to handle local jumps properly.

        self.functions = functions
        self.symtab = symtab

        self.globalmode = False

        # Fold constants etc.
        for name in symtab[0]:
            if name == -1:
                continue
            entry = symtab[0][name]
            if entry[1] == 'State':
                for event in entry[2]:
                    self.FoldTree(entry[2][event][2])
            elif type(entry[2]) == tuple:
                self.globalmode = len(entry) == 3
                self.FoldTree(entry[2]) # global
                if self.globalmode:
                    val = entry[2]
                    # Unfold constant
                    if val[0] == 'EXPR' and val[2][0] == CONSTANT:
                        symtab[0][name] = entry[:2] + (val[2][2],) + entry[3:]
                    elif not self.IsValidGlobalConstant(val):
                        warning('WARNING: Expression does not collapse to a single constant.')
                self.globalmode = False
