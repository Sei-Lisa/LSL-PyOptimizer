
import lslfuncs
from lslparse import S
import math

CONSTANT = S['CONSTANT']

class optimizer(object):

    # explicitly exclude assignments
    binary_ops = frozenset(('+','-','*','/','%','<<','>>','<','<=','>','>=',
        '==','!=','|','^','&','||','&&'))

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
            code[:] = code[2]

        code0 = code[0]

        if code0 == 'CAST':
            self.FoldTree(code[2])
            if code[2][0] == CONSTANT:
                # Enable key constants. We'll typecast them back on output, but
                # this enables some optimizations.
                #if code[1] != 'key': # key constants not possible
                    code[:] = [CONSTANT, code[1], lslfuncs.typecast(code[2][2])]
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
            if code[2][0] == CONSTANT:
                code[:] = code[2]

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
                        result = not result
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
                    raise Exception(u'Internal error: Operator not found: ' + op.decode('utf8'))
                code[:] = [CONSTANT, code[1], result]
            elif code[0] == '-' and code[2][1] in ('integer', 'float') and code[3][1] in ('integer', 'float'):
                # Change - to + - for int/float
                if code[3][0] == CONSTANT:
                    code[3][2] = lslfuncs.neg(code[3][2])
                else:
                    code[:] = [S['+'], code[1], code[2], [S['NEG'], code[3][1], code[3]]]
            elif code[0] == '<<' and code[3][0] == CONSTANT:
                # Transforming << into multiply saves some bytes.
                if code[2][0] in ('+', '-', 'NEG'): # operands with priority between * and <<
                    code[2] = [S['()'], code[2][1], code[2]]
                code[:] = [S['*'], code[1], code[2], 1<<(code[3][2] & 31)]
            return

        if self.globalmode:
            if code0 == 'IDENT':
                if code[1] != 'key' and self.symtab[code[3]][code[2]][2] is not None:
                    code[:] = [CONSTANT, code[1], self.symtab[code[2]][2]]
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
                # We can fold a vector or rotation field as they are constant.
                assert code[2][0] == 'IDENT'
                value = self.symtab[code[2][3]][code[2][2]][2]
                assert type(value) in (lslfuncs.Vector, lslfuncs.Quaternion)
                code[:] = [CONSTANT, 'float', lslfuncs.ff(value['xyzs'].index(code[3]))]
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

        if code0 == 'DECL':
            # The expression code is elsewhere.
            expr = self.symtab[code[3]][code[2]][2]
            if expr is not None:
                self.FoldTree(expr)

    def Fold(self, code, IsGlobal = True):
        assert type(code[2]) == tuple
        tree = list(code[2])
        self.globalmode = IsGlobal and len(code) == 3
        self.FoldTree(tree)
        # As a special case, we fold the constants that are keys,
        # because the folder

        # TODO: Move this to a post-folding optimization.
        #       Reasons: (1) it doesn't optimize deep constants and
        #       (2) it disturbs normal folding if done on the fly.
        # Mono optimization: (integer)-5 and (float)-3.0 is cheaper.
        if not IsGlobal and tree[0] == 'CONSTANT':
            # Disabled because we print integer constants in hex anyway.
            #if tree[1] == 'integer' and tree[2] < 0:
            #    tree[:] = [S['CAST'], 'integer', tree]
            if tree[1] == 'float' and tree[2] < 0.0 and not math.isinf(tree[2]):
                tree[:] = [S['CAST'], 'float', tree]

        if type(code) == tuple:
            code = list(code)
            code[2] = tuple(tree)
            code = tuple(code)
        else:
            assert False
            code[2] = tuple(tree)

        del self.globalmode

    def optimize(self, symtab, functions):
        """Optimize the symbolic table symtab in place. Uses a table of
        predefined functions for folding constants.
        """
        self.functions = functions
        self.symtab = symtab

        # Fold constants etc.
        for name in symtab[0]:
            if name == -1:
                continue
            entry = symtab[0][name]
            if entry[1] == 'State':
                for event in entry[2]:
                    self.Fold(entry[2][event], False)
            elif type(entry[2]) == tuple:
                self.Fold(entry) # global
