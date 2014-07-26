
import lslfuncs
from lslparse import S
import math

CONSTANT = S['CONSTANT']

class optimizer(object):

    # explicitly exclude assignments
    binary_ops = frozenset(('+','-','*','/','%','<<','>>','<','<=','>','>=',
        '==','!=','|','^','&','||','&&'))

    def FoldTree(self, code):
        """Recursively traverse the tree to  fold constants, changing the tree
        in place.
        """
        while code[0] == 'EXPR':
            code[:] = code[2]

        if code[0] == 'CAST':
            self.FoldTree(code[2])
            if code[2][0] == CONSTANT:
                if code[1] != 'key': # key constants not possible
                    code[:] = [CONSTANT, code[1], lslfuncs.typecast(code[2][2])]
            return

        if code[0] == '-':
            self.FoldTree(code[3])
            if code[3][1] in ('integer', 'float'): # no gain otherwise
                if code[3][0] == CONSTANT:
                    code[3][2] = -code[3][2]
                else:
                    code[:] = ['+', code[1], code[2], [S['NEG'], code[3][1], code[3]]]
                    self.FoldTree(code[2])
            else:
                self.FoldTree(code[2])
                if code[2][0] == code[3][0] == CONSTANT:
                    code[:] = ['-', code[1], lslfuncs.sub(code[2][2], code[3][2])]
            # Fall through to optimize it right away as addition

        if code[0] in self.binary_ops:
            # RTL evaluation
            self.FoldTree(code[3])
            self.FoldTree(code[2])
            if code[2][0] == code[3][0] == CONSTANT:
                code[:] = [CONSTANT, code[1], lslfuncs.add(code[2][2], code[3][2])]
            return

        if self.globalmode:
            if code[0] == 'IDENT':
                if code[1] != 'key' and self.symtab[code[2]][2] is not None:
                    code[:] = [CONSTANT, code[1], self.symtab[code[2]][2]]
                return

        if code[0] == 'FUNCTION':
            for x in code[3][::-1]:
                self.FoldTree(x)
            if code[2] in self.functions:
                for x in code[3]:
                    if x[0] != CONSTANT:
                        break
                else:
                    if code[2] in self.functions:
                        # Call it
                        val = self.functions[code[2]](tuple(x[2] for x in code[3]))
                        code[:] = [CONSTANT, code[1], val]
            return

        if code[0] == 'PRINT':
            # useless but who knows
            self.FoldTree(code[2])

        if code[0] == '{}':
            for x in code[2:]:
                self.FoldTree(x)
            return

        if code[0] in ('VECTOR', 'ROTATION', 'LIST'):
            for x in code[:1:-1]:
                self.FoldTree(x)

        if code[0] == 'FIELD':
            self.FoldTree(code[2])
            assert code[2][0] in ('VECTOR', 'ROTATION')
            idx = '--xyzs'.index(code[3])
            if code[2][idx][0] == CONSTANT:
                code[:] = [CONSTANT, 'float', code[2][idx][0]]
            return

    def Fold(self, code, IsGlobal = False):
        assert type(code[2]) == tuple
        tree = list(code[2])
        self.globalmode = IsGlobal
        self.FoldTree(tree)
        # Mono optimization: (integer)-5 and (float)-3.0 is cheaper.
        if not IsGlobal and tree[0] == 'CONSTANT':
            if tree[1] == 'integer' and tree[2] < 0:
                tree[:] = [S['CAST'], 'integer', tree]
            elif tree[1] == 'float' and tree[2] < 0.0 and not math.isinf(tree[2]):
                tree[:] = [S['CAST'], 'float', tree]

        if type(code) == tuple:
            code = list(code)
            code[2] = tuple(tree)
            code = tuple(code)
        else:
            assert False
            code[2] = tuple(tree)

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
                    self.Fold(entry[2][event])
            elif type(entry[2]) == tuple:
                self.Fold(entry, IsGlobal = True) # global
