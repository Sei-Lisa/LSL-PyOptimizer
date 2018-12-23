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

# Convert an abstract syntax tree + symbol table back to a script as text.

import lslfuncs
import lslcommon
from lslcommon import Key, Vector, Quaternion, warning
from math import copysign

class outscript(object):

    binary_operands = frozenset(('||','&&','^','|','&','==','!=','<','<=','>',
        '>=','<<','>>','+','-','*','/','%', '=', '+=', '-=', '*=', '/=','%=',
        ))
    extended_assignments = frozenset(('&=', '|=', '^=', '<<=', '>>='))
    unary_operands = frozenset(('NEG', '!', '~'))
    op_priority = {'=':0, '+=':0, '-=':0, '*=':0, '/=':0, '%=':0, '&=':0,
        '|=':0, '^=':0, '<<=':0, '>>=':0,
        '||':1, '&&':1, '|':2, '^':3, '&':4, '==':5, '!=':5,
        '<':6, '<=':6, '>':6, '>=':6, '<<':7, '>>':7, '+':8, '-':8,# 'NEG':8,
        '*':9, '/':9, '%':9}#, '!':10, '~':10, '++':10, '--':10, }
    assignment_ops = ('=', '+=', '-=', '*=', '/=','%=')

    def Value2LSL(self, value):
        tvalue = type(value)
        if tvalue in (Key, unicode):
            pfx = sfx = ''
            if type(value) == Key:
                # Constants of type key can not be represented
                #raise lslfuncs.ELSLTypeMismatch
                # Actually they can be the result of folding.
                # On second thought, if we report the error, the location info
                # is lost. So we emit a warning instead, letting the compiler
                # report the error in the generated source.
                if self.globalmode and self.listmode:
                    warning(u"Illegal combo: Key type inside a global list")
                if self.listmode or not self.globalmode:
                    if self.globalmode:
                        pfx = '(key)'
                    else:
                        pfx = '((key)'
                        sfx = ')'
            if u'\t' in value and self.warntabs:
                warning(u"A string contains a tab. Tabs are expanded to four"
                         " spaces by the viewer when copy-pasting the code"
                         " (disable this warning by disabling the 'warntabs'"
                         " option).")
            return pfx + '"' + value.encode('utf8').replace('\\','\\\\') \
                .replace('"','\\"').replace('\n','\\n') + '"' + sfx
        if tvalue == int:
            if value < 0 and not self.globalmode and self.optsigns:
                #return '0x%X' % (value + 4294967296)
                return '((integer)' + str(value) + ')'
            return str(value)
        if tvalue == float:
            if self.optfloats and value.is_integer() and -2147483648.0 <= value < 2147483648.0:
                if self.globalmode and not self.listmode:
                    if value == 0 and copysign(1, value) == -1:
                        return '-0.'
                    return str(int(value))
                elif not self.globalmode:
                    # Important inside lists!!
                    if value == 0 and copysign(1, value) == -1:
                        return '(-(float)0)'
                    return '((float)' + str(int(value)) + ')'
            s = repr(value)
            if s == 'nan':
                return '(1e40*0)' if copysign(1, value) < 0 else '(-1e40*0)'
            if s == 'inf':
                return '1e40'
            if s == '-inf':
                return '-1e40' if self.globalmode else '((float)-1e40)'
            # Try to remove as many decimals as possible but keeping the F32 value intact
            exp = s.find('e')
            if ~exp:
                s, exp = s[:exp], s[exp:]
                if exp[1] == '+':
                    exp = exp[:1] + exp[2:]
                if '.' not in s:
                    # I couldn't produce one but it's assumed that if it happens,
                    # this code deals with it correctly
                    s += '.' # pragma: no cover
            else:
                if '.' not in s:
                    # This should never happen (Python should always return a point or exponent)
                    return s + '.' # pragma: no cover
                exp = ''

            # Shorten the float as much as possible.
            while s[-1] != '.' and lslfuncs.F32(float(s[:-1]+exp)) == value:
                s = s[:-1]
            if s[-1] != '.':
                news = s[:-1]
                neg = ''
                if s[0] == '-':
                    news = news[1:]
                    neg = '-'
                # Try harder
                point = news.index('.') + 1 - len(news)
                if point:
                    news = str(int(news[:point-1] + news[point:]) + 1).zfill(len(news)-1) # Increment
                else:
                    news = str(int(news[:-1])+1).zfill(len(news)-1)
                news = news[:point + len(news)] + '.' + news[point + len(news):] # Reinsert point
                # Repeat the operation with the incremented number
                while news[-1] != '.' and lslfuncs.F32(float(neg+news[:-1]+exp)) == value:
                    news = news[:-1]
                if len(neg+news) < len(s) and lslfuncs.F32(float(neg+news+exp)) == value:
                    # Success! But we try even harder. We may have converted
                    # 9.9999e3 into 10.e3; that needs to be turned into 1.e4.
                    if exp != '':
                        if news[2:3] == '.': # we converted 9.9... into 10.
                            newexp = 'e' + str(int(exp[1:])+1) # increase exponent
                            news2 = news[0] + '.' + news[1] + news[3:] # move dot to the left
                            while news2[-1] == '0': # remove trailing zeros
                                news2 = news2[:-1]
                            if len(neg+news2) < len(s) and lslfuncs.F32(float(neg+news2+newexp)) == value:
                                news = news2
                                exp = newexp
                    s = neg+news
            if exp and s[-1] == '.':
                s = s[:-1] # transfrom e.g. 1.e-30 into 1e-30
            if value >= 0 or self.globalmode or not self.optsigns:
                return s + exp
            return '((float)' + s + exp + ')'
        if tvalue == Vector:
            return '<' + self.Value2LSL(value[0]) + ', ' + self.Value2LSL(value[1]) \
                + ', ' + self.Value2LSL(value[2]) + '>'
        if tvalue == Quaternion:
            return '<' + self.Value2LSL(value[0]) + ', ' + self.Value2LSL(value[1]) \
                + ', ' + self.Value2LSL(value[2]) + ', ' + self.Value2LSL(value[3]) + '>'
        if tvalue == list:
            if value == []:
                return '[]'
            if len(value) < 5:
                save_listmode = self.listmode
                self.listmode = True
                ret = '[' + self.Value2LSL(value[0])
                for elem in value[1:]:
                    ret += ', ' + self.Value2LSL(elem)
                ret += ']'
                self.listmode = save_listmode
                return ret
            ret = '' if lslcommon.IsCalc else '\n'
            first = True
            self.indentlevel += 0 if lslcommon.IsCalc else 1
            for entry in value:
                ret += self.dent() + ('[ ' if first else ', ')
                save_listmode = self.listmode
                self.listmode = True
                ret += self.Value2LSL(entry) + '\n'
                self.listmode = save_listmode
                first = False
            ret += self.dent()
            self.indentlevel -= 0 if lslcommon.IsCalc else 1
            return ret + ']'

        assert False, u'Value of unknown type in Value2LSL: ' + repr(value)

    def dent(self):
        return self.indent * self.indentlevel

    def FindName(self, node, scope = None):
        if scope is None:
            # node is a node
            if (hasattr(node, 'scope')
                    and 'NewName' in self.symtab[node.scope][node.name]):
                return self.symtab[node.scope][node.name]['NewName']
            if node.nt == 'FNCALL' and 'NewName' in self.symtab[0][node.name]:
                return self.symtab[0][node.name]['NewName']

            return node.name
        # node is a name
        if 'NewName' in self.symtab[scope][node]:
            return self.symtab[scope][node]['NewName']
        return node

    def OutIndented(self, node):
        if node.nt != '{}':
            self.indentlevel += 1
        ret = self.OutCode(node)
        if node.nt != '{}':
            self.indentlevel -= 1
        return ret

    def OutExprList(self, L):
        ret = ''
        if L:
            First = True
            for item in L:
                if not First:
                    ret += ', '
                ret += self.OutExpr(item)
                First = False
        return ret

    def OutExpr(self, expr):
        # Handles expression nodes (as opposed to statement nodes)
        nt = expr.nt
        child = expr.ch

        if nt in self.binary_operands:
            lnt = child[0].nt
            lparen = False
            rnt = child[1].nt
            rparen = False
            if nt in self.assignment_ops and nt in self.op_priority:
                # Assignment is right-associative, so it needs to be dealt with
                # separately.
                base_pri = self.op_priority[nt]
                if rnt in self.op_priority:
                    if self.op_priority[rnt] < base_pri: # should never happen
                        rparen = True
            elif nt in self.op_priority:
                base_pri = self.op_priority[nt]
                if lnt in self.op_priority:
                    if self.op_priority[lnt] < base_pri:
                        lparen = True
                elif lnt == 'NEG' and base_pri > self.op_priority['-']:
                    lparen = True

                # This situation has ugly cases due to the strange precedence
                # of unary minus. Consider the following two statements:
                #     (~-a) * a
                #     a * (~-a) * a
                # In one case, the (~-a) is a left child; in the other, it's
                # part of a right child. In both, cases, the parentheses are
                # mandatory, or they would be interpreted respectively as:
                #     ~-(a * a)
                #     a * ~-(a * a)
                # Yet the tree structure makes it quite hard to detect these.
                # So as a safeguard, for now we parenthesize all ~ and ! within
                # binary operands, as they have a deceitful binding power when
                # there's a unary minus downstream.
                #
                # TODO: See if the parenthesizing of ~ and ! can be improved.
                elif lnt in ('~', '!'):
                    lparen = True

                if rnt in self.op_priority:
                    if self.op_priority[rnt] <= base_pri:
                        rparen = True
                # see above
                elif rnt in ('~', '!'):
                    rparen = True

            if lparen:
                ret = '(' + self.OutExpr(child[0]) + ')'
            else:
                ret = self.OutExpr(child[0])
            ret += ' ' + nt + ' '
            if rparen:
                ret += '(' + self.OutExpr(child[1]) + ')'
            else:
                ret += self.OutExpr(child[1])
            return ret

        if nt == 'IDENT':
            return self.FindName(expr)

        if nt == 'CONST':
            if (self.foldconst and expr.t == 'list' and len(expr.value) == 1
                    and not self.globalmode):
                return '(list)' + self.Value2LSL(expr.value[0])
            return self.Value2LSL(expr.value)

        if nt == 'CAST' or self.foldconst and nt in ('LIST', 'CONST') and len(child)==1 and not self.globalmode:
            ret =  '(' + expr.t + ')'
            expr = child[0]
            if expr.nt in ('CONST', 'IDENT', 'V++', 'V--', 'VECTOR',
               'ROTATION', 'LIST', 'FIELD', 'PRINT', 'FNCALL'):
                if expr.nt != 'LIST' or len(expr.ch) != 1:
                    return ret + self.OutExpr(expr)
            return ret + '(' + self.OutExpr(expr) + ')'

        if nt == 'LIST':
            self.listmode = True
            if len(child) < 5:
                ret = '[' + self.OutExprList(child) + ']'
            else:
                self.indentlevel += 0 if lslcommon.IsCalc else 1
                ret = '' if lslcommon.IsCalc else '\n'
                first = True
                for elem in child:
                    ret += self.dent() + ('[ ' if first else ', ')
                    ret += self.OutExpr(elem) + '\n'
                    first = False
                ret += self.dent() + ']'
                self.indentlevel -= 0 if lslcommon.IsCalc else 1
            self.listmode = False
            return ret

        if nt in ('VECTOR', 'ROTATION'):
            ret = ('<' + self.OutExpr(child[0]) + ','
                   + self.OutExpr(child[1]) + ',')
            if nt == 'ROTATION':
                ret += self.OutExpr(child[2]) + ','
            lnt = child[-1].nt
            if lnt in self.op_priority \
               and self.op_priority[lnt] <= self.op_priority['>']:
                ret += '(' + self.OutExpr(child[-1]) + ')'
            else:
                ret += self.OutExpr(child[-1])
            return ret + '>'

        if nt == 'FNCALL':
            return self.FindName(expr) + '(' + self.OutExprList(child) + ')'

        if nt == 'PRINT':
            return 'print(' + self.OutExpr(child[0]) + ')'

        if nt in self.unary_operands:
            ret = nt
            lnt = child[0].nt
            paren = False
            if nt == 'NEG':
                ret = '-'
                if (lnt == 'CONST' and child[0].t == 'integer'
                    and child[0].value < 0
                   ):
                    # shortcut
                    ret += str(child[0].value + 4294967296)
                    return ret
                if lnt in self.op_priority:
                    paren = self.op_priority[lnt] <= self.op_priority['-']
                elif (lnt == 'NEG' or lnt == '--V'
                      or lnt == 'CONST'
                         and child[0].t == 'float'
                         and child[0].value < 0
                     ):
                    ret += ' ' # don't output "--" as that's a different token
            else:
                if lnt in self.op_priority:
                    paren = True

            if paren:
                ret += '(' + self.OutExpr(child[0]) + ')'
            else:
                ret += self.OutExpr(child[0])
            return ret

        if nt == 'FLD':
            return self.OutExpr(child[0]) + '.' + expr.fld

        if nt in ('V--', 'V++'):
            return self.OutExpr(child[0]) + ('++' if nt == 'V++' else '--')

        if nt in ('--V', '++V'):
            return ('++' if nt == '++V' else '--') + self.OutExpr(child[0])

        if nt in self.extended_assignments:
            lvalue = self.OutExpr(child[0])
            return lvalue + ' = ' + lvalue + ' ' + nt[:-1] + ' (' + self.OutExpr(child[1]) + ')'

        if nt == 'EXPRLIST':
            return self.OutExprList(child)

        if nt == 'SUBIDX':
            return '(MISSING TYPE)' + self.OutExpr(child[0]) + '[' + self.OutExprList(child[1:]) + ']'

        assert False, 'Internal error: expression type "' + nt + '" not handled' # pragma: no cover

    def OutCode(self, node):
        nt = node.nt
        child = node.ch

        if nt == 'IF':
            ret = self.dent()
            while True:
                ret += 'if (' + self.OutExpr(child[0]) + ')\n'
                # Do we need to add braces around the THEN side?
                needs_braces = False
                if len(child) == 3:
                    testnode = child[1]
                    # Find last IF in an ELSE IF chain
                    while testnode.nt == 'IF' and len(testnode.ch) == 3:
                        testnode = testnode.ch[2]
                    if testnode.nt == 'IF':
                        # hit an IF without ELSE at the end of the chain
                        needs_braces = True
                if needs_braces:
                    ret += self.dent() + '{\n'
                    ret += self.OutIndented(child[1])
                    ret += self.dent() + '}\n'
                else:
                    ret += self.OutIndented(child[1])
                if len(child) < 3:
                    return ret
                if child[2].nt != 'IF':
                    ret += self.dent() + 'else\n' + self.OutIndented(child[2])
                    return ret
                ret += self.dent() + 'else '
                node = child[2]
                child = node.ch
        if nt == 'WHILE':
            ret = self.dent() + 'while (' + self.OutExpr(child[0]) + ')\n'
            ret += self.OutIndented(child[1])
            return ret
        if nt == 'DO':
            ret = self.dent() + 'do\n'
            ret += self.OutIndented(child[0])
            return ret + self.dent() + 'while (' + self.OutExpr(child[1]) + ');\n'
        if nt == 'FOR':
            ret = self.dent() + 'for ('
            ret += self.OutExpr(child[0])
            ret += '; ' + self.OutExpr(child[1]) + '; '
            ret += self.OutExpr(child[2])
            ret += ')\n'
            ret += self.OutIndented(child[3])
            return ret
        if nt == '@':
            return self.dent() + '@' + self.FindName(node) + ';\n'
        if nt == 'JUMP':
            return self.dent() + 'jump ' + self.FindName(node) + ';\n'
        if nt == 'STSW':
            return self.dent() + 'state ' + self.FindName(node) + ';\n'
        if nt == 'RETURN':
            if child:
                return self.dent() + 'return ' + self.OutExpr(child[0]) + ';\n'
            return self.dent() + 'return;\n'
        if nt == 'DECL':
            ret = self.dent() + node.t + ' ' + self.FindName(node)
            if child:
                if hasattr(child[0], 'orig') and (child[0].orig.nt != 'IDENT'
                        or child[0].orig.name
                           in self.symtab[child[0].orig.scope]):
                    ret += ' = ' + self.OutExpr(child[0].orig)
                else:
                    ret += ' = ' + self.OutExpr(child[0])
            return ret + ';\n'
        if nt == ';':
            return self.dent() + ';\n'

        if nt in ('STDEF', '{}'):
            ret = ''
            if nt == 'STDEF':
                if node.name == 'default':
                    ret = self.dent() + 'default\n'
                else:
                    ret = self.dent() + 'state ' + self.FindName(node) + '\n'

            ret += self.dent() + '{\n'
            self.indentlevel += 1
            firstnode = True
            for stmt in node.ch:
                if stmt.nt == 'LAMBDA':
                    continue
                if nt == 'STDEF' and not firstnode:
                    ret += '\n'
                ret += self.OutCode(stmt)
                firstnode = False
            self.indentlevel -= 1
            return ret + self.dent() + '}\n'

        if nt == 'FNDEF':
            ret = self.dent()
            if node.t is not None:
                ret += node.t + ' '
            ret += self.FindName(node) + '('
            scope = node.pscope
            ret += ', '.join(typ + ' ' + self.FindName(name, scope)
                             for typ, name in zip(node.ptypes, node.pnames))
            return ret + ')\n' + self.OutCode(child[0])

        if nt == 'EXPR':
            return self.dent() + self.OutExpr(child[0]) + (
                ';\n' if not lslcommon.IsCalc else '')

        if nt == 'LAMBDA':
            return ''

        assert False, "Internal error: node type not handled: " + nt # pragma: no cover

    def output(self, treesymtab, options = ('optimize',
            'optsigns','optfloats','warntabs')):
        # Build a sorted list of dict entries
        self.tree, self.symtab = treesymtab

        # Grab options
        self.optimize = 'optimize' in options
        # These are optimization options that depend on the above:
        self.optsigns = self.optimize and 'optsigns' in options
        self.optfloats = self.optimize and 'optfloats' in options
        self.foldconst = self.optimize and 'constfold' in options

        self.warntabs = 'warntabs' in options

        ret = ''
        self.indent = '    '
        self.indentlevel = 0
        self.globalmode = False
        self.listmode = False
        firstnode = True
        prevnt = None
        for node in self.tree:
            if node.nt == 'LAMBDA':
                # these don't produce output, skip
                continue
            if not firstnode and (node.nt != 'DECL' or prevnt != 'DECL'):
                ret += '\n'
            self.globalmode = node.nt == 'DECL'
            ret += self.OutCode(node)
            self.globalmode = False
            firstnode = False
            prevnt = node.nt

        return ret
