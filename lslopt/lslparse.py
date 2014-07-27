from lslcommon import Key, Vector, Quaternion
import lslfuncs
import sys, re

# Note this module was basically written from bottom to top, which may help
# reading it.

def warning(txt):
    assert type(txt) == str
    sys.stderr.write(txt + '\n')

def isdigit(c):
    return '0' <= c <= '9'

def isalpha_(c):
    return c == '_' or 'A' <= c <= 'Z' or 'a' <= c <= 'z'

def isalphanum_(c):
    return isalpha_(c) or isdigit(c)

def ishex(c):
    return '0' <= c <= '9' or 'A' <= c <= 'F' or 'a' <= c <= 'f'

def fieldpos(inp, sep, n):
    "Return the starting position of field n in a string inp that has zero or more fields separated by sep"
    i = -1
    for n in xrange(n):
        i = inp.find(sep, i + 1)
        if i < 0:
            return i
    return i + 1

class EParse(Exception):

    def __init__(self, parser, msg):
        lno = parser.script.count('\n', 0, parser.errorpos)
        cno = parser.errorpos - fieldpos(parser.script, '\n', lno)
        # Note the column number reported is in bytes.

        msg = u"(Line %d char %d): ERROR: %s" % (lno + 1, cno + 1, msg)
        super(EParse, self).__init__(msg)

class EParseUEOF(EParse):
    def __init__(self, parser):
        parser.errorpos = len(parser.script)
        super(self.__class__, self).__init__(parser, u"Unexpected EOF")

class EParseSyntax(EParse):
    def __init__(self, parser):
        super(self.__class__, self).__init__(parser, u"Syntax error")

class EParseAlreadyDefined(EParse):
    def __init__(self, parser):
        super(self.__class__, self).__init__(parser, u"Name previously declared within scope")

class EParseUndefined(EParse):
    def __init__(self, parser):
        super(self.__class__, self).__init__(parser, u"Name not defined within scope")

class EParseUnexpected(EParse):
    def __init__(self, parser):
        super(self.__class__, self).__init__(parser, u"Unexpected internal error")

class EParseTypeMismatch(EParse):
    def __init__(self, parser):
        super(self.__class__, self).__init__(parser, u"Type mismatch")

class EParseReturnShouldBeEmpty(EParse):
    def __init__(self, parser):
        super(self.__class__, self).__init__(parser, u"Return statement type doesn't match function return type")

class EParseReturnIsEmpty(EParse):
    def __init__(self, parser):
        super(self.__class__, self).__init__(parser, u"Function returns a value but return statement doesn't")

# This error message may sound funny, for good reasons.
class EParseInvalidField(EParse):
    def __init__(self, parser):
        super(self.__class__, self).__init__(parser, u"Use of vector or quaternion method on incorrect type")

class EParseFunctionMismatch(EParse):
    def __init__(self, parser):
        super(self.__class__, self).__init__(parser, u"Function type mismatches type or number of arguments")

class EParseDeclarationScope(EParse):
    def __init__(self, parser):
        super(self.__class__, self).__init__(parser, u"Declaration requires a new scope -- use { and }")

class EInternal(Exception):
    """This exception is a construct to allow a different function to cause an
    immediate return of EOF from parser.GetToken(). Reused elsewhere for
    detecting parsing errors.
    """
    pass

# This table is to save memory in the parse tree in interpreters that don't
# intern strings.
S = ('integer','float','string','key','vector','rotation','quaternion','list',
    'IDENT','x','y','z','s','CAST','<','<=','>=','>','CONSTANT','VECTOR',
    'ROTATION','LIST','PRINT','FUNCTION','FIELD','EXPR','V++','V--','=',
    '+=','-=','*=','/=','%=','&=','|=','^=','<<=','>>=','NEG','!','~','++V',
    '--V','()','*','/','%','+','-','<<','>>','==','!=','&','^','|','&&','||',
    '@','JUMP','STATE','RETURN','IF','WHILE','DO','FOR','DECL','{}',';',
    'Label','State','TRUE','FALSE','default','DEFAULT'
    )
S = {i:i for i in S}

class parser(object):
    assignment_ops = frozenset(('=', '+=', '-=', '*=', '/=', '%='))
    extassignment_ops = frozenset(('|=', '&=', '^=', '<<=', '>>='))

    double_ops = frozenset(('++', '--', '+=', '-=', '*=', '/=', '%=', '==',
                                     '!=', '>=', '<=', '&&', '||', '<<', '>>'))
    extdouble_ops = frozenset(('|=', '&=', '^='))

    # These are hardcoded because additions or modifications imply
    # important changes to the code anyway.
    keywords = frozenset((S['default'], 'state', 'event', 'jump', 'return', 'if',
        'else', 'for', 'do', 'while', 'print', S['TRUE'], S['FALSE']))
    types = frozenset((S['integer'],S['float'],S['string'],S['key'],S['vector'],
        S['quaternion'],S['rotation'],S['list']))
    # Default values per type when declaring variables
    DefaultValues = {S['integer']: 0, S['float']: 0.0, S['string']: u'',
        S['key']: Key(u''), S['vector']: lslfuncs.ZERO_VECTOR,
        S['rotation']: lslfuncs.ZERO_ROTATION, S['list']: []
        }

    PythonType2LSL = {int: S['integer'], float: S['float'],
        unicode: S['string'], Key: S['key'], Vector: S['vector'],
        Quaternion: S['rotation'], list: S['list']}

    PythonType2LSLToken = {int:'INTEGER_VALUE', float:'FLOAT_VALUE',
        unicode:'STRING_VALUE', Key:'KEY_VALUE', Vector:'VECTOR_VALUE',
        Quaternion:'ROTATION_VALUE', list:'LIST_VALUE'}


    def PushScope(self):
        """Create a new symbol table / scope level"""
        self.symtab.append({-1: self.scopeindex})
        self.scopeindex = len(self.symtab)-1

    def PopScope(self):
        """Return to the previous scope level"""
        self.scopeindex = self.symtab[self.scopeindex][-1]
        if self.scopeindex is None:
            raise EParseUnexpected(self)

    def FindSymbolPartial(self, symbol, MustBeLabel = False):
        """Find a symbol in all visible scopes in order.

        Labels have special scope rules: other identifiers with the same
        name that are not labels are invisible to JUMP statements. Example:

            default{timer(){  @x; {integer x; jump x;} }}

        finds the label at the outer block. However:

            default{timer(){  @x; integer x; }}

        gives an identifier already defined error. On the other hand, labels
        hide other types (but that's dealt with in the caller to this function):

            default{timer(){ integer a; { @a; a++; } }}

        gives an Name Not Defined error.
        """
        scope = self.scopeindex
        while scope is not None:
            symtab = self.symtab[scope]
            if symbol in symtab and (not MustBeLabel or symtab[symbol][1] == 'Label'):
                return symtab[symbol]
            scope = symtab[-1] # it's a dict, not a list; -1 is a key
        return None

    # No labels or states allowed here (but functions are)
    def FindSymbolFull(self, symbol):
        """Returns either a string with the LSL type, or a tuple if it's a
        function.
        """
        scope = self.scopeindex
        while scope:
            symtab = self.symtab[scope]
            if symbol in symtab:
                # This can't happen, as functions can't be local
                #if len(symtab[symbol]) > 3:
                #    return (symtab[symbol][1], symtab[symbol][3])
                return symtab[symbol][1]
            scope = symtab[-1]
        if self.globalmode and symbol not in self.symtab[0] and symbol not in self.functions:
            return None # Disallow forwards in global var mode
        if symbol not in self.globals:
            return None
        return self.globals[symbol]

    def FindScopeIndex(self, symbol, MustBeLabel = False):
        """Same as FindSymbolPartial, but stops at globals, and returns scope
        level instead of symbol table entry.
        """
        scope = self.scopeindex
        while scope:
            symtab = self.symtab[scope]
            if symbol in symtab and (not MustBeLabel or symtab[symbol][1] == 'Label'):
                return scope
            scope = symtab[-1]
        return scope

    def ValidateField(self, typ, field):
        if typ == 'vector' and field in ('x', 'y', 'z') \
           or typ == 'rotation' and field in ('x', 'y', 'z', 's'):
            return
        raise EParseInvalidField(self)

    def order(self):
        self.dictorder += 1
        return self.dictorder

    def autocastcheck(self, value, typ):
        """Check if automatic dynamic cast is possible, and insert it if
        requested explicitly.
        """
        if value[1] == typ:
            return value
        if value[1] in ('string', 'key') and typ in ('string', 'key') \
           or value[1] == 'integer' and typ == 'float':
            if self.explicitcast:
                return [S['CAST'], S[typ], value]
            return value
        raise EParseTypeMismatch(self)

    def ueof(self):
        "Check for unexpected EOF"
        if self.pos >= self.length:
            raise EParseUEOF(self)

    def ceof(self):
        "Check for normal EOF"
        if self.pos >= self.length:
            raise EInternal() # force GetToken to return EOF

    def GetToken(self):
        "Lexer"

        # Keep track of the current position. If an error occurs, it will happen at the start of this token.
        self.errorpos = self.pos

        try:
            while self.pos < self.length:
                c = self.script[self.pos]
                self.pos += 1

                # Process comments
                if c == '/':
                    if self.script[self.pos:self.pos+1] == '/':
                        self.pos += 1
                        self.ceof()
                        while self.script[self.pos] != '\n':
                            self.pos += 1
                            self.ceof() # A single-line comment at EOF is not unexpected EOF.

                        self.pos += 1
                        self.ceof()
                        continue

                    elif self.script[self.pos:self.pos+1] == '*':
                        self.pos += 2
                        while self.script[self.pos-1:self.pos+1] != '*/':
                            self.pos += 1
                            self.ueof() # An unterminated multiline comment *is* unexpected EOF.

                        self.pos += 1
                        self.ceof()
                        continue

                # Process strings
                if c == '"' or c == 'L' and self.script[self.pos:self.pos+1] == '"':
                    strliteral = ''
                    if c == 'L':
                        self.pos += 1
                        strliteral = '"'

                    while self.script[self.pos:self.pos+1] != '"':
                        self.ueof()
                        if self.script[self.pos] == '\\':
                            self.pos += 1
                            self.ueof()
                            if self.script[self.pos] == 'n':
                                strliteral += '\n'
                            elif self.script[self.pos] == 't':
                                strliteral += '    '
                            else:
                                strliteral += self.script[self.pos]
                        else:
                            strliteral += self.script[self.pos]
                        self.pos += 1

                    self.pos += 1
                    return ('STRING_VALUE', strliteral.decode('utf8'))

                if isalpha_(c):
                    # Identifier or reserved

                    ident = c
                    while isalphanum_(self.script[self.pos:self.pos+1]):
                        ident += self.script[self.pos]
                        self.pos += 1

                    # Got an identifier - check if it's a reserved word
                    if ident in self.keywords:
                        return (ident.upper(),)
                    if ident in self.types:
                        if ident == 'quaternion':
                            ident = 'rotation' # Normalize types
                        return ('TYPE',ident)
                    if ident in self.events:
                        return ('EVENT_NAME',ident)
                    if ident in self.constants:
                        value = self.constants[ident]
                        return (self.PythonType2LSLToken[type(value)], value)

                    return ('IDENT', ident)

                # Process numbers: float, hex integer, dec integer
                if c == '.' or isdigit(c):

                    number = ''
                    if c != '.':
                        # We have a digit, which means we have for sure either
                        # an integer or a float.

                        # Eat as many decimal digits as possible
                        number = c
                        while isdigit(self.script[self.pos:self.pos+1]):
                            number += self.script[self.pos]
                            self.pos += 1

                        if number == '0' and self.script[self.pos:self.pos+1] in ('x','X') \
                           and ishex(self.script[self.pos+1:self.pos+2]):
                            # We don't need the 0x prefix.

                            self.pos += 1
                            # Eat leading zeros to know the real length.
                            while self.script[self.pos:self.pos+1] == '0':
                                self.pos += 1
                            number = ''

                            while ishex(self.script[self.pos:self.pos+1]):
                                if len(number) < 9: # don't let it grow more than necessary
                                    number += self.script[self.pos]
                                self.pos += 1
                            if number == '':
                                # We know there was at least a valid digit so it
                                # must've been all zeros.
                                number = '0'
                            if len(number) > 8:
                                number = -1
                            else:
                                number = lslfuncs.S32(int(number, 16))
                            return ('INTEGER_VALUE', number)

                        # Add the dot if present
                        if self.script[self.pos:self.pos+1] == '.':
                            number += '.'
                            self.pos += 1
                    else:
                        number = c

                    while isdigit(self.script[self.pos:self.pos+1]):
                        number += self.script[self.pos]
                        self.pos += 1

                    # At this point, number contains as many digits as there are before the dot,
                    # the dot if present, and as many digits as there are after the dot.
                    if number != '.': # A dot alone can't be a number so we rule it out here.
                        exp = ''
                        if self.script[self.pos:self.pos+1] in ('e','E'):
                            epos = self.pos # Temporary position tracker, made permanent only if the match succeeds
                            exp = self.script[epos]
                            epos += 1
                            if self.script[epos:epos+1] in ('+','-'):
                                exp += self.script[epos]
                                epos += 1
                            if isdigit(self.script[epos:epos+1]):
                                # Now we *do* have an exponent.
                                exp += self.script[epos]
                                epos += 1
                                while isdigit(self.script[epos:epos+1]):
                                    exp += self.script[epos]
                                    epos += 1
                                self.pos = epos # "Commit" the new position
                            else:
                                exp = '' # No cigar. Rollback and backtrack. Invalidate exp.

                        if exp != '' or '.' in number: # Float
                            if '.' in number:
                                # Eat the 'F' if present
                                if self.script[self.pos:self.pos+1] in ('f','F'):
                                    # Python doesn't like the 'F' so don't return it
                                    #exp += self.script[self.pos]
                                    self.pos += 1
                            return ('FLOAT_VALUE', lslfuncs.F32(float(number + exp)))

                        if len(number) > 10 or len(number) == 10 and number > '4294967295':
                            number = -1
                        else:
                            number = lslfuncs.S32(int(number))

                        return ('INTEGER_VALUE', number)

                if self.script[self.pos-1:self.pos+1] in self.double_ops \
                   or self.extendedassignment and self.script[self.pos-1:self.pos+1] in self.extdouble_ops:
                    self.pos += 1
                    if self.extendedassignment and self.script[self.pos-2:self.pos+1] in ('<<=', '>>='):
                        self.pos += 1
                        return (self.script[self.pos-3:self.pos],)
                    return (self.script[self.pos-2:self.pos],)

                if c in '.;{},=()-+*/%@:<>[]&|^~!' and c != '':
                    return (c,)

                # We eat spacers AND any other character so the following is not needed,
                # although the lex file includes it (the lex file does not count() invalid characters
                # for the purpose of error reporting).
                #if c in ' \n\r\x0B':
                #    continue

        except EInternal:
            pass # clear the exception and fall through

        return ('EOF',)

    def NextToken(self):
        """Calls GetToken and sets the internal token."""
        self.tok = self.GetToken()

    # Recursive-descendent parser. The result is a symbol table.

    def expect(self, toktype):
        """Raise exception if the current token is not the given one."""
        if self.tok[0] != toktype:
            if self.tok[0] == 'EOF':
                raise EParseUEOF(self)
            raise EParseSyntax(self)

    def Parse_vector_rotation_tail(self):
        """(See Parse_unary_postfix_expression for context)

        To our advantage, the precedence of the closing '>' in a vector or
        rotation literal is that of an inequality. Our strategy will thus be
        to perform the job of an inequality, calling the lower level 'shift'
        rule and building the inequalities if they are not '>'. When we find a
        '>', we check whether the next token makes sense as beginning an
        inequality; if not, we finally close the vector or rotation.

        But first, a quaternion _may_ have a full expression at the third
        component, so we tentatively parse this position as an expression, and
        backtrack if it causes an error. This is the only point where this
        parser backtracks.
        """
        ret = []
        pos = self.pos
        errorpos = self.errorpos
        tok = self.tok
        try:
            ret.append(self.Parse_expression())

            # Checking here for '>' might parse a different grammar, because
            # it might allow e.g. <1,2,3==3>; as a vector, which is not valid.
            # Not too sure about that, but we're cautious and disable this
            # just in case.
            #if self.tok[0] == '>':
            #    return ret

            self.expect(',')
            self.NextToken()
        except EParseSyntax:
            # Backtrack
            self.pos = pos
            self.errorpos = errorpos
            self.tok = tok

        # OK, here we are.
        inequality = self.Parse_shift() # shift is the descendant of inequality
        while self.tok[0] in ('<', '<=', '>=', '>'):
            op = self.tok[0]
            self.NextToken()
            if op == '>':
                # Check if the current token can be a part of a comparison.
                # If not, it's a vector/quaternion terminator.
                if self.tok[0] not in (
                   # List adapted from this section of the bison report:
#state 570
#
#  176 expression: expression '>' . expression
#  214 quaternion_initializer: '<' expression ',' expression ',' expression ',' expression '>' .

                   'IDENT', 'INTEGER_VALUE', 'FLOAT_VALUE', 'STRING_VALUE',
                   'KEY_VALUE', 'VECTOR_VALUE', 'ROTATION_VALUE', 'LIST_VALUE',
                   'TRUE', 'FALSE', '++', '--', 'PRINT', '!', '~', '(', '['
                   ):
                    ret.append(inequality)
                    return ret
            # This is basically a copy/paste of the Parse_inequality handler
            type1 = inequality[1]
            if type1 not in ('integer', 'float'):
                raise EParseTypeMismatch(self)
            value = self.Parse_shift()
            type2 = value[1]
            if type2 not in ('integer', 'float'):
                raise EParseTypeMismatch(self)
            if type1 != type2:
                if type2 == 'float':
                    inequality = self.autocastcheck(inequality, type2)
                else:
                    value = self.autocastcheck(value, type1)
            inequality = [S[op], S['integer'], inequality, value]

        # Reaching this means an operator or lower precedence happened,
        # e.g. <1,1,1,2==2> (that's syntax error in ==)
        raise EParseSyntax(self)


    def Parse_unary_postfix_expression(self, AllowAssignment = True):
        """Grammar parsed here:

        unary_postfix_expression: INTEGER_VALUE | FLOAT_VALUE
            | STRING_VALUE | KEY_VALUE | VECTOR_VALUE | ROTATION_VALUE
            | LIST_VALUE | TRUE | FALSE | vector_literal | rotation_literal | list_literal
            | PRINT '(' expression ')' | IDENT '(' expression_list ')'
            | lvalue '++' | lvalue '--' | assignment %if allowed
            | lvalue
        vector_literal: '<' expression ',' expression ',' expression '>'
        rotation_literal: '<' expression ',' expression ',' expression
            ',' expression '>'
        list_literal: '[' optional_expression_list ']'
        assignment: lvalue '=' expression | lvalue '+=' expression
            | lvalue '-=' expression | lvalue '*=' expression
            | lvalue '/=' expression | lvalue '%=' expression
            %EXTENDED RULES:
            | lvalue '|=' expression | lvalue '&=' expression
            | lvalue '<<=' expression | lvalue '>>=' expression
        lvalue: IDENT | IDENT '.' IDENT
        """
        tok0 = self.tok[0]
        val = self.tok[1] if len(self.tok) > 1 else None
        self.NextToken()
        CONSTANT = S['CONSTANT']
        if tok0 == '-' and self.tok[0] in ('INTEGER_VALUE', 'FLOAT_VALUE'):
            tok0 = self.tok[0]
            val = self.tok[1]
            self.NextToken()
            return [CONSTANT, S['integer' if type(val) == int else 'float'], -val]
        if tok0 == 'INTEGER_VALUE':
            return [CONSTANT, S['integer'], val]
        if tok0 == 'FLOAT_VALUE':
            return [CONSTANT, S['float'], val]
        if tok0 == 'STRING_VALUE':
            if self.allowmultistrings:
                while self.tok[0] == 'STRING_VALUE':
                    val += self.tok[1]
                    self.NextToken()
            return [CONSTANT, S['string'], val]
        # Key constants are not currently supported - use string
        #if tok0 == 'KEY_VALUE':
        #    return [CONSTANT, S['key'], val]
        if tok0 == 'VECTOR_VALUE':
            return [CONSTANT, S['vector'], val]
        if tok0 == 'ROTATION_VALUE':
            return [CONSTANT, S['rotation'], val]
        if tok0 == 'LIST_VALUE':
            return [CONSTANT, S['list'], val]
        if tok0 in ('TRUE', 'FALSE'):
            return [CONSTANT, S['integer'], 1 if tok0 == 'TRUE' else 0]
        if tok0 == '<':
            val = [self.Parse_expression()]
            self.expect(',')
            self.NextToken()
            val.append(self.Parse_expression())
            self.expect(',')
            self.NextToken()

            # It would be cute if it were this simple:
            #val.append(self.Parse_expression())
            #if self.tok[0] == '>':
            #    self.NextToken()
            #    return [S['VECTOR'], S['vector']] + val
            #self.expect(',')
            #self.NextToken()
            #val.append(self.Parse_inequality())
            #self.expect('>')
            #self.NextToken()
            #return [S['ROTATION'], S['rotation']] + val

            # Alas, it isn't. The closing angle bracket of a vector '>'
            # conflicts with the inequality operator '>' in unexpected ways.
            # Example: <2,2,2> * 2 will trigger the problem:
            # the expression parser tries to parse the inequality 2 > *2,
            # choking at the *. To make things worse, LSL admits things such as
            # <2,2,2 > 2> (but not things like <2,2,2 == 2> because the == has
            # lower precedence than the '>' and thus it forces termination of
            # the vector constant). And to make things even worse, it also
            # admits things such as <2,2,2 == 2, 2> because the comma is not in
            # the precedence scale, so it's quite complex to handle.

            # We defer it to a separate function.
            val += self.Parse_vector_rotation_tail()

            if len(val) == 3:
                return [S['VECTOR'], S['vector']] + val
            return [S['ROTATION'], S['rotation']] + val

        if tok0 == '[':
            val = self.Parse_optional_expression_list()
            self.expect(']')
            self.NextToken()
            return [S['LIST'], S['list']] + val
        if tok0 == 'PRINT':
            self.expect('(')
            self.NextToken()
            val = self.Parse_expression()
            if val[1] not in self.types:
                raise EParseTypeMismatch(self) if val[1] is None else EParseUndefined(self)
            self.expect(')')
            self.NextToken()
            return [S['PRINT'], None, val]

        if tok0 != 'IDENT':
            if tok0 == 'EOF':
                raise EParseUEOF(self)
            raise EParseSyntax(self)
        typ = self.FindSymbolFull(val)
        if typ is None:
            raise EParseUndefined(self)
        # Note this may fail to do interning of the string from the symbol table.
        # Doing so with a dictionary key may affect performance.
        name = val

        # Course of action decided here.
        tok0 = self.tok[0]
        if tok0 == '(':
            # Function call
            self.NextToken()
            if type(typ) != tuple:
                raise EParseUndefined(self)
            args = self.Parse_optional_expression_list(typ[1])
            self.expect(')')
            self.NextToken()
            return [S['FUNCTION'], None if typ[0] is None else S[typ[0]], name, args, self.scopeindex]
        if typ not in self.types:
            raise EParseTypeMismatch(self)
        typ = S[typ]
        lvalue = [S['IDENT'], typ, name, self.FindScopeIndex(name)]
        if tok0 == '.':
            self.NextToken()
            self.expect('IDENT')
            self.ValidateField(typ, self.tok[1])
            lvalue = [S['FIELD'], S['float'], lvalue, S[self.tok[1]]]
            self.NextToken()
            tok0 = self.tok[0]

        if tok0 in ('++', '--'):
            self.NextToken()
            if lvalue[1] not in ('integer', 'float'):
                raise EParseTypeMismatch(self)
            return [S['V'+tok0], lvalue[1], lvalue]
        if AllowAssignment and (tok0 in self.assignment_ops
                                or self.extendedassignment and tok0 in self.extassignment_ops):
            self.NextToken()
            expr = self.Parse_expression()
            rtyp = expr[1]
            if rtyp not in self.types:
                raise EParseTypeMismatch(self)
            if typ in ('integer', 'float'):
                # LSL admits integer *= float (go figger).
                # It acts like: lhs = (integer)((float)lhs * rhs)
                # That would trigger an error without this check.
                if tok0 != '*=' or typ == 'float':
                    expr = self.autocastcheck(expr, typ)
                    rtyp = typ
            # Lots of drama for checking types. This is pretty much like
            # addition, subtraction, multiply, divide, etc. all in one go.
            if tok0 == '=':
                if typ == 'list' != rtyp:
                    if self.explicitcast:
                        expr = [S['CAST'], typ, expr]
                else:
                    expr = self.autocastcheck(expr, typ)

                return [S['='], typ, lvalue, expr]

            if tok0 == '+=':
                if typ == 'float':
                    expr = self.autocastcheck(expr, typ)
                if rtyp != typ != 'list' or typ == rtyp == 'key':
                    # key + key is the only disallowed combo of equals
                    raise EParseTypeMismatch(self)
                if self.explicitcast:
                    if typ == 'list' != rtyp:
                        expr = [S['CAST'], S[typ], expr]
                return [S[tok0], typ, lvalue, expr]

            if tok0 == '-=':
                if typ == rtyp in ('integer', 'float', 'vector', 'rotation'):
                    return [S[tok0], typ, lvalue, expr]
                raise EParseTypeMismatch(self)

            if tok0 in ('*=', '/='):
                # There is a special case dealt with in advance.
                if tok0 == '*=' and typ == 'integer' and rtyp == 'float':
                    return [S[tok0], typ, lvalue, expr]

                if (typ == rtyp or typ == 'vector') and rtyp in ('integer', 'float', 'rotation'):
                    if typ == 'vector' and rtyp == 'integer':
                        expr = self.autocastcheck(expr, 'float')
                    return [S[tok0], typ, lvalue, expr]
                raise EParseTypeMismatch(self)

            if tok0 == '%=':
                if typ == rtyp in ('integer', 'vector'):
                    return [S[tok0], typ, lvalue, expr]

            # Rest take integer operands only

            if typ == rtyp == 'integer':
                return [S[tok0], typ, lvalue, expr]

        return lvalue

    def Parse_unary_expression(self, AllowAssignment = True):
        """Grammar parsed here:

        unary_expression: '-' factor | '!' unary_expression | '~' unary_expression
            # we expand lvalue here to facilitate parsing
            | '++' IDENT | '++' IDENT '.' IDENT
            | '--' IDENT | '--' IDENT '.' IDENT
            | '(' TYPE ')' typecast_expression | '(' expression ')'
            | unary_postfix_expression
        %NORMAL RULES ONLY:
        typecast_expression: '(' expression ')' | unary_postfix_expression %except assignment
        %EXTENDED RULES ONLY:
        typecast_expression: unary_expression %except assignment
        """
        tok0 = self.tok[0]
        if tok0 == '-':
            # Unary minus
            self.NextToken()
            value = self.Parse_factor()
            if value[1] not in ('integer', 'float', 'vector', 'rotation'):
                raise EParseTypeMismatch(self)
            return [S['NEG'], value[1], value]
        if tok0 in ('!', '~'):
            # Unary logic and bitwise NOT - applies to integers only
            self.NextToken()
            value = self.Parse_unary_expression()
            if value[1] != 'integer':
                raise EParseTypeMismatch(self)
            return [S[tok0], S['integer'], value]
        if tok0 in ('++', '--'):
            # Pre-increment / pre-decrement
            self.NextToken()
            self.expect('IDENT')
            name = self.tok[1]
            typ = self.FindSymbolFull(name)
            if typ not in self.types:
                # Pretend it doesn't exist
                raise EParseUndefined(self)
            typ = S[typ]

            ret = [S['IDENT'], typ, name, self.FindScopeIndex(name)]
            self.NextToken()
            if self.tok[0] == '.':
                self.NextToken()
                self.expect('IDENT')
                self.ValidateField(typ, self.tok[1])
                ret = [S['FIELD'], S['float'], ret, S[self.tok[1]]]
                self.NextToken()

            typ = ret[1]
            if typ not in ('integer', 'float'):
                raise EParseTypeMismatch(self)

            return [S[tok0+'V'], typ, ret]

        if tok0 == '(':
            # Parenthesized expression or typecast

            self.NextToken()
            if self.tok[0] != 'TYPE':
                # Parenthesized expression
                expr = self.Parse_expression()
                self.expect(')')
                self.NextToken()
                return [S['()'], expr[1], expr]

            # Typecast
            typ = S[self.tok[1]]
            self.NextToken()
            self.expect(')')
            self.NextToken()

            if self.extendedtypecast:
                # Allow any unary expression (except assignment). The type cast
                # acts as a prefix operator.
                expr = self.Parse_unary_expression(AllowAssignment = False)
            else:
                if self.tok[0] == '(':
                    self.NextToken()
                    expr = self.Parse_expression()
                    self.expect(')')
                    self.NextToken()
                    expr = [S['()'], expr[1], expr]
                else:
                    expr = self.Parse_unary_postfix_expression(AllowAssignment = False)
            basetype = expr[1]
            if typ == 'list' and basetype in self.types \
               or basetype in ('integer', 'float') and typ in ('integer', 'float', 'string') \
               or basetype == 'string' and typ in self.types \
               or basetype == 'key' and typ in ('string', 'key') \
               or basetype == 'vector' and typ in ('string', 'vector') \
               or basetype == 'rotation' and typ in ('string', 'rotation') \
               or basetype == 'list' and typ == 'string':
                return [S['CAST'], typ, expr]
            raise EParseTypeMismatch(self)

        # Must be a postfix expression.
        return self.Parse_unary_postfix_expression(AllowAssignment)

    def Parse_factor(self):
        """Grammar parsed here:

        factor: unary_expression | factor '*' unary_expression
            | factor '/' unary_expresssion | factor '%' unary_expression
        """
        factor = self.Parse_unary_expression()
        while self.tok[0] in ('*', '/', '%'):
            op = self.tok[0]
            type1 = factor[1]
            # Acceptable types for LHS
            if op in ('*', '/') and type1 not in ('integer', 'float',
                                                  'vector', 'rotation') \
               or op == '%' and type1 not in ('integer', 'vector'):
                raise EParseTypeMismatch(self)
            self.NextToken()
            value = self.Parse_unary_expression()
            type2 = value[1]
            # Mod is easier to check for
            if op == '%' and type1 != type2:
                raise EParseTypeMismatch(self)
            if op == '%' or type1 == type2 == 'integer':
                # Deal with the special cases first (it's easy)
                factor = [S[op], S[type1], factor, value]
            else:
                # Any integer must be promoted to float now
                if type1 == 'integer':
                    type1 = 'float'
                    factor = self.autocastcheck(factor, type1)
                if type2 == 'integer':
                    type2 = 'float'
                    value = self.autocastcheck(value, type2)
                if type1 == 'float' and type2 in ('float', 'vector') \
                   or type1 == 'vector' and type2 in ('float', 'vector', 'rotation') \
                   or type1 == type2 == 'rotation':
                    if op == '/' and type2 == 'vector':
                        # Division by vector isn't valid
                        raise EParseTypeMismatch(self)
                    # The rest are valid
                    if type1 == 'float' and type2 == 'vector':
                        resulttype = type2
                    elif type1 == type2 == 'vector':
                        resulttype = 'float'
                    else:
                        resulttype = type1
                    factor = [S[op], S[resulttype], factor, value]
                else:
                    raise EParseTypeMismatch(self)

        return factor

    def Parse_term(self):
        """Grammar parsed here:

        term: factor | term '+' factor | term '-' factor
        """
        term = self.Parse_factor()
        while self.tok[0] in ('+', '-'):
            op = self.tok[0]
            type1 = term[1]
            if op == '+' and type1 not in self.types \
               or op == '-' and type1 not in ('integer', 'float',
                                              'vector', 'rotation'):
                raise EParseTypeMismatch(self)
            self.NextToken()
            value = self.Parse_factor()
            type2 = value[1]
            # This is necessary, but the reason is subtle.
            # The types must match in principle (except integer/float), so it
            # doesn't seem necessary to check type2. But there's the case
            # where the first element is a list, where the types don't need to
            # match but the second type must make sense.
            if op == '+' and type2 not in self.types:
               #or op == '-' and type2 not in ('integer', 'float',
               #                               'vector', 'rotation'):
                raise EParseTypeMismatch(self)
            # Isolate the additions where the types match to make our life easier later
            if op == '+' and (type1 == type2 or type1 == 'list' or type2 == 'list'):
                if type1 == type2 == 'key':
                    # key + key is the only disallowed combo of equals
                    raise EParseTypeMismatch(self)
                if self.explicitcast:
                    if type1 == 'list' != type2:
                        value = [S['CAST'], S[type1], value]
                        #type2 = type1 # unused
                    elif type2 == 'list' != type1:
                        term = [S['CAST'], S[type2], term]
                        type1 = type2
                term = [S[op], S[type1], term, value]
                # Note that although list + nonlist is semantically the same as
                # list + (list)nonlist and same goes for nonlist + list, they
                # don't compile to the same thing, but the optimizer should deal
                # with typecast removal anyway.
            elif self.allowkeyconcat and op == '+' \
                 and type1 in ('key', 'string') and type2 in ('key', 'string'):
                # Allow string+key addition (but add explicit cast)
                if type1 == 'key':
                    term = [S[op], S[type2], [S['CAST'], S[type2], term], value]
                else:
                    term = [S[op], S[type1], term, [S['CAST'], S[type1], value]]
            elif type1 == 'key' or type2 == 'key':
                # Only list + key or key + list is allowed, otherwise keys can't
                # be added or subtracted with anything.
                raise EParseTypeMismatch(self)
            else:
                if type1 == 'float':
                    # Promote value to float
                    term = [S[op], S[type1], term, self.autocastcheck(value, type1)]
                else:
                    # Convert LHS to type2 if possible (note no keys arrive here)
                    term = [S[op], S[type2], self.autocastcheck(term, type2), value]

        return term

    def Parse_shift(self):
        """Grammar parsed here:

        shift: term | shift '<<' term | shift '>>' term
        """
        shift = self.Parse_term()
        while self.tok[0] in ('<<', '>>'):
            if shift[1] != 'integer':
                raise EParseTypeMismatch(self)
            op = self.tok[0]
            self.NextToken()
            shift = [S[op], S['integer'], shift , self.Parse_term()]
            if shift[3][1] != 'integer':
                raise EParseTypeMismatch(self)

        return shift

    def Parse_inequality(self):
        """Grammar parsed here:

        inequality: shift | inequality '<' shift | inequality '<=' shift
            | inequality '>' shift | inequality '>=' shift
        """
        inequality = self.Parse_shift()
        while self.tok[0] in ('<', '<=', '>', '>='):
            op = self.tok[0]
            type1 = inequality[1]
            if type1 not in ('integer', 'float'):
                raise EParseTypeMismatch(self)
            self.NextToken()
            value = self.Parse_shift()
            type2 = value[1]
            if type2 not in ('integer', 'float'):
                raise EParseTypeMismatch(self)
            if type1 != type2:
                if type2 == 'float':
                    inequality = self.autocastcheck(inequality, type2)
                else:
                    value = self.autocastcheck(value, type1)
            inequality = [S[op], S['integer'], inequality, value]

        return inequality

    def Parse_comparison(self):
        """Grammar parsed here:

        comparison: inequality | comparison '==' inequality
            | comparison '!=' inequality
        """
        comparison = self.Parse_inequality()
        while self.tok[0] in ('==', '!='):
            op = self.tok[0]
            type1 = comparison[1]
            if type1 not in self.types:
                raise EParseTypeMismatch(self)
            self.NextToken()
            value = self.Parse_inequality()
            type2 = value[1]
            if type1 == 'float':
                value = self.autocastcheck(value, type1)
            else:
                # For string & key, RHS (type2) mandates the conversion
                # (that's room for optimization: always compare strings)
                comparison = self.autocastcheck(comparison, type2)
            comparison = [S[op], S['integer'], comparison, value]

        return comparison

    def Parse_bitbool_factor(self):
        """Grammar parsed here:

        bitbool_factor: comparison | bitbool_factor '&' comparison
        """
        bitbool_factor = self.Parse_comparison()
        while self.tok[0] == '&':
            if bitbool_factor[1] != 'integer':
                raise EParseTypeMismatch(self)
            op = self.tok[0]
            self.NextToken()
            bitbool_factor = [S[op], S['integer'], bitbool_factor, self.Parse_comparison()]
            if bitbool_factor[3][1] != 'integer':
                raise EParseTypeMismatch(self)

        return bitbool_factor

    def Parse_bitxor_term(self):
        """Grammar parsed here:

        bitxor_term: bitbool_factor | bitxor_term '^' bitbool_factor
        """
        bitxor_term = self.Parse_bitbool_factor()
        while self.tok[0] == '^':
            if bitxor_term[1] != 'integer':
                raise EParseTypeMismatch(self)
            op = self.tok[0]
            self.NextToken()
            bitxor_term = [S[op], S['integer'], bitxor_term, self.Parse_bitbool_factor()]
            if bitxor_term[3][1] != 'integer':
                raise EParseTypeMismatch(self)

        return bitxor_term

    def Parse_bitbool_term(self):
        """Grammar parsed here:

        bitbool_term: bitxor_term | bitbool_term '|' bitxor_term
        """
        bitbool_term = self.Parse_bitxor_term()
        while self.tok[0] == '|':
            if bitbool_term[1] != 'integer':
                raise EParseTypeMismatch(self)
            op = self.tok[0]
            self.NextToken()
            bitbool_term = [S[op], S['integer'], bitbool_term, self.Parse_bitxor_term()]
            if bitbool_term[3][1] != 'integer':
                raise EParseTypeMismatch(self)

        return bitbool_term

    def Parse_expression(self):
        """Grammar parsed here:

        expression: bitbool_term | expression '||' bitbool_term
            | expression '&&' bitbool_term

        Most operators with same priority, in general, are executed in
        right-to-left order but calculated with precedence left-to-right.
        That is, the tree is generated LTR but traversed RTL (in post-order).

        E.g. a-b+c is calculated (in RPN notation) as: c, b, a, swap, -, +
        i.e. c is evaluated first and a last, but the operation is still (a-b)+c
        which is normal LTR.

        At this point we're just constructing the tree, so we follow normal
        precedence rules.
        """
        expression = self.Parse_bitbool_term()
        while self.tok[0] in ('&&', '||'):
            if expression[1] != 'integer':
                raise EParseTypeMismatch(self)
            op = self.tok[0]
            self.NextToken()
            expression = [S[op], S['integer'], expression, self.Parse_bitbool_term()]
            if expression[3][1] != 'integer':
                raise EParseTypeMismatch(self)

        return [S['EXPR'], expression[1], expression]

    def Parse_optional_expression_list(self, expected_types = None):
        """Grammar parsed here:

        optional_expression_list: LAMBDA | expression_list
        expression_list: expression | expression_list ',' expression
        """
        # This is a maze of which we get out with a dirty hack.
        # optional_expression_list is used by FOR statements (closed by ';' or ')'),
        # list constants (closed by ']') and function arguments (closed by ')').
        # If it's not the right token, we'll err anyway, in Parse_expression or
        # upon return.
        ret = []
        idx = 0
        if self.tok[0] not in (']', ')', ';'):
            while True:
                val = self.Parse_expression()
                if expected_types is not None:
                    if idx >= len(expected_types):
                        raise EParseFunctionMismatch(self)
                    try:
                        val = self.autocastcheck(val, expected_types[idx]);
                    except EParseTypeMismatch:
                        raise EParseFunctionMismatch(self)
                else:
                    if val[1] not in self.types:
                        raise EParseTypeMismatch(self)
                idx += 1
                ret.append(val)
                if self.tok[0] != ',':
                    break
                self.NextToken()
        if expected_types is not None and idx != len(expected_types):
            raise EParseFunctionMismatch(self)
        return ret

    def Parse_statement(self, ReturnType, AllowDecl = False):
        """Grammar parsed here:

        statement: ';' | single_statement | code_block
        single_statement: if_statement | while_statement | do_statement
            | for_statement | jump_statement | state_statement | label_statement
            | return_statement | declaration_statement | expression ';'
        if_statement: IF '(' expression ')' statement ELSE statement
            | IF '(' expression ')' statement
        while_statement: WHILE '(' expression ')' statement
        do_statement: DO statement WHILE '(' expression ')' ';'
        for_statement: FOR '(' optional_expression_list ';' expression ';'
            optional_expression_list ')' statement
        jump_statement: JUMP IDENT ';'
        state_statement: STATE DEFAULT ';' | STATE IDENT ';'
        label_statement: '@' IDENT ';'
        return_statement: RETURN ';' | RETURN expression ';'
        declaration_statement: TYPE lvalue ';' | TYPE lvalue '=' expression ';'

        There's a restriction: a *single* statement can not be a declaration.
        """
        tok0 = self.tok[0]
        if tok0 == '{':
            return self.Parse_code_block(ReturnType)
        if tok0 == ';':
            self.NextToken()
            return [';', None]
        if tok0 == '@':
            self.NextToken()
            self.expect('IDENT')
            name = self.tok[1]
            if name in self.symtab[self.scopeindex]:
                raise EParseAlreadyDefined(self)
            self.symtab[self.scopeindex][name] = (self.order(), S['Label'])
            self.NextToken()
            self.expect(';')
            self.NextToken()
            return [S['@'], None, name]
        if tok0 == 'JUMP':
            self.NextToken()
            self.expect('IDENT')
            name = self.tok[1]
            tmp = self.FindSymbolPartial(name, MustBeLabel=True)
            if not tmp or tmp[1] != 'Label':
                # It might still be a forward reference, so we add it to the
                # list of things to look up when done
                self.jump_lookups.append((name, self.scopeindex, self.errorpos))
            self.NextToken()
            self.expect(';')
            self.NextToken()
            return [S['JUMP'], None, ['IDENT', S['Label'], name, self.FindScopeIndex(name, MustBeLabel=True)]]
        if tok0 == 'STATE':
            self.NextToken()
            if self.tok[0] not in ('DEFAULT', 'IDENT'):
                raise EParseSyntax(self)
            # States are only searched in the global scope
            name = self.tok[1] if self.tok[0] == 'IDENT' else 'default'
            if name not in self.symtab[0] and (name not in self.globals or self.globals[name] != 'State'):
                raise EParseUndefined(self)
            self.NextToken()
            self.expect(';')
            self.NextToken()
            return [S['STATE'], None,
                [S['IDENT'], S['State'], name, 0] if name != 'default' else S['DEFAULT']]
        if tok0 == 'RETURN':
            self.NextToken()
            if self.tok[0] == ';':
                value = None
            else:
                value = self.Parse_expression()
            self.expect(';')
            self.NextToken()
            if ReturnType is None and value is not None:
                raise EParseReturnShouldBeEmpty(self)
            if ReturnType is not None and value is None:
                raise EParseReturnIsEmpty(self)
            if value is None:
                return [S['RETURN'], None, None]
            return [S['RETURN'], None, self.autocastcheck(value, ReturnType)]
        if tok0 == 'IF':
            self.NextToken()
            self.expect('(')
            self.NextToken()
            condition = self.Parse_expression()
            self.expect(')')
            self.NextToken()
            then_branch = self.Parse_statement(ReturnType)
            else_branch = None
            if self.tok[0] == 'ELSE':
                self.NextToken()
                else_branch = self.Parse_statement(ReturnType)
            return [S['IF'], None, condition, then_branch] + ([else_branch] if else_branch is not None else [])
        if tok0 == 'WHILE':
            self.NextToken()
            self.expect('(')
            self.NextToken()
            condition = self.Parse_expression()
            self.expect(')')
            self.NextToken()
            return [S['WHILE'], None, condition, self.Parse_statement(ReturnType)]
        if tok0 == 'DO':
            self.NextToken()
            stmt = self.Parse_statement(ReturnType)
            self.expect('WHILE')
            self.NextToken()
            self.expect('(')
            self.NextToken()
            condition = self.Parse_expression()
            self.expect(')')
            self.NextToken()
            self.expect(';')
            self.NextToken()
            return [S['DO'], None, stmt, condition]
        if tok0 == 'FOR':
            self.NextToken()
            self.expect('(')
            self.NextToken()
            initializer = self.Parse_optional_expression_list()
            self.expect(';')
            self.NextToken()
            condition = self.Parse_expression()
            self.expect(';')
            self.NextToken()
            iterator = self.Parse_optional_expression_list()
            self.expect(')')
            self.NextToken()
            stmt = self.Parse_statement(ReturnType)
            return [S['FOR'], None, initializer, condition, iterator, stmt]
        if tok0 == 'TYPE':
            if not AllowDecl:
                raise EParseDeclarationScope(self)
            typ = S[self.tok[1]]
            self.NextToken()
            self.expect('IDENT')
            name = self.tok[1]
            if name in self.symtab[self.scopeindex]:
                raise EParseAlreadyDefined(self)
            self.NextToken()
            value = None
            if self.tok[0] == '=':
                self.NextToken()
                value = self.Parse_expression()
            self.expect(';')
            self.NextToken()
            self.symtab[self.scopeindex][name] = (self.order(), typ, value)
            return [S['DECL'], None, name, self.scopeindex]

        # If none of the above, it must be an expression.
        value = self.Parse_expression()
        self.expect(';')
        self.NextToken()
        return value

    def Parse_code_block(self, ReturnType):
        """Grammar parsed here:

        code_block: '{' statements '}'
        statements: LAMBDA | statements statement

        It receives the return type to expect for return statements.
        """
        self.expect('{')
        self.NextToken()

        self.PushScope()

        ret = [S['{}'], None]
        while True:
            if self.tok[0] == '}':
                break
            ret.append(self.Parse_statement(ReturnType, AllowDecl = True))

        self.PopScope()

        self.expect('}')
        self.NextToken()

        return ret

    def Parse_simple_expr(self, List=False):
        """Grammar parsed here:

        simple_expr: simple_expr_except_list | list_simple_expr
        simple_expr_except_list: STRING_VALUE | KEY_VALUE | VECTOR_VALUE
            | ROTATION_VALUE | TRUE | FALSE | number_value
            | '<' simple_expr ',' simple_expr ',' simple_expr '>'
            | '<' simple_expr ',' simple_expr ',' simple_expr ',' simple_expr '>'
        number_value: FLOAT_VALUE | INTEGER_VALUE | '-' FLOAT_VALUE | '-' INTEGER_VALUE
        list_simple_expr: '[' ']' | '[' list_simple_expr_items ']'
        list_simple_expr_items: simple_expr_except_list
            | list_simple_expr_items ',' simple_expr_except_list
        """
        tok = self.tok
        self.NextToken()
        if tok[0] == 'TRUE': # TRUE and FALSE don't admit sign in globals
            return 1
        if tok[0] == 'FALSE':
            return 0
        if tok[0] in ('STRING_VALUE', 'KEY_VALUE', 'VECTOR_VALUE', 'ROTATION_VALUE', 'LIST_VALUE'):
            val = tok[1]
            if tok[0] == 'STRING_VALUE' and self.allowmultistrings:
                while self.tok[0] == 'STRING_VALUE':
                    val += self.tok[1]
                    self.NextToken()
            return val
        if tok[0] == 'IDENT':
            tmp = self.FindSymbolPartial(tok[1])
            if tmp is None or len(tmp) > 3 or tmp[1] not in self.types:
                raise EParseUndefined(self)
            #return tmp[2]
            return (S['IDENT'], S[tmp[1]], tok[1], self.FindScopeIndex(tok[1]))
        if tok[0] == '<':
            value = [self.Parse_simple_expr()]
            self.autocastcheck((0, self.PythonType2LSL[type(value[0])]), 'float')
            self.expect(',')
            self.NextToken()
            value.append(self.Parse_simple_expr())
            self.autocastcheck((0, self.PythonType2LSL[type(value[1])]), 'float')
            self.expect(',')
            self.NextToken()
            value.append(self.Parse_simple_expr())
            self.autocastcheck((0, self.PythonType2LSL[type(value[2])]), 'float')
            if self.tok[0] == '>':
                self.NextToken()
                return Vector(value)
            self.expect(',')
            self.NextToken()
            value.append(self.Parse_simple_expr())
            self.autocastcheck((0, self.PythonType2LSL[type(value[3])]), 'float')
            self.expect('>')
            self.NextToken()
            return Quaternion(value)

        if tok[0] == '[' and not List:
            value = []
            if self.tok[0] == ']':
                self.NextToken()
                return value
            while True:
                value.append(self.Parse_simple_expr(List=True))
                if self.tok[0] == ']':
                    self.NextToken()
                    return value
                self.expect(',')
                self.NextToken()
        neg = False
        if tok[0] == '-':
            neg = True
            tok = self.tok
            self.NextToken()
        if tok[0] not in ('INTEGER_VALUE', 'FLOAT_VALUE'):
            raise EParseSyntax(self)
        if neg:
            if tok[0] == 'INTEGER_VALUE':
                if tok[1] == -2147483648:
                    return -2147483648
            return -tok[1]
        return tok[1]

    def Parse_optional_param_list(self):
        """Grammar parsed here:

        optional_param_list: LAMBDA | param_list
        param_list: TYPE IDENT | param_list ',' TYPE IDENT
        """
        ret = []

        if self.tok[0] == 'TYPE':
            while True:
                typ = S[self.tok[1]]
                self.NextToken()
                self.expect('IDENT')

                name = self.tok[1]
                ret.append(name)
                if name in self.symtab[self.scopeindex]:
                    raise EParseAlreadyDefined(self)

                self.symtab[self.scopeindex][name] = (self.order(), typ, None) # Value is not predefined
                self.NextToken()
                if self.tok[0] != ',':
                    break
                self.NextToken()
                self.expect('TYPE')

        return tuple(ret)

    def Parse_events(self):
        """Grammar parsed here:

        events: event | events event
        event: EVENT_NAME '(' optional_parameter_list ')' code_block
        """
        self.expect('EVENT_NAME') # mandatory

        ret = {}

        while self.tok[0] == 'EVENT_NAME':
            name = self.tok[1]
            self.NextToken()
            self.expect('(')
            self.NextToken()
            # Function parameters go to a dedicated symbol table.
            self.PushScope()
            params = self.Parse_optional_param_list()
            # NOTE: Parse_events: This is a bit crude, as the error is given at the end of the param list.
            # To do it correctly, we can pass the parameter list to Parse_optional_param_list().
            if tuple(self.symtab[self.scopeindex][x][1] for x in params) != self.events[name]:
                raise EParseSyntax(self)
            self.expect(')')
            self.NextToken()
            value = tuple(self.Parse_code_block(None))
            ret[name] = (self.order(), None, value, params, self.scopeindex)
            self.PopScope()

        return ret

    def Parse_globals(self):
        """Grammar parsed here:

        globals: LAMBDA | globals var_def | globals func_def
        var_def: TYPE IDENT ';' | TYPE IDENT '=' simple_expr ';'
        func_def: optional_type IDENT '(' optional_param_list ')' code_block
        optional_type: LAMBDA | TYPE
        """
        while self.tok[0] in ('TYPE','IDENT'):
            typ = None
            if self.tok[0] == 'TYPE':
                typ = S[self.tok[1]]
                self.NextToken()
                self.expect('IDENT')

            name = self.tok[1]
            if name in self.symtab[self.scopeindex]:
                raise EParseAlreadyDefined(self)
            self.NextToken()

            if self.tok[0] == '=' or self.tok[0] == ';':
                # This is a variable definition
                if typ is None: # Typeless variables are not allowed
                    raise EParseSyntax(self)

                if self.tok[0] == '=':
                    self.NextToken()
                    if self.extendedglobalexpr:
                        self.globalmode = True # Var def. Disallow forward globals.
                        value = tuple(self.Parse_expression()) # Use advanced expression evaluation.
                        self.globalmode = False # Allow forward globals again.
                    else:
                        value = self.Parse_simple_expr() # Use LSL's dull global expression.
                    self.expect(';')
                    self.NextToken()
                else: # must be semicolon
                    self.NextToken()
                    value = None

                if value is not None:
                    if type(value) != tuple and not self.extendedglobalexpr:
                        self.autocastcheck((0, self.PythonType2LSL[type(value)]), typ)
                    else:
                        self.autocastcheck(value, typ)
                self.symtab[self.scopeindex][name] = (self.order(), typ, value)

            elif self.tok[0] == '(':
                # This is a function definition
                self.NextToken()
                self.PushScope() # Parameter names don't conflict with globals.
                params = self.Parse_optional_param_list()
                self.expect(')')
                self.NextToken()
                value = tuple(self.Parse_code_block(typ))
                paramscope = self.scopeindex
                self.PopScope()
                self.symtab[self.scopeindex][name] = (self.order(), typ, value, params, paramscope)
            else:
                raise EParseSyntax(self)
        pass

    def Parse_states(self):
        """Grammar parsed here:

        states: LAMBDA | states state
        state: state_header '{' events '}'
        state_header: DEFAULT | STATE IDENT

        (but we enforce DEFAULT to be the first token found, meaning there will
        be at least one state and the first must be DEFAULT as in the original
        grammar)
        """
        self.expect('DEFAULT')

        while True:
            if self.tok[0] != 'DEFAULT' and self.tok[0] != 'STATE':
                return

            if self.tok[0] == 'DEFAULT':
                name = S['default']
            else:
                self.NextToken()
                if self.tok[0] != 'IDENT':
                    raise EParseSyntax(self)
                name = self.tok[1]

            if name in self.symtab[self.scopeindex]:
                raise EParseAlreadyDefined(self)

            self.symtab[self.scopeindex][name] = (self.order(), S['State']) # to expand later
            self.NextToken()

            self.expect('{')
            self.NextToken()

            events = self.Parse_events()

            self.expect('}')
            self.symtab[self.scopeindex][name] += (events,)
            self.NextToken()

    def Parse_script(self):
        """Parses the whole LSL script

        Grammar parsed here:

        script: globals states EOF
        """


        self.Parse_globals()
        self.Parse_states()
        self.expect('EOF')

        # Check the pending jump targets
        for tgt in self.jump_lookups:
            self.scopeindex = tgt[1]
            if self.FindSymbolPartial(tgt[0], MustBeLabel = True) is None:
                self.errorpos = tgt[2]
                raise EParseUndefined(self)


    def BuildTempGlobalsTable(self):
        """Build an approximate globals table.

        If the script syntax is correct, the globals table will be accurate.
        If it is not, it may contain too many or too few symbols (normally the
        latter). This globals table is not the normal globals in the symbol
        table; it's just needed to resolve which names are declared at all as
        globals and their type. It's temporary.

        The grammar is approximately:
        script: globals states
        globals: [global [global [...]]]
        global: [TYPE] IDENT '(' TYPE anytoken [',' TYPE anytoken [...]]
                anytoken_except_comma balanced_braces_or_anything_else
            | TYPE IDENT [anytoken_except_semicolon [...]] ';'
        states: state [state [...]]
        state: (DEFAULT | STATE IDENT) balanced_braces_or_anything_else
        """
        ret = self.functions.copy() # The library functions go here too.

        # If there's a syntax error, that's not our business. We just return
        # what we have so far. Doing a proper parse will determine the exact
        # location and cause.

        # Here we don't even care if it's duplicate - that will be caught
        # when adding to the real symbol table.

        # Scan globals
        try:
            while self.tok[0] not in ('DEFAULT', 'EOF'):
                typ = None
                if self.tok[0] == 'TYPE':
                    typ = S[self.tok[1]]
                    self.NextToken()
                if self.tok[0] != 'IDENT':
                    return ret
                name = self.tok[1]
                self.NextToken()
                if self.tok[0] == '(':
                    # Function call
                    self.NextToken()
                    params = []
                    if self.tok[0] != ')':
                        while True:
                            if self.tok[0] != 'TYPE':
                                return ret
                            params.append(S[self.tok[1]])
                            self.NextToken()
                            self.NextToken() # not interested in parameter names
                            if self.tok[0] != ',':
                                break
                            self.NextToken()
                    self.NextToken()
                    if self.tok[0] != '{':
                        return ret
                    self.NextToken() # Enter the first brace

                    bracelevel = 1
                    while bracelevel and self.tok[0] != 'EOF':
                        if self.tok[0] == '{':
                            bracelevel += 1
                        elif self.tok[0] == '}':
                            bracelevel -= 1
                        self.NextToken()
                    ret[name] = (typ, tuple(params))

                elif typ is None:
                    return ret # A variable needs a type
                else:
                    ret[name] = typ

                    while self.tok[0] != ';': # Don't stop to analyze what's before the ending ';'
                        self.NextToken()
                    self.NextToken()
        except EParseUEOF:
            return ret

        # Scan states
        while True:
            if self.tok[0] not in ('DEFAULT', 'STATE'):
                return ret # includes EOF i.e. this is the normal return

            if self.tok[0] == 'STATE':
                self.NextToken()
                if self.tok[0] != 'IDENT':
                    return ret
                name = self.tok[1]
            else:
                name = S['default']

            ret[name] = S['State']
            self.NextToken()

            if self.tok[0] != '{':
                return ret
            self.NextToken() # Enter the first brace

            bracelevel = 1
            while bracelevel and self.tok[0] != 'EOF':
                if self.tok[0] == '{':
                    bracelevel += 1
                elif self.tok[0] == '}':
                    bracelevel -= 1
                self.NextToken()


    def parse(self, script, options = frozenset()):
        """Parse the given stream with the given options.

        This function also builds the temporary globals table.
        """
        self.script = script
        self.length = len(script)

        # Extended expressions in globals (needs support from the optimizer to work)
        self.extendedglobalexpr = 'extendedglobalexpr' in options

        # Extended typecast syntax (typecast as a regular unary operator)
        self.extendedtypecast = 'extendedtypecast' in options

        # Extended assignment operators: |= &= <<= >>=
        self.extendedassignment = 'extendedassignment' in options

        # Add explicit type casts when implicit (the output module takes care of
        # the correctness of the output)
        self.explicitcast = 'explicitcast' in options

        # Allow string + key = string and key + string = string
        self.allowkeyconcat = 'allowkeyconcat' in options

        # Allow C style string composition of strings: "blah" "blah" = "blahblah"
        self.allowmultistrings = 'allowmultistrings' in options

        # TODO: Add option to skip preprocessor directives (specifically #line).

        # TODO: Allow pure C-style string parsing. This is low-priority.
        #self.allowcescapes = 'allowcescapes' in options

        # TODO: Enable switch statements.
        #self.enableswitch = 'enableswitch' in options

        # TODO: Enable brackets for list elements e.g. (float)mylist[3], or mylist[5]=4
        #self.lazylists = 'lazylists' in options

        del options # no longer needed

        # Symbol table:
        # This is a list of all local and global symbol tables.
        # The first element (0) is the global scope. Each symbol table is a
        # dictionary. Element -1 of the dictionary is the parent index. The
        # entries are lists of three or four values. The first one is the
        # order; the second is the type, the third is the value, and if it's
        # a function, the fourth is the parameter list. Functions contain a
        # parse tree as their value.
        self.symtab = [{-1: None}]
        self.scopeindex = 0

        self.dictorder = 0

        # This is a small hack to prevent circular definitions in globals when
        # extended expressions are enabled. When false (default), forward
        # globals are allowed; if true, only already seen globals are permitted.
        self.globalmode = False

        # Globals and labels can be referenced before they are defined. That
        # includes states.
        #
        # Our first approach was going to be to build a list that keeps track of
        # undefined references, to check them after parsing. But that has a big
        # problem: expressions need to know the types of the arguments in order
        # to give appropriate errors if they don't suit the operand, and in
        # order to mark and check the types appropriately. But we don't know the
        # types of the globals that haven't been found yet. Therefore, sticking
        # to this approach would mean to scan the tree for every expression with
        # a pending reference, fixing up every node upstream with the correct
        # type with the possibility to find a type mismatch in a place for which
        # we have no location info.
        #
        # For that reason, we change the strategy. We still don't want to do
        # two full or almost full passes of the parser, nitpicking on every
        # detail. But given LSL's structure, it's relatively easy to do a fast
        # incomplete parsing pass, gathering globals with their types and
        # function arguments. And that's what we do.

        self.pos = 0
        self.tok = self.GetToken()

        self.globals = self.BuildTempGlobalsTable()

        # We need a table of undefined jump references anyway, to check later,
        # as jumps are local, not global, and allow forward definitions.
        self.jump_lookups = []

        # Restart

        self.pos = 0
        self.tok = self.GetToken()

        # Start the parsing proper
        self.Parse_script()

        del self.globals # No longer needed. The data that is not in self.functions is in self.symtab[0].
        del self.jump_lookups # Already used.

        #while self.tok[0] != 'EOF':
        #    print self.tok
        #    self.NextToken()

        #for n in xrange(len(self.symtab)):
        #    print n, '{',
        #    i = self.symtab[n]
        #    for j in sorted(i.items(), key=lambda k: -1 if k[0]==-1 else k[1][0]):
        #        print repr(j[0]) + ':' + repr(j[1]) + ',',
        #    print '}'

        return self.symtab

    def parsefile(self, filename, options = set()):
        """Convenience function to parse a file"""
        f = open(filename, 'rb')
        try:
            script = f.read()
        finally:
            f.close()

        return self.parse(script, options)

    def __init__(self):
        """Reads the library."""

        self.events = {}
        self.constants = {}
        self.functions = {}

        # Library read code

        parse_lin_re = re.compile(
            r'^\s*(event|void|integer|float|string|key|vector|quaternion|rotation|list)\s+'
            r'([a-zA-Z_][a-zA-Z0-9_]*)\s*\(\s*('
                r'(?:integer|float|string|key|vector|quaternion|rotation|list)\s+[a-zA-Z_][a-zA-Z0-9_]*'
                r'(?:\s*,\s*(?:integer|float|string|key|vector|quaternion|rotation|list)\s+[a-zA-Z_][a-zA-Z0-9_]*)*'
            r')?\s*\)\s*$'
            r'|'
            r'^\s*const\s+(integer|float|string|key|vector|quaternion|rotation|list)'
            r'\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*=\s*(.*?)\s*$'
            r'|'
            r'^\s*(?:#.*|//.*)?$')
        parse_arg_re = re.compile(r'^\s*([a-z]+)\s+[a-zA-Z_][a-zA-Z0-9_]*\s*$')
        parse_num_re = re.compile(r'^\s*(-?(?=[0-9]|\.[0-9])[0-9]*((?:\.[0-9]*)?(?:[Ee][+-]?[0-9]+)?))\s*$')
        parse_str_re = re.compile(ur'^"((?:[^"\\]|\\.)*)"$')

        f = open('builtins.txt', 'rb')
        try:
            while True:
                line = f.readline()
                if not line: break
                match = parse_lin_re.match(line)
                if not match:
                    warning('Syntax error in builtins.txt: ' + line)
                    continue
                if match.group(1):
                    # event or function
                    typ = match.group(1)
                    if typ == 'quaternion':
                        typ = 'rotation'
                    if typ == 'void':
                        typ = None
                    args = []
                    arglist = match.group(3)
                    if arglist:
                        arglist = arglist.split(',')
                        for arg in arglist:
                            args.append(parse_arg_re.match(arg).group(1))
                    name = match.group(2)
                    if typ == 'event':
                        if name in self.events:
                            warning('Event already defined in bultins.txt, overwriting: ' + name)
                        self.events[name] = tuple(args)
                    else:
                        # Library functions go to the functions table. If
                        # they are implemented in lslfuncs.*, they get a
                        # reference to the implementation; otherwise None.
                        if name in self.functions:
                            warning('Function already defined in bultins.txt, overwriting: ' + name)
                        self.functions[name] = (typ, tuple(args), getattr(lslfuncs, name, None))
                elif match.group(4):
                    # constant
                    name = match.group(5)
                    if name in self.constants:
                        warning('Global already defined in bultins.txt, overwriting: ' + name)
                    try:
                        typ = match.group(4)
                        if typ == 'quaternion':
                            typ = 'rotation'
                        val = match.group(6)
                        if typ == 'integer':
                            val = int(val, 0)
                        elif typ == 'float':
                            val = lslfuncs.F32(float(val))
                        elif typ == 'string':
                            val = val.decode('utf8')
                            if not parse_str_re.match(val):
                                raise EInternal
                            esc = False
                            tmp = val[1:-1]
                            val = u''
                            for c in tmp:
                                if esc:
                                    if c == u'n':
                                        c = u'\n'
                                    elif c == u't':
                                        c = u'    '
                                    val += c
                                    esc = False
                                elif c == u'\\':
                                    esc = True
                                else:
                                    val += c
                            #if typ == 'key':
                            #    val = Key(val)
                        elif typ == 'key':
                            warning('Key constants not supported in builtins.txt: ' + line)
                            val = None
                        elif typ in ('vector', 'rotation'):
                            if val[0:1] != '<' or val[-1:] != '>':
                                raise ValueError
                            val = val[1:-1].split(',')
                            if len(val) != (3 if typ == 'vector' else 4):
                                raise ValueError
                            num = parse_num_re.match(val[0])
                            if not num:
                                raise ValueError
                            val[0] = lslfuncs.F32(float(num.group(1)))
                            num = parse_num_re.match(val[1])
                            if not num:
                                raise ValueError
                            val[1] = lslfuncs.F32(float(num.group(1)))
                            num = parse_num_re.match(val[2])
                            if not num:
                                raise ValueError
                            val[2] = lslfuncs.F32(float(num.group(1)))
                            if typ != 'vector':
                                num = parse_num_re.match(val[3])
                                if not num:
                                    raise ValueError
                                val[3] = lslfuncs.F32(float(num.group(1)))
                                val = Quaternion(val)
                            else:
                                val = Vector(val)
                        else:
                            assert typ == 'list'
                            warning('List constants not supported in builtins.txt: ' + line)
                            val = None
                        if val is not None:
                            self.constants[name] = val

                    except EInternal:
                        warning('Invalid string in builtins.txt: ' + line)
                    except ValueError:
                        warning('Invalid vector syntax in builtins.txt: ' + line)
        finally:
            f.close()
