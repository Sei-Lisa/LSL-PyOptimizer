#    (C) Copyright 2015-2017 Sei Lisa. All rights reserved.
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

# Parser module. Converts the source into an abstract syntax tree,
# generating also the symbol table.

# TODO: Add info to be able to propagate error position to the source.

from lslcommon import Key, Vector, Quaternion
import lslcommon
import lslfuncs
import sys, re

# Note this module was basically written from bottom to top, which may help
# reading it.

def warning(txt):
    assert type(txt) == unicode
    sys.stderr.write(u"WARNING: " + txt + u"\n")

def isdigit(c):
    return '0' <= c <= '9'

def isalpha_(c):
    return c == '_' or 'A' <= c <= 'Z' or 'a' <= c <= 'z'

def isalphanum_(c):
    return isalpha_(c) or isdigit(c)

def ishex(c):
    return '0' <= c <= '9' or 'A' <= c <= 'F' or 'a' <= c <= 'f'

def fieldpos(inp, sep, n):
    """Return the starting position of field n in a string inp that has zero or
    more fields separated by sep
    """
    i = -1
    for n in xrange(n):
        i = inp.find(sep, i + 1)
        if i < 0:
            return i
    return i + 1

class EParse(Exception):

    def __init__(self, parser, msg):
        self.errorpos = parser.errorpos
        self.lno = parser.script.count('\n', 0, self.errorpos)
        self.cno = self.errorpos - fieldpos(parser.script, '\n', self.lno)
        # Note the column number reported is in bytes.

        msg = u"(Line %d char %d): ERROR: %s" % (self.lno + 1, self.cno + 1, msg)
        super(EParse, self).__init__(msg)

class EParseUEOF(EParse):
    def __init__(self, parser):
        parser.errorpos = len(parser.script)
        super(EParseUEOF, self).__init__(parser, u"Unexpected EOF")

class EParseSyntax(EParse):
    def __init__(self, parser):
        super(EParseSyntax, self).__init__(parser, u"Syntax error")

class EParseAlreadyDefined(EParse):
    def __init__(self, parser):
        super(EParseAlreadyDefined, self).__init__(parser,
            u"Name previously declared within scope")

class EParseUndefined(EParse):
    def __init__(self, parser):
        super(EParseUndefined, self).__init__(parser,
            u"Name not defined within scope")

class EParseTypeMismatch(EParse):
    def __init__(self, parser):
        super(EParseTypeMismatch, self).__init__(parser, u"Type mismatch")

class EParseReturnShouldBeEmpty(EParse):
    def __init__(self, parser):
        super(EParseReturnShouldBeEmpty, self).__init__(parser,
            u"Return statement type doesn't match function return type")

class EParseReturnIsEmpty(EParse):
    def __init__(self, parser):
        super(EParseReturnIsEmpty, self).__init__(parser,
            u"Function returns a value but return statement doesn't")

# This error message may sound funny, for good reasons.
class EParseInvalidField(EParse):
    def __init__(self, parser):
        super(EParseInvalidField, self).__init__(parser,
            u"Use of vector or quaternion method on incorrect type")

class EParseFunctionMismatch(EParse):
    def __init__(self, parser):
        super(EParseFunctionMismatch, self).__init__(parser,
            u"Function call mismatches type or number of arguments")

class EParseDeclarationScope(EParse):
    def __init__(self, parser):
        super(EParseDeclarationScope, self).__init__(parser,
            u"Declaration requires a new scope -- use { and }")

class EParseCantChangeState(EParse):
    def __init__(self, parser):
        super(EParseCantChangeState, self).__init__(parser,
            u"Global functions can't change state")

class EParseCodePathWithoutRet(EParse):
    def __init__(self, parser):
        super(EParseCodePathWithoutRet, self).__init__(parser,
            u"Not all code paths return a value")

class EParseDuplicateLabel(EParse):
    def __init__(self, parser):
        super(EParseDuplicateLabel, self).__init__(parser,
            u"Duplicate local label name. That won't allow the Mono script"
            u" to be saved, and will not work as expected in LSO.")

class EParseInvalidCase(EParse):
    def __init__(self, parser, kind):
        super(EParseInvalidCase, self).__init__(parser,
            u"'%s' used outside a 'switch' statement" % kind)

class EParseCaseNotAllowed(EParse):
    def __init__(self, parser, kind):
        super(EParseCaseNotAllowed, self).__init__(parser,
            u"'%s' label only allowed at the main 'switch' block" % kind)

class EParseManyDefaults(EParse):
    def __init__(self, parser):
        super(EParseManyDefaults, self).__init__(parser,
            u"multiple 'default' labels inside 'switch' statement")

class EParseMissingDefault(EParse):
    def __init__(self, parser):
        super(EParseMissingDefault, self).__init__(parser,
            u"Missing 'default:' label inside 'switch' statement; disable"
            u" option 'errmissingdefault' to disable this error.")

class EParseInvalidBreak(EParse):
    def __init__(self, parser):
        super(EParseInvalidBreak, self).__init__(parser,
            u"'break' used outside a loop or switch"
            if parser.enableswitch and parser.breakcont
            else u"'break' used outside a switch" if parser.enableswitch
            else u"'break' used outside a loop")

class EParseInvalidCont(EParse):
    def __init__(self, parser):
        super(EParseInvalidCont, self).__init__(parser,
            u"'continue' used outside a loop")

class EParseInvalidBrkContArg(EParse):
    def __init__(self, parser):
        super(EParseInvalidBrkContArg, self).__init__(parser,
            u"Invalid argument to 'break' or 'continue'" if parser.breakcont
            else u"Invalid argument to 'break'")

class EParseInvalidBackslash(EParse):
    def __init__(self, parser):
        super(EParseInvalidBackslash, self).__init__(parser,
            u"Preprocessor directive can't end in backslash."
            u" Activate the preprocessor or put everything in the same line.")

class EInternal(Exception):
    """This exception is a construct to allow a different function to cause an
    immediate return of EOF from parser.GetToken().
    """
    pass

class parser(object):
    assignment_toks = frozenset(('=', '+=', '-=', '*=', '/=', '%='))
    extassignment_toks = frozenset(('|=', '&=', '^=', '<<=', '>>='))

    double_toks = frozenset(('++', '--', '+=', '-=', '*=', '/=', '%=', '==',
                                     '!=', '>=', '<=', '&&', '||', '<<', '>>'))
    extdouble_toks = frozenset(('|=', '&=', '^='))

    # These are hardcoded because additions or modifications imply
    # important changes to the code anyway.
    base_keywords = frozenset(('default', 'state', 'event', 'jump', 'return',
        'if', 'else', 'for', 'do', 'while', 'print', 'TRUE', 'FALSE'))
    brkcont_keywords = frozenset(('break', 'continue'))
    switch_keywords = frozenset(('switch', 'case', 'break', 'default'))
    types = frozenset(('integer','float','string','key','vector',
        'quaternion','rotation','list'))
    PythonType2LSLToken = {int:'INTEGER_VALUE', float:'FLOAT_VALUE',
        unicode:'STRING_VALUE', Key:'KEY_VALUE', Vector:'VECTOR_VALUE',
        Quaternion:'ROTATION_VALUE', list:'LIST_VALUE'}

    TypeToExtractionFunction = {'integer':'llList2Integer',
        'float':'llList2Float', 'string':'llList2String', 'key':'llList2Key',
        'vector':'llList2Vector', 'rotation':'llList2Rot', 'list':'llList2List'}

    # Utility function
    def GenerateLabel(self):
        while True:
            self.labelcnt += 1
            unique = 'J_autoGen%05d' % self.labelcnt
            if unique not in self.locallabels:
                break
        self.locallabels.add(unique)
        return unique

    def PushScope(self):
        """Create a new symbol table / scope level"""
        self.symtab.append({-1:self.scopeindex}) # Add parent pointer
        self.scopeindex = len(self.symtab)-1

    def PopScope(self):
        """Return to the previous scope level"""
        self.scopeindex = self.symtab[self.scopeindex][-1] # -1 is a dict key, not an index
        assert self.scopeindex is not None, 'Unexpected internal error'

    def AddSymbol(self, kind, scope, name, **values):
        values['Kind'] = kind
        if kind in 'vl':
            values['Scope'] = scope
        self.symtab[scope][name] = values

    def FindSymbolPartial(self, symbol, MustBeLabel = False):
        """Find a symbol in all visible scopes in order, but not in the full
        globals table (only globals seen so far are visible).

        Labels have special scope rules: other identifiers with the same
        name that are not labels are invisible to JUMP statements. Example:

            default{timer(){ @x; {integer x; jump x;} }}

        finds the label at the outer block. However:

            default{timer(){ @x; integer x; }}

        gives an identifier already defined error. On the other hand, labels
        hide other types (but that's dealt with in the caller to this function):

            default{timer(){ integer a; { @a; a++; } }}

        gives an Name Not Defined error.
        """
        scopelevel = self.scopeindex
        while scopelevel is not None:
            symtab = self.symtab[scopelevel]
            if symbol in symtab and (not MustBeLabel or symtab[symbol]['Kind'] == 'l'):
                return symtab[symbol]
            scopelevel = symtab[-1] # -1 is a dict key, not an index
        return None

    # No labels or states allowed here (but functions are)
    def FindSymbolFull(self, symbol, scopelevel = None):
        """Returns the symbol table entry for the given symbol."""
        if scopelevel is None:
            # unspecified scope level means to look in the current scope
            scopelevel = self.scopeindex
        while scopelevel: # Loop over all local scopes
            symtab = self.symtab[scopelevel]
            if symbol in symtab:
                # This can't happen, as functions can't be local
                #if len(symtab[symbol]) > 3:
                #    return (symtab[symbol][1], symtab[symbol][3])
                return symtab[symbol]
            scopelevel = symtab[-1]
        try:
            return self.symtab[0][symbol] # Quick guess
        except KeyError:
            if self.disallowglobalvars and symbol not in self.symtab[0] \
               or symbol not in self.globals:
                return None # Disallow forwards in global var mode
            return self.globals[symbol]

    def ValidateField(self, typ, field):
        if typ == 'vector' and field in ('x', 'y', 'z') \
           or typ == 'rotation' and field in ('x', 'y', 'z', 's'):
            return
        raise EParseInvalidField(self)

    def autocastcheck(self, value, tgttype):
        """Check if automatic dynamic cast is possible, and insert it if
        requested explicitly.
        """
        tval = value['t']
        if tval == tgttype:
            return value
        if tval in ('string', 'key') and tgttype in ('string', 'key') \
           or tval == 'integer' and tgttype == 'float':
            if self.explicitcast:
                return {'nt':'CAST', 't':tgttype, 'ch':[value]}
            return value
        raise EParseTypeMismatch(self)

    def ueof(self):
        """Check for unexpected EOF"""
        if self.pos >= self.length:
            raise EParseUEOF(self)

    def ceof(self):
        """Check for normal EOF"""
        if self.pos >= self.length:
            raise EInternal() # force GetToken to return EOF

    def SetOpt(self, option, value):
        # See parse() for meaning of options.
        if option == 'extendedglobalexpr':
            self.extendedglobalexpr = value

        if option == 'extendedtypecast':
            self.extendedtypecast = value

        if option == 'extendedassignment':
            self.extendedassignment = value

        if option == 'explicitcast':
            self.explicitcast = value

        if option == 'allowkeyconcat':
            self.allowkeyconcat = value

        if option == 'allowmultistrings':
            self.allowmultistrings = value

        if option == 'processpre':
            self.processpre = value

        # TODO: Allow pure C-style string escapes. This is low-priority.
        #if option == 'allowcescapes':
        #    self.allowcescapes = value

        # Enable switch statements.
        if option == 'enableswitch':
            if not self.enableswitch and value:
                self.keywords |= self.switch_keywords
            elif self.enableswitch and not value:
                self.keywords = self.base_keywords
                if self.breakcont:
                    self.keywords |= self.brkcont_keywords

            self.enableswitch = value

        # Enable break/continue
        if option == 'breakcont':
            if not self.breakcont and value:
                self.keywords |= self.brkcont_keywords
            elif self.breakcont and not value:
                self.keywords = self.base_keywords
                if self.enableswitch:
                    self.keywords |= self.switch_keywords

            self.breakcont = value

        if option == 'errmissingdefault':
            self.errmissingdefault = value

        if option == 'lazylists':
            self.lazylists = value

        if option == 'duplabels':
            self.duplabels = value

        if option == 'shrinknames':
            self.shrinknames = value

        if option == 'funcoverride':
            self.funcoverride = value

    def ProcessDirective(self, directive):
        """Process a given preprocessor directive during parsing."""

        # Ignore directives on the first pass
        if self.scanglobals:
            return

        if directive[len(directive)-1:] == '\\':
            raise EParseInvalidBackslash(self)

        if self.parse_directive_re is None:
            self.parse_directive_re = re.compile(
                r'^#\s*(?:'
                    r'(?:[Ll][Ii][Nn][Ee]\s+)?(\d+)(?:\s+("(?:[^"\\]|\\.)*"))?'
                    r'|'
                    r'([A-Za-z0-9_]+)\s+([A-Za-z0-9_]+)\s+([-+,A-Za-z0-9_]+)'
                r')\s*$'
            )
        match = self.parse_directive_re.search(directive)
        if match is not None:
            # Something parsed
            if match.group(1) is not None:
                #line directive
                if match.group(2) is not None:
                    # filename included
                    if match.group(2).find('\\') != -1:
                        # interpret escapes
                        from ast import literal_eval
                        filename = literal_eval(match.group(2))
                    else:
                        filename = match.group(2)[1:-1]
                    # TODO: what do we do with the filename?
                    filename # keep pyflakes happy

                    del filename
                linenum = int(match.group(1))
                linenum # keep pyflakes happy
                # TODO: process line number
                del linenum
            else:
                assert match.group(3) is not None
                if match.group(3).lower() == 'pragma' and match.group(4) == 'OPT':
                    opts = match.group(5).lower().split(',')
                    for opt in opts:
                        if opt != '':
                            if opt[0] == '-':
                                self.SetOpt(opt[1:], False)
                            elif opt[0] == '+':
                                self.SetOpt(opt[1:], True)
                            else:
                                self.SetOpt(opt, True)

    def GetToken(self):
        """Lexer"""

        try:
            while self.pos < self.length:
                # If an error occurs, it will happen at the start of this token.
                self.errorpos = self.pos

                c = self.script[self.pos]
                self.pos += 1

                # Process preprocessor directives
                if self.processpre and self.linestart and c == '#':
                    # Preprocessor directive.
                    # Most are not supposed to reach us but some do:
                    # - gcpp generates lines in the output like:
                    #       # 123 "file.lsl"
                    # - other preprocessors including Boost Wave and mcpp
                    #   generate lines like:
                    #       #line 123 "file.lsl"
                    #   Firestorm comments these out and instead outputs
                    #   //#line 123 "file.lsl"
                    # - #pragma directives
                    # - #define directives from mcpp's #pragma MCPP put_defines
                    #   or from gcpp's -dN option, that we use to detect some
                    #   definitions.
                    self.ceof()
                    while self.script[self.pos] != '\n':
                        self.pos += 1
                        self.ceof() # A preprocessor command at EOF is not unexpected EOF.

                    self.ProcessDirective(self.script[self.errorpos:self.pos])

                    self.pos += 1
                    self.ceof()
                    continue

                # Process comments
                if c == '/':
                    if self.script[self.pos:self.pos+1] == '/':
                        self.pos += 1
                        self.ceof()
                        while self.script[self.pos] != '\n':
                            self.pos += 1
                            self.ceof() # A single-line comment at EOF is not unexpected EOF.

                        self.linestart = True
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

                # self.linestart is related to the preprocessor, therefore we
                # check the characters that are relevant for standard C.
                if c not in ' \n\r\x0B\x0C':
                    self.linestart = False

                # Process strings
                if c == '"' or c == 'L' and self.script[self.pos:self.pos+1] == '"':
                    strliteral = ''
                    if c == 'L':
                        self.pos += 1
                        strliteral = '"'

                    savepos = self.pos # we may need to backtrack
                    is_string = True # by default

                    while self.script[self.pos:self.pos+1] != '"':
                        # per the grammar, on EOF, it's not considered a string
                        if self.pos >= self.length:
                            self.pos = savepos
                            is_string = False
                            break

                        if self.script[self.pos] == '\\':
                            self.pos += 1
                            self.ueof()
                            if self.script[self.pos] == 'n':
                                strliteral += '\n'
                            elif self.script[self.pos] == 't':
                                strliteral += '    '
                            elif self.script[self.pos] == '\n':
                                # '\' followed by a newline; it's not a string.
                                self.pos = savepos
                                is_string = False
                                self.linestart = True
                                break
                            else:
                                strliteral += self.script[self.pos]
                        else:
                            strliteral += self.script[self.pos]
                        self.pos += 1

                    if is_string:
                        self.pos += 1
                        return ('STRING_VALUE', lslfuncs.zstr(strliteral.decode('utf8')))
                    # fall through (to consider the L or to ignore the ")

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

                if self.script[self.pos-1:self.pos+1] in self.double_toks \
                   or self.extendedassignment and self.script[self.pos-1:self.pos+1] in self.extdouble_toks:
                    self.pos += 1
                    if self.extendedassignment and self.script[self.pos-2:self.pos+1] in ('<<=', '>>='):
                        self.pos += 1
                        return (self.script[self.pos-3:self.pos],)
                    return (self.script[self.pos-2:self.pos],)

                if c in '.;{},=()-+*/%@:<>[]&|^~!' and c != '':
                    return (c,)

                if c == '\n':
                    self.linestart = True
                # We eat spacers AND any other character, so the following is not needed,
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

    # Recursive-descendent parser. The result is an AST and a symbol table.

    def expect(self, toktype):
        """Raise exception if the current token is not the given one."""
        if self.tok[0] != toktype:
            if self.tok[0] == 'EOF':
                raise EParseUEOF(self)
            raise EParseSyntax(self)

    def does_something(self, blk):
        """Tell if a list of nodes does something or is just empty statements
        (a pure combination of ';' and '{}' and '@')
        """
        for node in blk:
            if '@' != node['nt'] != ';':
                if node['nt'] == '{}':
                    if self.does_something(node['ch']):
                        return True
                else:
                    return True
        return False

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
        backtrack if it causes an error.
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
        except EParse: # The errors can be varied, e.g. <0,0,0>-v; raises EParseTypeMismatch
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
            ltype = inequality['t']
            if ltype not in ('integer', 'float'):
                raise EParseTypeMismatch(self)
            rexpr = self.Parse_shift()
            rtype = rexpr['t']
            if rtype not in ('integer', 'float'):
                raise EParseTypeMismatch(self)
            if ltype != rtype:
                if rtype == 'float':
                    inequality = self.autocastcheck(inequality, rtype)
                else:
                    rexpr = self.autocastcheck(rexpr, ltype)
            inequality = {'nt':op, 't':'integer', 'ch':[inequality, rexpr]}

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
            | IDENT '[' expression ']' '=' expression %if lazylists
            | IDENT '[' expression ']' %if lazylists
            | lvalue
        vector_literal: '<' expression ',' expression ',' expression '>'
        rotation_literal: '<' expression ',' expression ',' expression
            ',' expression '>'
        list_literal: '[' optional_expression_list ']'
        assignment: lvalue '=' expression | lvalue '+=' expression
            | lvalue '-=' expression | lvalue '*=' expression
            | lvalue '/=' expression | lvalue '%=' expression
            | lvalue '|=' expression %if extendedassignment
            | lvalue '&=' expression %if extendedassignment
            | lvalue '<<=' expression %if extendedassignment
            | lvalue '>>=' expression %if extendedassignment
        lvalue: IDENT | IDENT '.' IDENT
        """
        tok0 = self.tok[0]
        val = self.tok[1] if len(self.tok) > 1 else None
        CONST = 'CONST'
        if tok0 == '-':
            self.NextToken()
            if self.tok[0] in ('INTEGER_VALUE', 'FLOAT_VALUE'):
                val = self.tok[1]
                self.NextToken()
                return {'nt':CONST, 't':'integer' if type(val) == int else 'float', 'value':-val}
        if tok0 == 'INTEGER_VALUE':
            self.NextToken()
            return {'nt':CONST, 't':'integer', 'value':val}
        if tok0 == 'FLOAT_VALUE':
            self.NextToken()
            return {'nt':CONST, 't':'float', 'value':val}
        if tok0 == 'STRING_VALUE':
            self.NextToken()
            if self.allowmultistrings:
                while self.tok[0] == 'STRING_VALUE':
                    val += self.tok[1]
                    self.NextToken()
            return {'nt':CONST, 't':'string', 'value':val}
        # Key constants are not currently supported - use string
        #if tok0 == 'KEY_VALUE':
        #    return [CONST, 'key', val]
        if tok0 == 'VECTOR_VALUE':
            self.NextToken()
            return {'nt':CONST, 't':'vector', 'value':val}
        if tok0 == 'ROTATION_VALUE':
            self.NextToken()
            return {'nt':CONST, 't':'rotation', 'value':val}
        if tok0 == 'LIST_VALUE':
            self.NextToken()
            return {'nt':CONST, 't':'list', 'value':val}
        if tok0 in ('TRUE', 'FALSE'):
            self.NextToken()
            return {'nt':CONST, 't':'integer', 'value':int(tok0 == 'TRUE')}
        if tok0 == '<':
            self.NextToken()
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
            #    return ['VECTOR', 'vector'] + val
            #self.expect(',')
            #self.NextToken()
            #val.append(self.Parse_inequality())
            #self.expect('>')
            #self.NextToken()
            #return ['ROTATION', 'rotation'] + val

            # Alas, it isn't. The closing angle bracket of a vector '>'
            # conflicts with the inequality operator '>' in unexpected ways.
            # Example: <2,2,2> * 2 would trigger the problem with that code:
            # the expression parser would try to parse the inequality 2 > *2,
            # choking at the *. To make things worse, LSL admits things such as
            # <2,2,2 > 2> (but not things like <2,2,2 == 2> because the == has
            # lower precedence than the '>' and thus it forces termination of
            # the vector constant). And to make things even worse, it also
            # admits things such as <2,2,2 == 2, 2> because the comma is not in
            # the precedence scale, so it's quite complex to handle.

            # We defer it to a separate function.
            val += self.Parse_vector_rotation_tail()

            if len(val) == 3:
                return {'nt':'VECTOR', 't':'vector', 'ch':val}
            return {'nt':'ROTATION', 't':'rotation', 'ch':val}

        if tok0 == '[':
            self.NextToken()
            val = self.Parse_optional_expression_list(False)
            self.expect(']')
            self.NextToken()
            return {'nt':'LIST', 't':'list', 'ch':val}
        if tok0 == 'PRINT':
            self.NextToken()
            self.expect('(')
            self.NextToken()
            expr = self.Parse_expression()
            if expr['t'] not in self.types:
                raise EParseTypeMismatch(self) if expr['t'] is None else EParseUndefined(self)
            self.expect(')')
            self.NextToken()
            # Syntactically, print returns the same type as the expression.
            # However, compilation in Mono throws an exception, and even in
            # LSO, it throws a bounds check error when the result is a string
            # or key or list and the returned value is used.
            return {'nt':'PRINT', 't':expr['t'], 'ch':[expr]}

        if tok0 != 'IDENT':
            if tok0 == 'EOF':
                raise EParseUEOF(self)
            raise EParseSyntax(self)
        name = val
        self.NextToken()

        # Course of action decided here.
        tok0 = self.tok[0]
        if tok0 == '(':
            # Function call
            self.NextToken()

            # Functions are looked up in the global scope only.
            sym = self.FindSymbolFull(val, 0)
            if sym is None:
                raise EParseUndefined(self)

            if sym['Kind'] != 'f':
                raise EParseUndefined(self)
            args = self.Parse_optional_expression_list(sym['ParamTypes'])
            self.expect(')')
            self.NextToken()
            return {'nt':'FNCALL', 't':sym['Type'], 'name':name, 'ch':args}

        sym = self.FindSymbolFull(val)
        if sym is None or sym['Kind'] != 'v':
            raise EParseUndefined(self)

        typ = sym['Type']
        lvalue = {'nt':'IDENT', 't':typ, 'name':name, 'scope':sym['Scope']}

        # Lazy lists
        if self.lazylists and tok0 == '[':
            self.NextToken()
            if typ != 'list':
                raise EParseTypeMismatch(self)
            idxexpr = self.Parse_optional_expression_list(False)
            self.expect(']')
            self.NextToken()
            if self.tok[0] != '=' or not AllowAssignment:
                return {'nt':'SUBIDX', 't':None, 'ch':[lvalue] + idxexpr}

            # Lazy list assignment
            if len(idxexpr) != 1:
                raise EParseFunctionMismatch(self)
            if idxexpr[0]['t'] != 'integer':
                raise EParseTypeMismatch(self)
            idxexpr = idxexpr[0]
            self.NextToken()
            expr = self.Parse_expression()
            rtyp = expr['t']
            # Define aux function if it doesn't exist
            # (leaves users room for writing their own replacement, e.g.
            # one that fills with something other than zeros)
            if 'lazy_list_set' not in self.symtab[0]:
                self.PushScope()
                paramscope = self.scopeindex
                params = (['list', 'integer', 'list'],
                          ['L', 'i', 'v'])
                self.AddSymbol('f', 0, 'lazy_list_set', Loc=self.usedspots,
                    Type='list', ParamTypes=params[0], ParamNames=params[1])
                self.AddSymbol('v', paramscope, 'L', Type='list')
                self.AddSymbol('v', paramscope, 'i', Type='integer')
                self.AddSymbol('v', paramscope, 'v', Type='list')
                #self.PushScope() # no locals

                # Add body (apologies for the wall of text)
                # Generated from this source:
                '''
list lazy_list_set(list L, integer i, list v)
{
    while (llGetListLength(L) < i)
        L = L + 0;
    return llListReplaceList(L, v, i, i);
}
                '''
                self.tree[self.usedspots] = {'ch': [{'ch': [{'ch': [{'ch': [{'ch': [{'scope': paramscope, 'nt': 'IDENT', 't': 'list', 'name': 'L'}], 'nt': 'FNCALL', 't': 'integer', 'name': 'llGetListLength'}, {'scope': paramscope, 'nt': 'IDENT', 't': 'integer', 'name': 'i'}], 'nt': '<', 't': 'integer'}, {'ch': [{'ch': [{'scope': paramscope, 'nt': 'IDENT', 't': 'list', 'name': 'L'}, {'ch': [{'scope': paramscope, 'nt': 'IDENT', 't': 'list', 'name': 'L'}, {'nt': 'CONST', 't': 'integer', 'value': 0}], 'nt': '+', 't': 'list'}], 'nt': '=', 't': 'list'}], 'nt': 'EXPR', 't': 'list'}], 'nt': 'WHILE', 't': None}, {'ch': [{'ch': [{'scope': paramscope, 'nt': 'IDENT', 't': 'list', 'name': 'L'}, {'scope': paramscope, 'nt': 'IDENT', 't': 'list', 'name': 'v'}, {'scope': paramscope, 'nt': 'IDENT', 't': 'integer', 'name': 'i'}, {'scope': paramscope, 'nt': 'IDENT', 't': 'integer', 'name': 'i'}], 'nt': 'FNCALL', 't': 'list', 'name': 'llListReplaceList'}], 'nt': 'RETURN', 't': None, 'LIR': True}], 'nt': '{}', 't': None, 'LIR': True}], 't': 'list', 'pnames': params[1], 'scope': 0, 'pscope': paramscope, 'nt': 'FNDEF', 'ptypes': params[0], 'name': 'lazy_list_set'}
                self.usedspots += 1
                #self.PopScope() # no locals
                self.PopScope()

            if expr['t'] is None:
                raise EParseTypeMismatch(self)
            if expr['t'] != 'list':
                expr = {'nt':'CAST', 't':'list', 'ch':[expr]}

            return {'nt':'=', 't':'list', 'ch':[lvalue, {
                    'nt':'FNCALL', 't':'list', 'name':'lazy_list_set',
                    'scope':0,
                    'ch':[lvalue.copy(), idxexpr, expr]
                }]}

        if tok0 == '.':
            self.NextToken()
            self.expect('IDENT')
            self.ValidateField(typ, self.tok[1])
            lvalue = {'nt':'FLD', 't':'float', 'ch':[lvalue], 'fld':self.tok[1]}
            self.NextToken()
            tok0 = self.tok[0]
            typ = 'float'

        if tok0 in ('++', '--'):
            self.NextToken()
            if lvalue['t'] not in ('integer', 'float'):
                raise EParseTypeMismatch(self)
            return {'nt':'V++' if tok0 == '++' else 'V--', 't':lvalue['t'], 'ch':[lvalue]}
        if AllowAssignment and (tok0 in self.assignment_toks
                                or self.extendedassignment
                                   and tok0 in self.extassignment_toks):
            self.NextToken()
            expr = self.Parse_expression()
            rtyp = expr['t']
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
                expr = self.autocastcheck(expr, typ)

                return {'nt':'=', 't':typ, 'ch':[lvalue, expr]}

            if tok0 == '+=':
                if typ == 'float':
                    expr = self.autocastcheck(expr, typ)
                if rtyp != typ != 'list' or typ == rtyp == 'key':
                    # key + key is the only disallowed combo of equal types
                    raise EParseTypeMismatch(self)
                if self.explicitcast:
                    if typ == 'list' != rtyp:
                        expr = {'nt':'CAST', 't':typ, 'ch':[expr]}
                return {'nt':tok0, 't':typ, 'ch':[lvalue, expr]}

            if tok0 == '-=':
                if typ == rtyp in ('integer', 'float', 'vector', 'rotation'):
                    return {'nt':tok0, 't':typ, 'ch':[lvalue, expr]}
                raise EParseTypeMismatch(self)

            if tok0 in ('*=', '/='):
                # There is a special case that was dealt with before.
                if tok0 == '*=' and typ == 'integer' and rtyp == 'float':
                    return {'nt':tok0, 't':typ, 'ch':[lvalue, expr]}

                if (typ == rtyp or typ == 'vector') and rtyp in ('integer', 'float', 'rotation'):
                    if typ == 'vector' and rtyp == 'integer':
                        expr = self.autocastcheck(expr, 'float')
                    return {'nt':tok0, 't':typ, 'ch':[lvalue, expr]}
                raise EParseTypeMismatch(self)

            if tok0 == '%=':
                if typ == rtyp in ('integer', 'vector'):
                    return {'nt':tok0, 't':typ, 'ch':[lvalue, expr]}
                raise EParseTypeMismatch(self)

            # Rest take integer operands only

            if typ == rtyp == 'integer':
                return {'nt':tok0, 't':typ, 'ch':[lvalue, expr]}
            raise EParseTypeMismatch(self)

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
            if value['t'] not in ('integer', 'float', 'vector', 'rotation'):
                raise EParseTypeMismatch(self)
            return {'nt':'NEG', 't':value['t'], 'ch':[value]}
        if tok0 in ('!', '~'):
            # Unary logic and bitwise NOT - applies to integers only
            self.NextToken()
            value = self.Parse_unary_expression()
            if value['t'] != 'integer':
                raise EParseTypeMismatch(self)
            return {'nt':tok0, 't':'integer', 'ch':[value]}
        if tok0 in ('++', '--'):
            # Pre-increment / pre-decrement
            self.NextToken()
            self.expect('IDENT')
            name = self.tok[1]
            sym = self.FindSymbolFull(name)
            if sym is None or sym['Kind'] != 'v':
                # Pretend it doesn't exist
                raise EParseUndefined(self)
            typ = sym['Type']

            ret = {'nt':'IDENT', 't':typ, 'name':name, 'scope':sym['Scope']}
            self.NextToken()
            if self.tok[0] == '.':
                self.NextToken()
                self.expect('IDENT')
                self.ValidateField(typ, self.tok[1])
                ret = {'nt':'FLD', 't':'float', 'ch':[ret], 'fld':self.tok[1]}
                self.NextToken()

            typ = ret['t']
            if typ not in ('integer', 'float'):
                raise EParseTypeMismatch(self)

            return {'nt':'++V' if tok0 == '++' else '--V', 't':typ, 'ch':[ret]}

        if tok0 == '(':
            # Parenthesized expression or typecast

            self.NextToken()
            if self.tok[0] != 'TYPE':
                # Parenthesized expression
                expr = self.Parse_expression()
                self.expect(')')
                self.NextToken()
                return expr

            # Typecast
            typ = self.tok[1]
            self.NextToken()
            self.expect(')')
            self.NextToken()

            if self.extendedtypecast:
                # Allow any unary expression (except assignment). The type cast
                # acts as a prefix operator.

                # Deal with the case of minus a constant integer or float.
                #  E.g. ~(integer)-2*3 should be parsed as (~(integer)-2)*3
                #  and not as ~(integer)(-(2*3))
                # Note ~(integer)-a*3 is also parsed as ~(integer)(-a)*3
                # which is bordering a violation of the POLA because of the
                # priority of - with respect to *. But the syntax is quite
                # explicit: what is typecast is always a unary expression,
                # therefore processed first.
                if self.tok[0] == '-':
                    self.NextToken()
                    if self.tok[0] == 'INTEGER_VALUE':
                        expr = {'nt':'CONST','t':'integer','value':-self.tok[1]}
                        self.NextToken()
                    elif self.tok[0] == 'FLOAT_VALUE':
                        expr = {'nt':'CONST','t':'float','value':-self.tok[1]}
                        self.NextToken()
                    else:
                        expr = self.Parse_unary_expression(AllowAssignment = False)
                        expr = {'nt':'NEG','t':expr['t'],'ch':[expr]}
                else:
                    expr = self.Parse_unary_expression(AllowAssignment = False)
            else:
                if self.tok[0] == '(':
                    self.NextToken()
                    expr = self.Parse_expression()
                    self.expect(')')
                    self.NextToken()
                else:
                    expr = self.Parse_unary_postfix_expression(AllowAssignment = False)
            basetype = expr['t']
            if self.lazylists and basetype is None and expr['nt'] == 'SUBIDX':
                fn = self.TypeToExtractionFunction[typ]
                sym = self.FindSymbolFull(fn, 0)
                if sym is None:
                    # in the unlikely event that the underlying function is not
                    # defined in builtins.txt, throw a syntax error (making a
                    # new exception just for this seems overkill, and throwing
                    # an unknown identifier error would be confusing)
                    raise EParseSyntax(self)
                fnparamtypes = sym['ParamTypes']
                subparamtypes = [x['t'] for x in expr['ch']]
                if fnparamtypes != subparamtypes:
                    raise EParseFunctionMismatch(self)
                return {'nt':'FNCALL', 't':sym['Type'], 'name':fn, 'scope':0,
                    'ch':expr['ch']}

            if typ == 'list' and basetype in self.types \
               or basetype in ('integer', 'float') and typ in ('integer', 'float', 'string') \
               or basetype == 'string' and typ in self.types \
               or basetype == 'key' and typ in ('string', 'key') \
               or basetype == 'vector' and typ in ('string', 'vector') \
               or basetype == 'rotation' and typ in ('string', 'rotation') \
               or basetype == 'list' and typ == 'string':
                return {'nt':'CAST', 't':typ, 'ch':[expr]}
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
            ltype = factor['t']
            # Acceptable types for LHS
            if op in ('*', '/') and ltype not in ('integer', 'float',
                                                  'vector', 'rotation') \
               or op == '%' and ltype not in ('integer', 'vector'):
                raise EParseTypeMismatch(self)
            self.NextToken()
            rexpr = self.Parse_unary_expression()
            rtype = rexpr['t']
            # Mod is easier to check for
            if op == '%' and ltype != rtype:
                raise EParseTypeMismatch(self)
            if op == '%' or ltype == rtype == 'integer':
                # Deal with the special cases first (it's easy)
                factor = {'nt':op, 't':ltype, 'ch':[factor, rexpr]}
            else:
                # Any integer must be promoted to float now
                if ltype == 'integer':
                    ltype = 'float'
                    factor = self.autocastcheck(factor, ltype)
                if rtype == 'integer':
                    rtype = 'float'
                    rexpr = self.autocastcheck(rexpr, rtype)
                if ltype == 'float' and rtype in ('float', 'vector') \
                   or ltype == 'vector' and rtype in ('float', 'vector', 'rotation') \
                   or ltype == rtype == 'rotation':
                    if op == '/' and rtype == 'vector':
                        # Division by vector isn't valid
                        raise EParseTypeMismatch(self)
                    # The rest are valid
                    if ltype == 'float' and rtype == 'vector':
                        resulttype = rtype
                    elif ltype == rtype == 'vector':
                        resulttype = 'float'
                    else:
                        resulttype = ltype
                    factor = {'nt':op, 't':resulttype, 'ch':[factor, rexpr]}
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
            ltype = term['t']
            if op == '+' and ltype not in self.types \
               or op == '-' and ltype not in ('integer', 'float',
                                              'vector', 'rotation'):
                raise EParseTypeMismatch(self)
            self.NextToken()
            rexpr = self.Parse_factor()
            rtype = rexpr['t']
            # This is necessary, but the reason is subtle.
            # The types must match in principle (except integer/float), so it
            # doesn't seem necessary to check rtype. But there's the case
            # where the first element is a list, where the types don't need to
            # match but the second type must make sense.
            if op == '+' and rtype not in self.types:
               #or op == '-' and rtype not in ('integer', 'float',
               #                               'vector', 'rotation'):
                raise EParseTypeMismatch(self)
            # Isolate the additions where the types match to make our life easier later
            if op == '+' and (ltype == rtype or ltype == 'list' or rtype == 'list'):
                if ltype == rtype == 'key':
                    # key + key is the only disallowed combo of equals
                    raise EParseTypeMismatch(self)
                # Note that although list + nonlist is semantically the
                # same as list + (list)nonlist, and similarly for
                # nonlist + list, they don't compile to the same thing,
                # so we don't act on self.explicitcast in this case.
                if rtype == 'list':
                    ltype = rtype
                term = {'nt':op, 't':ltype, 'ch':[term, rexpr]}
            elif self.allowkeyconcat and op == '+' \
                 and ltype in ('key', 'string') and rtype in ('key', 'string'):
                # Allow string+key addition (but add explicit cast)
                if ltype == 'key':
                    term = {'nt':op, 't':rtype,
                        'ch':[{'nt':'CAST', 't':rtype, 'ch':[term]}, rexpr]}
                else:
                    term = {'nt':op, 't':ltype,
                        'ch':[term, {'nt':'CAST', 't':ltype, 'ch':[rexpr]}]}
            elif ltype == 'key' or rtype == 'key':
                # Only list + key or key + list is allowed, otherwise keys can't
                # be added or subtracted with anything.
                raise EParseTypeMismatch(self)
            else:
                if ltype == 'float':
                    # Promote rexpr to float
                    term = {'nt':op, 't':ltype, 'ch':[term, self.autocastcheck(rexpr, ltype)]}
                else:
                    # Convert LHS to rtype if possible (note no keys arrive here)
                    term = {'nt':op, 't':rtype, 'ch':[self.autocastcheck(term, rtype), rexpr]}

        return term

    def Parse_shift(self):
        """Grammar parsed here:

        shift: term | shift '<<' term | shift '>>' term
        """
        shift = self.Parse_term()
        while self.tok[0] in ('<<', '>>'):
            if shift['t'] != 'integer':
                raise EParseTypeMismatch(self)
            op = self.tok[0]
            self.NextToken()
            rexpr = self.Parse_term()
            if rexpr['t'] != 'integer':
                raise EParseTypeMismatch(self)
            shift = {'nt':op, 't':'integer', 'ch':[shift , rexpr]}

        return shift

    def Parse_inequality(self):
        """Grammar parsed here:

        inequality: shift | inequality '<' shift | inequality '<=' shift
            | inequality '>' shift | inequality '>=' shift
        """
        inequality = self.Parse_shift()
        while self.tok[0] in ('<', '<=', '>', '>='):
            op = self.tok[0]
            ltype = inequality['t']
            if ltype not in ('integer', 'float'):
                raise EParseTypeMismatch(self)
            self.NextToken()
            rexpr = self.Parse_shift()
            rtype = rexpr['t']
            if rtype not in ('integer', 'float'):
                raise EParseTypeMismatch(self)
            if ltype != rtype:
                if rtype == 'float':
                    inequality = self.autocastcheck(inequality, rtype)
                else:
                    rexpr = self.autocastcheck(rexpr, ltype)
            inequality = {'nt':op, 't':'integer', 'ch':[inequality, rexpr]}

        return inequality

    def Parse_comparison(self):
        """Grammar parsed here:

        comparison: inequality | comparison '==' inequality
            | comparison '!=' inequality
        """
        comparison = self.Parse_inequality()
        while self.tok[0] in ('==', '!='):
            op = self.tok[0]
            ltype = comparison['t']
            if ltype not in self.types:
                raise EParseTypeMismatch(self)
            self.NextToken()
            rexpr = self.Parse_inequality()
            rtype = rexpr['t']
            if ltype == 'float':
                rexpr = self.autocastcheck(rexpr, ltype)
            else:
                # For string & key, RHS (rtype) mandates the conversion
                # (that's room for optimization: always compare strings)
                comparison = self.autocastcheck(comparison, rtype)
            comparison = {'nt':op, 't':'integer', 'ch':[comparison, rexpr]}

        return comparison

    def Parse_bitbool_factor(self):
        """Grammar parsed here:

        bitbool_factor: comparison | bitbool_factor '&' comparison
        """
        bitbool_factor = self.Parse_comparison()
        while self.tok[0] == '&':
            if bitbool_factor['t'] != 'integer':
                raise EParseTypeMismatch(self)
            op = self.tok[0]
            self.NextToken()
            rexpr = self.Parse_comparison()
            if rexpr['t'] != 'integer':
                raise EParseTypeMismatch(self)
            bitbool_factor = {'nt':op, 't':'integer', 'ch':[bitbool_factor, rexpr]}

        return bitbool_factor

    def Parse_bitxor_term(self):
        """Grammar parsed here:

        bitxor_term: bitbool_factor | bitxor_term '^' bitbool_factor
        """
        bitxor_term = self.Parse_bitbool_factor()
        while self.tok[0] == '^':
            if bitxor_term['t'] != 'integer':
                raise EParseTypeMismatch(self)
            op = self.tok[0]
            self.NextToken()
            rexpr = self.Parse_bitbool_factor()
            if rexpr['t'] != 'integer':
                raise EParseTypeMismatch(self)
            bitxor_term = {'nt':op, 't':'integer', 'ch':[bitxor_term, rexpr]}

        return bitxor_term

    def Parse_bitbool_term(self):
        """Grammar parsed here:

        bitbool_term: bitxor_term | bitbool_term '|' bitxor_term
        """
        bitbool_term = self.Parse_bitxor_term()
        while self.tok[0] == '|':
            if bitbool_term['t'] != 'integer':
                raise EParseTypeMismatch(self)
            op = self.tok[0]
            self.NextToken()
            rexpr = self.Parse_bitxor_term()
            if rexpr['t'] != 'integer':
                raise EParseTypeMismatch(self)
            bitbool_term = {'nt':op, 't':'integer', 'ch':[bitbool_term, rexpr]}

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
            if expression['t'] != 'integer':
                raise EParseTypeMismatch(self)
            op = self.tok[0]
            self.NextToken()
            rexpr = self.Parse_bitbool_term()
            if rexpr['t'] != 'integer':
                raise EParseTypeMismatch(self)
            expression = {'nt':op, 't':'integer', 'ch':[expression, rexpr]}

        return expression

    def Parse_optional_expression_list(self, expected_types = None):
        """Grammar parsed here:

        optional_expression_list: LAMBDA | expression_list
        expression_list: expression | expression_list ',' expression
        """
        # Recursive descendent parsers are nice, but not exempt of problems.
        # We need to accept empty lists. This is a maze of which we get out
        # with a dirty hack. Rather than attempt to parse as an expression and
        # backtrack in case of error, we check the next token to see if it
        # is one that closes the expression list.
        # optional_expression_list is used by FOR loops (closed by ';' or ')'),
        # list constants and lazy lists (closed by ']') and function arguments
        # (closed by ')'). If it's not the right token, we'll err anyway upon
        # return.
        ret = []
        idx = 0
        if self.tok[0] not in (']', ')', ';'):
            while True:
                expr = self.Parse_expression()
                if False is not expected_types is not None:
                    if idx >= len(expected_types):
                        raise EParseFunctionMismatch(self)
                    try:
                        expr = self.autocastcheck(expr, expected_types[idx]);
                    except EParseTypeMismatch:
                        raise EParseFunctionMismatch(self)
                elif expected_types is False: # don't accept void expressions
                    if expr['t'] not in self.types:
                        raise EParseTypeMismatch(self)
                idx += 1
                ret.append(expr)
                if self.tok[0] != ',':
                    break
                self.NextToken()
        if False is not expected_types is not None and idx != len(expected_types):
            raise EParseFunctionMismatch(self)
        return ret

    def Parse_statement(self, ReturnType, AllowDecl = False, AllowStSw = False,
        InsideSwitch = False, InsideLoop = False):
        """Grammar parsed here:

        statement: ';' | single_statement | code_block
        single_statement: if_statement | while_statement | do_statement
            | for_statement | jump_statement | state_statement | label_statement
            | return_statement | declaration_statement | expression ';'
            | switch_statement %if enableswitch
            | case_statement %if enableswitch and InsideSwitch
            | break_statement %if enableswitch and InsideSwitch or breakcont and InsideLoop
            | continue_statement %if breakcont and InsideLoop
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
        switch_statement: SWITCH '(' expression ')' code_block
        case_statement: CASE expression ':' | CASE expression code_block
            | DEFAULT ':' | DEFAULT code_block
        break_statement: BREAK ';'
        continue_statement: CONTINUE ';'

        There's a restriction: a *single* statement can not be a declaration.
        For example: if (1) integer x; is not allowed.

        Note that SWITCH expects a code block because CASE is a full statement
        for us, rather than a label. So for example this wouldn't work:
        switch (expr) case expr: stmt; // works in C but not in this processor
        but this works in both: switch (expr) { case expr: stmt; }
        """
        tok0 = self.tok[0]

        if tok0 == '{':
            return self.Parse_code_block(ReturnType, AllowStSw = AllowStSw,
                InsideSwitch = InsideSwitch, InsideLoop = InsideLoop)

        if tok0 == ';':
            self.NextToken()
            return {'nt':';', 't':None}

        if tok0 == '@':
            self.NextToken()
            self.expect('IDENT')
            name = self.tok[1]
            if name in self.symtab[self.scopeindex]:
                raise EParseAlreadyDefined(self)
            # shrinknames *needs* all labels renamed, so they are out of the way
            if self.duplabels or self.shrinknames:
                # Duplicate labels allowed.
                if name in self.locallabels or self.shrinknames:
                    # Generate a new unique name and attach it to the symbol.
                    unique = self.GenerateLabel()
                    self.AddSymbol('l', self.scopeindex, name, NewName=unique)
                else:
                    # Use the existing name. Faster and more readable.
                    unique = name
                    self.locallabels.add(name)
                    self.AddSymbol('l', self.scopeindex, name)

            else:
                # Duplicate labels disallowed.
                # All labels go to a common pool local to the current function.
                # Check if it's already there, and add it otherwise.
                if name in self.locallabels:
                    raise EParseDuplicateLabel(self)
                self.locallabels.add(name)
                self.AddSymbol('l', self.scopeindex, name)
            self.NextToken()
            self.expect(';')
            self.NextToken()
            return {'nt':'@', 't':None, 'name':name, 'scope':self.scopeindex}

        if tok0 == 'JUMP':
            self.NextToken()
            self.expect('IDENT')
            name = self.tok[1]
            sym = self.FindSymbolPartial(name, MustBeLabel=True)
            jumpnode = {'nt':'JUMP', 't':None, 'name':name}
            if not sym or sym['Kind'] != 'l':
                # It might still be a forward reference, so we add it to the
                # list of things to look up when done
                self.jump_lookups.append((name, self.scopeindex, self.errorpos, jumpnode))
            else:
                jumpnode['scope'] = sym['Scope']
            self.NextToken()
            self.expect(';')
            self.NextToken()
            return jumpnode

        if tok0 == 'STATE':
            self.NextToken()
            if self.tok[0] not in ('DEFAULT', 'IDENT'):
                raise EParseSyntax(self)
            # State Switch only searches for states in the global scope
            name = self.tok[1] if self.tok[0] == 'IDENT' else 'default'
            if name not in self.symtab[0] and (name not in self.globals or self.globals[name]['Kind'] != 's'):
                raise EParseUndefined(self)
            self.NextToken()
            self.expect(';')
            self.NextToken()
            if self.localevents is None and not AllowStSw:
                raise EParseCantChangeState(self)
            return {'nt':'STSW', 't':None, 'name':name, 'scope':0}

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
                return {'nt':'RETURN', 't':None}
            # Sets LastIsReturn flag too
            return {'nt':'RETURN', 't':None, 'LIR':True,
                'ch':[self.autocastcheck(value, ReturnType)]}

        if tok0 == 'IF':
            ret = {'nt':'IF', 't':None, 'ch':[]}
            self.NextToken()
            self.expect('(')
            self.NextToken()
            ret['ch'].append(self.Parse_expression())
            self.expect(')')
            self.NextToken()
            # INCOMPATIBILITY NOTE: This is more permissive than LSL.
            # In LSL, an if...then...else does NOT allow a state change
            # in either branch. Only an if...then without else does.
            # BUT since the decision to allow or not needs to be taken before
            # the 'else' is found, we're not going to check the branch after
            # parsing, only for the sake of reporting that error. The compiler
            # will report it.
            ret['ch'].append(self.Parse_statement(ReturnType, AllowStSw = True, InsideLoop = InsideLoop))
            if self.tok[0] == 'ELSE':
                LastIsReturn = 'LIR' in ret['ch'][1]
                self.NextToken()
                ret['ch'].append(self.Parse_statement(ReturnType, AllowStSw = AllowStSw, InsideLoop = InsideLoop))
                if LastIsReturn and 'LIR' in ret['ch'][2]:
                    ret['LIR'] = True
            return ret

        if tok0 == 'WHILE':
            self.NextToken()
            if self.breakcont:
                # We may add braces - or not. The safe approach is to assume
                # we always do and open a new scope for it. At worst it will be
                # empty. At least it is not reflected as braces in the code if
                # braces are not used.
                #
                # This is designed to deal with cases like:
                # if (a) while (b) { ... break; }
                #
                # This works by adding braces around the while and the newly
                # added label, like this:
                # if (a) { while (b) { ... jump label; } @label; }
                self.PushScope()

                self.breakstack.append([self.GenerateLabel(), self.scopeindex, False])
                self.continuestack.append([self.GenerateLabel(), None, False])
            self.expect('(')
            self.NextToken()
            condition = self.Parse_expression()
            self.expect(')')
            self.NextToken()
            # To fix a problem with a corner case (LSL allows defining a label
            # in a single statement, at the same scope as the loop, breaking
            # some of our logic), we check if the statement is a label. If so,
            # we pop the scope to parse the statement and push it again.
            # It won't cause scope problems in turn because we won't add any
            # break or continue labels if no break or continue statement is
            # present, which it can't because the statement is a label.
            if self.breakcont and self.tok[0] == '@':
                self.PopScope()
                stmt = self.Parse_statement(ReturnType, AllowStSw = True, InsideLoop = True)
                self.PushScope()
            else:
                stmt = self.Parse_statement(ReturnType, AllowStSw = True, InsideLoop = True)

            ret = {'nt':'WHILE', 't':None, 'ch':[condition, stmt]}

            if self.breakcont:
                last = self.continuestack.pop()
                if last[2]:
                    assert ret['ch'][1]['nt'] == '{}'
                    ret['ch'][1]['ch'].append({'nt':'@', 't':None, 'name':last[0], 'scope':last[1]})
                    self.AddSymbol('l', last[1], last[0])

                last = self.breakstack.pop()
                if last[2]:
                    ret = {'nt':'{}', 't':None, 'ch':[ret, {'nt':'@', 't':None, 'name':last[0], 'scope':last[1]}]}
                    self.AddSymbol('l', last[1], last[0])
                self.PopScope()
            return ret

        if tok0 == 'DO':
            self.NextToken()
            if self.breakcont:
                self.PushScope()

                self.breakstack.append([self.GenerateLabel(), self.scopeindex, False])
                self.continuestack.append([self.GenerateLabel(), None, False])
            if self.breakcont and self.tok[0] == '@':
                self.PopScope()
                stmt = self.Parse_statement(ReturnType, AllowStSw = True, InsideLoop = True)
                self.PushScope()
            else:
                stmt = self.Parse_statement(ReturnType, AllowStSw = True, InsideLoop = True)
            self.expect('WHILE')
            self.NextToken()
            self.expect('(')
            self.NextToken()
            condition = self.Parse_expression()
            self.expect(')')
            self.NextToken()
            self.expect(';')
            self.NextToken()
            ret = {'nt':'DO', 't':None, 'ch':[stmt, condition]}
            if self.breakcont:
                last = self.continuestack.pop()
                if last[2]:
                    assert ret['ch'][0]['nt'] == '{}'
                    ret['ch'][0]['ch'].append({'nt':'@', 't':None, 'name':last[0], 'scope':last[1]})
                    self.AddSymbol('l', last[1], last[0])

                last = self.breakstack.pop()
                if last[2]:
                    ret = {'nt':'{}', 't':None, 'ch':[ret, {'nt':'@', 't':None, 'name':last[0], 'scope':last[1]}]}
                    self.AddSymbol('l', last[1], last[0])
                self.PopScope()
            return ret

        if tok0 == 'FOR':
            self.NextToken()
            if self.breakcont:
                self.PushScope()

                self.breakstack.append([self.GenerateLabel(), self.scopeindex, False])
                self.continuestack.append([self.GenerateLabel(), None, False])
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
            if self.breakcont and self.tok[0] == '@':
                self.PopScope()
                stmt = self.Parse_statement(ReturnType, AllowStSw = True, InsideLoop = True)
                self.PushScope()
            else:
                stmt = self.Parse_statement(ReturnType, AllowStSw = True, InsideLoop = True)
            ret = {'nt':'FOR', 't':None,
                'ch':[{'nt':'EXPRLIST','t':None, 'ch':initializer},
                      condition,
                      {'nt':'EXPRLIST','t':None, 'ch':iterator},
                      stmt]}
            if self.breakcont:
                last = self.continuestack.pop()
                if last[2]:
                    assert ret['ch'][3]['nt'] == '{}'
                    ret['ch'][3]['ch'].append({'nt':'@', 't':None, 'name':last[0], 'scope':last[1]})
                    self.AddSymbol('l', last[1], last[0])

                last = self.breakstack.pop()
                if last[2]:
                    ret = {'nt':'{}', 't':None, 'ch':[ret, {'nt':'@', 't':None, 'name':last[0], 'scope':last[1]}]}
                    self.AddSymbol('l', last[1], last[0])
                self.PopScope()
            return ret

        if tok0 == 'SWITCH':
            self.NextToken()
            self.expect('(')
            self.NextToken()
            expr = self.Parse_expression()
            self.expect(')')
            self.NextToken()
            brk = self.GenerateLabel()
            self.breakstack.append([brk, None, False])
            blk = self.Parse_code_block(ReturnType, AllowStSw = AllowStSw,
                InsideSwitch = True, InsideLoop = InsideLoop)
            blkscope  = self.breakstack[-1][1]

            # Replace the block
            #   switch (expr1) { case expr2: stmts1; break; default: stmts2; }
            # is translated to:
            #   {
            #       if (expr1==expr2) jump label1;
            #       jump labeldef;
            #
            #       @label1;
            #       stmts1;
            #       jump labelbrk;
            #       @labeldef;
            #       stmts2;
            #       @labelbrk;
            #   }
            # The prelude is the ifs and the jumps.
            # The block gets the cases replaced with labels,
            # and the breaks replaced with jumps.

            switchcaselist = []
            switchcasedefault = None
            # Since label scope rules prevent us from being able to jump inside
            # a nested block, only one nesting level is considered.
            assert blk['nt'] == '{}'
            blk = blk['ch'] # Disregard the '{}' - we'll add it back later
            for idx in xrange(len(blk)):
                if blk[idx]['nt'] == 'CASE':
                    lbl = self.GenerateLabel()
                    switchcaselist.append((lbl, blk[idx]['ch'][0]))
                    self.AddSymbol('l', blkscope, lbl)
                    blk[idx] = {'nt':'@', 'name':lbl, 'scope':blkscope}
                elif blk[idx]['nt'] == 'DEFAULTCASE':
                    if switchcasedefault is not None:
                        raise EParseManyDefaults(self)
                    lbl = self.GenerateLabel()
                    switchcasedefault = lbl
                    self.AddSymbol('l', blkscope, lbl)
                    blk[idx] = {'nt':'@', 'name':lbl, 'scope':blkscope}

            prelude = []
            ltype = expr['t']
            for case in switchcaselist:
                rexpr = case[1]
                lexpr = expr
                if ltype == 'float':
                    rexpr = self.autocastcheck(rexpr, ltype)
                else:
                    # For string & key, RHS (rtype) mandates the conversion
                    # (that's room for optimization: always compare strings)
                    lexpr = self.autocastcheck(lexpr, rexpr['t'])
                prelude.append({'nt':'IF', 't':None, 'ch':[
                    {'nt':'==', 't':'integer', 'ch':[lexpr, rexpr]},
                    {'nt':'JUMP', 't':None, 'name':case[0], 'scope':blkscope}
                    ]})

            if switchcasedefault is None:
                if self.errmissingdefault:
                    raise EParseMissingDefault(self)
                # Check if it's worth adding a break label. If there's no
                # executable code, there's no point. However, this check is
                # insufficient. It misses SEF expressions. For that reason,
                # this is best left up to a later optimizer that knows about
                # SEF. But we do a preliminary elimination here.
                if self.does_something(blk):
                    switchcasedefault = brk
                    self.breakstack[-1][2] = True
            else:
                # Check if no code up to the default label does anything.
                # If so, remove the label and don't generate the jump.
                for i in xrange(len(blk)):
                    node = blk[i]
                    if (node['nt'] == '@' and node['name'] == switchcasedefault
                       and node['scope'] == blkscope):
                        switchcasedefault = None
                        del blk[i]
                        break
                    if self.does_something([node]):
                        break
                del i, node

            if switchcasedefault is not None:
                prelude.append({'nt':'JUMP', 't':None, 'name':switchcasedefault,
                    'scope':blkscope})
            last = self.breakstack.pop()
            if last[2]:
                blk.append({'nt':'@', 'name':brk, 'scope':blkscope})
                self.AddSymbol('l', blkscope, brk)
            return {'nt':'{}', 't':None, 'ch':prelude + blk}

        if tok0 == 'CASE':
            if not InsideSwitch:
                raise EParseInvalidCase(self, u"case")
            if self.scopeindex != self.breakstack[-1][1]:
                # If this block is nested and not the main switch block, this
                # won't work. LSL label scope rules don't expose the nested
                # labels. Nothing we can do about that.
                raise EParseCaseNotAllowed(self, u"case")
            self.NextToken()
            expr = self.Parse_expression()
            if self.tok[0] == ':':
                self.NextToken()
            elif self.tok[0] != '{':
                raise EParseSyntax(self)
            return {'nt':'CASE', 't':None, 'ch':[expr]}

        if tok0 == 'DEFAULT':
            if self.enableswitch:
                if not InsideSwitch:
                    raise EParseInvalidCase(self, u"default")
                if self.scopeindex != self.breakstack[-1][1]:
                    # If this block is nested and not the main switch block, this
                    # won't work. Label scope rules don't expose the nested
                    # labels. Nothing we can do about that.
                    raise EParseCaseNotAllowed(self, u"default")
                self.NextToken()
                if self.tok[0] == ':':
                    self.NextToken()
                elif self.tok[0] != '{':
                    raise EParseSyntax(self)
                return {'nt':'DEFAULTCASE', 't':None}
            # else fall through to eventually fail

        if tok0 == 'BREAK':
            if not self.breakstack:
                raise EParseInvalidBreak(self)
            self.NextToken()
            n = -1
            if self.tok[0] == 'INTEGER_VALUE':
                if self.tok[1] <= 0:
                    raise EParseInvalidBrkContArg(self)
                n = -self.tok[1]
                self.NextToken()
            self.expect(';')
            self.NextToken()
            try:
                self.breakstack[n][2] = True
            except IndexError:
                raise EParseInvalidBrkContArg(self)
            return {'nt':'JUMP', 't':None, 'name':self.breakstack[n][0],
                'scope':self.breakstack[n][1]}

        if tok0 == 'CONTINUE':
            if not self.continuestack:
                raise EParseInvalidCont(self)
            self.NextToken()
            n = -1
            if self.tok[0] == 'INTEGER_VALUE':
                if self.tok[1] <= 0:
                    raise EParseInvalidBrkContArg(self)
                n = -self.tok[1]
                self.NextToken()
            self.expect(';')
            self.NextToken()
            if n == -1 and self.continuestack[-1][1] is None:
                # We're not inside a block - 'continue' is essentially a nop
                # e.g. while (cond) continue; is the same as while (cond) ;
                return {'nt':';', 't':'None'}
            try:
                if self.continuestack[n][1] is None:
                    # this can happen with e.g.:
                    # while (cond) while (cond) while (cond) continue 3;
                    # Transform to while(cond) while(cond) while(cond) break 2;
                    # which is equivalent since there are no {}.
                    n += 1 # e.g. -3 -> -2
                    self.breakstack[n][2] = True # mark the break as used
                    return {'nt':'JUMP', 't':None, 'name':self.breakstack[n][0],
                        'scope':self.breakstack[n][1]}
            except IndexError:
                raise EParseInvalidBrkContArg(self)
            self.continuestack[n][2] = True
            return {'nt':'JUMP', 't':None, 'name':self.continuestack[n][0],
                'scope':self.continuestack[n][1]}

        if tok0 == 'TYPE':
            if not AllowDecl:
                raise EParseDeclarationScope(self)
            typ = self.tok[1]
            self.NextToken()
            self.expect('IDENT')
            name = self.tok[1]
            if name in self.symtab[self.scopeindex]:
                raise EParseAlreadyDefined(self)
            self.NextToken()
            value = None
            decl = {'nt':'DECL','t':typ, 'name':name, 'scope':self.scopeindex}
            if self.tok[0] == '=':
                self.NextToken()
                decl['ch'] = [self.autocastcheck(self.Parse_expression(), typ)]
            self.expect(';')
            self.NextToken()
            self.AddSymbol('v', self.scopeindex, name, Type=typ)
            return decl

        # If none of the above, it must be an expression.
        value = self.Parse_expression()
        self.expect(';')
        self.NextToken()
        return {'nt':'EXPR', 't':value['t'], 'ch':[value]}

    def Parse_code_block(self, ReturnType, AllowStSw = False, InsideSwitch = False,
        InsideLoop = False):
        """Grammar parsed here:

        code_block: '{' statements '}'
        statements: LAMBDA | statements statement

        It receives the return type to expect for return statements.
        """
        self.expect('{')
        self.NextToken()

        self.PushScope()

        # Kludge to find the scope of the break (for switch) /
        # continue (for loops) labels.
        if self.breakstack: # non-empty iff inside loop or switch
            if InsideSwitch and self.breakstack[-1][1] is None:
                self.breakstack[-1][1] = self.scopeindex
            if InsideLoop and self.continuestack[-1][1] is None:
                self.continuestack[-1][1] = self.scopeindex

        body = []
        LastIsReturn = False
        while True:
            if self.tok[0] == '}':
                break
            stmt = self.Parse_statement(ReturnType, AllowDecl = True,
                AllowStSw = AllowStSw, InsideSwitch = InsideSwitch,
                InsideLoop = InsideLoop)
            LastIsReturn = 'LIR' in stmt
            body.append(stmt)

        self.PopScope()

        self.expect('}')
        self.NextToken()

        node = {'nt':'{}', 't':None, 'ch':body}
        if LastIsReturn:
            node['LIR'] = True
        return node

    def Parse_simple_expr(self, ForbidList=False):
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
        if tok[0] in ('TRUE', 'FALSE'): # TRUE and FALSE don't admit sign in globals
            return {'nt':'CONST', 't':'integer', 'value':int(tok[0]=='TRUE')}
        if tok[0] in ('STRING_VALUE', 'KEY_VALUE', 'VECTOR_VALUE', 'ROTATION_VALUE', 'LIST_VALUE'):
            val = tok[1]
            if tok[0] == 'STRING_VALUE' and self.allowmultistrings:
                while self.tok[0] == 'STRING_VALUE':
                    val += self.tok[1]
                    self.NextToken()
            return {'nt':'CONST', 't':lslcommon.PythonType2LSL[type(val)],
                    'value':val}
        if tok[0] == 'IDENT':
            sym = self.FindSymbolPartial(tok[1])
            if sym is None or sym['Kind'] != 'v':
                raise EParseUndefined(self)
            typ = sym['Type']
            if ForbidList and lslcommon.LSO and typ == 'key':
                # This attempts to reproduce LSO's behaviour that a key global
                # var inside a list global definition takes a string value
                # (SCR-295).
                typ = 'string'
            return {'nt':'IDENT', 't':typ, 'name':tok[1], 'scope':sym['Scope']}
        if tok[0] == '<':
            value = [self.Parse_simple_expr()]
            self.autocastcheck(value[0], 'float')
            self.expect(',')
            self.NextToken()
            value.append(self.Parse_simple_expr())
            self.autocastcheck(value[1], 'float')
            self.expect(',')
            self.NextToken()
            value.append(self.Parse_simple_expr())
            self.autocastcheck(value[2], 'float')
            if self.tok[0] == '>':
                self.NextToken()
                return {'nt':'VECTOR', 't':'vector', 'ch':value}
            self.expect(',')
            self.NextToken()
            value.append(self.Parse_simple_expr())
            self.autocastcheck(value[3], 'float')
            self.expect('>')
            self.NextToken()
            return {'nt':'ROTATION', 't':'rotation', 'ch':value}

        if tok[0] == '[' and not ForbidList:
            value = []
            if self.tok[0] == ']':
                self.NextToken()
                return {'nt':'LIST','t':'list','ch':value}
            while True:
                value.append(self.Parse_simple_expr(ForbidList=True))
                if self.tok[0] == ']':
                    self.NextToken()
                    return {'nt':'LIST','t':'list','ch':value}
                self.expect(',')
                self.NextToken()
        # Integer or Float constant expected
        neg = False
        if tok[0] == '-':
            neg = True
            tok = self.tok
            self.NextToken()
        if tok[0] not in ('INTEGER_VALUE', 'FLOAT_VALUE'):
            raise EParseSyntax(self)
        value = tok[1]
        if neg and (tok[0] != 'INTEGER_VALUE' or value != -2147483648):
            value = -value
        return {'nt':'CONST', 't':'float' if tok[0] == 'FLOAT_VALUE' else 'integer', 'value':value}

    def Parse_optional_param_list(self):
        """Grammar parsed here:

        optional_param_list: LAMBDA | param_list
        param_list: TYPE IDENT | param_list ',' TYPE IDENT
        """
        types = []
        names = []

        if self.tok[0] == 'TYPE':
            while True:
                typ = self.tok[1]
                self.NextToken()
                self.expect('IDENT')

                name = self.tok[1]
                if name in self.symtab[self.scopeindex]:
                    raise EParseAlreadyDefined(self)

                types.append(typ)
                names.append(name)

                self.AddSymbol('v', self.scopeindex, name, Type=typ, Param=True)
                self.NextToken()
                if self.tok[0] != ',':
                    break
                self.NextToken()
                self.expect('TYPE')

        return (types, names)

    def Parse_events(self):
        """Grammar parsed here:

        events: event | events event
        event: EVENT_NAME '(' optional_parameter_list ')' code_block
        """
        self.expect('EVENT_NAME') # mandatory

        ret = []

        while self.tok[0] == 'EVENT_NAME':
            name = self.tok[1]
            self.NextToken()
            if name in self.localevents:
                raise EParseAlreadyDefined(self)
            self.localevents.add(name)
            self.expect('(')
            self.NextToken()
            # Function parameters go to a dedicated symbol table.
            self.PushScope()
            params = self.Parse_optional_param_list()
            self.expect(')')
            self.NextToken()
            # NOTE: Parse_events: This is a bit crude, as the error is given at the end of the param list.
            # To do it correctly, we can pass the parameter list to Parse_optional_param_list().
            if tuple(params[0]) != self.events[name]:
                raise EParseSyntax(self)
            self.locallabels = set()
            body = self.Parse_code_block(None)
            del self.locallabels
            ret.append({'nt':'FNDEF', 't':None, 'name':name, # no scope as these are reserved words
                'pscope':self.scopeindex, 'ptypes':params[0], 'pnames':params[1],
                'ch':[body]})
            self.PopScope()

        return ret

    def Parse_globals(self):
        """Grammar parsed here:

        globals: LAMBDA | globals var_def | globals func_def
        var_def: TYPE IDENT ';' | TYPE IDENT '=' simple_expr ';'
        func_def: optional_type IDENT '(' optional_param_list ')' code_block
        optional_type: LAMBDA | TYPE
        """
        assert self.scopeindex == 0
        while self.tok[0] in ('TYPE','IDENT'):
            typ = None
            if self.tok[0] == 'TYPE':
                typ = self.tok[1]
                self.NextToken()
                self.expect('IDENT')

            name = self.tok[1]
            self.NextToken()
            if name in self.symtab[0]:
                # Duplicate identifier. That's an exception unless function
                # override is in effect.
                report = True
                if self.funcoverride:
                    # Is it a function definition, and is the entry in the
                    # symbol table a function definition itself? And is it
                    # a user-defined function?
                    if self.tok[0] == '(' \
                       and self.symtab[0][name]['Kind'] == 'f' \
                       and 'Loc' in self.symtab[0][name]:
                        # Override it.
                        report = False
                        # Delete the previous definition.
                        self.tree[self.symtab[0][name]['Loc']] = \
                            {'nt':'LAMBDA', 't':None}
                        del self.symtab[0][name]
                if report:
                    raise EParseAlreadyDefined(self)

            if self.tok[0] in ('=', ';'):
                # This is a variable definition
                if typ is None: # Typeless variables are not allowed
                    raise EParseSyntax(self)

                if self.tok[0] == '=':
                    self.NextToken()
                    if self.extendedglobalexpr:
                        self.disallowglobalvars = True # Disallow forward globals.
                        # Mark backtracking position
                        pos = self.pos
                        errorpos = self.errorpos
                        tok = self.tok
                        try:
                            value = self.Parse_simple_expr()
                            self.expect(';')
                            value['Simple'] = True # Success - mark it as simple
                        except EParse:
                            # Backtrack
                            self.pos = pos
                            self.errorpos = errorpos
                            self.tok = tok
                            # Use advanced expression evaluation.
                            value = self.Parse_expression()
                            self.expect(';')
                        self.disallowglobalvars = False # Allow forward globals again.
                    else:
                        # Use LSL's dull global expression.
                        value = self.Parse_simple_expr()
                        self.expect(';')
                else: # must be semicolon
                    value = None

                assert self.scopeindex == 0
                decl = {'nt':'DECL', 't':typ, 'name':name, 'scope':0}
                if value is not None:
                    value = self.autocastcheck(value, typ)
                    decl['ch'] = [value]
                self.NextToken()
                self.AddSymbol('v', 0, name, Loc=len(self.tree), Type=typ)
                self.tree.append(decl)

            elif self.tok[0] == '(':
                # This is a function definition
                self.NextToken()
                self.PushScope()
                params = self.Parse_optional_param_list()
                self.expect(')')
                self.NextToken()
                self.localevents = None
                self.locallabels = set()
                body = self.Parse_code_block(typ)
                del self.locallabels
                if typ and 'LIR' not in body: # is LastIsReturn flag set?
                    raise EParseCodePathWithoutRet(self)
                paramscope = self.scopeindex
                self.AddSymbol('f', 0, name, Loc=len(self.tree), Type=typ,
                    ParamTypes=params[0], ParamNames=params[1])
                self.tree.append({'nt':'FNDEF', 't':typ, 'name':name, 'scope':0,
                    'pscope':paramscope,
                    'ptypes':params[0], 'pnames':params[1], 'ch':[body]})
                self.PopScope()
                assert self.scopeindex == 0
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
                name = 'default'
            else:
                self.NextToken()
                if self.tok[0] != 'IDENT':
                    raise EParseSyntax(self)
                name = self.tok[1]

            if name in self.symtab[self.scopeindex]:
                raise EParseAlreadyDefined(self)

            assert self.scopeindex == 0
            self.AddSymbol('s', 0, name, Loc=len(self.tree))
            self.NextToken()

            self.expect('{')
            self.NextToken()

            self.localevents = set()
            events = self.Parse_events()
            del self.localevents

            self.expect('}')
            self.tree.append({'nt':'STDEF', 't':None, 'name':name, 'scope':0, 'ch':events})
            self.NextToken()

    def Parse_script(self):
        """Parses the whole LSL script

        Grammar parsed here:

        script: globals states EOF
        """

        # We need a table of undefined jump references, to check later,
        # as jumps are local, not global, and allow forward definitions.
        # This avoids making one more pass, or making the first pass more
        # detailed unnecessarily.
        self.jump_lookups = []

        self.globalmode = True
        self.Parse_globals()
        self.globalmode = False
        self.Parse_states()
        self.expect('EOF')

        # Check the pending jump targets
        for tgt in self.jump_lookups:
            self.scopeindex = tgt[1]
            sym = self.FindSymbolPartial(tgt[0], MustBeLabel = True)
            if sym is None:
                self.errorpos = tgt[2]
                raise EParseUndefined(self)
            tgt[3]['scope'] = sym['Scope']

        del self.jump_lookups # Finished with it.

    def Parse_single_expression(self):
        """Parse the script as an expression, Used by lslcalc.

        Grammar parsed here:

        script: expression EOF
        """
        value = self.Parse_expression()
        self.tree.append({'nt':'EXPR', 't':value['t'], 'ch':[value]})
        self.expect('EOF')
        return

    def BuildTempGlobalsTable(self):
        """Build an approximate globals table.

        If the script syntax is correct, the globals table will be accurate.
        If it is not, it may contain too many or too few symbols (normally the
        latter). This globals table is not the normal globals in the symbol
        table; it's just needed to resolve forward references. It's temporary.

        The grammar is approximately:
        script: globals states
        globals: [global [global [...]]]
        global: [TYPE] IDENT '(' TYPE anytoken [',' TYPE anytoken [...]]
                anytoken_except_comma balanced_braces_or_anything_else
            | TYPE IDENT [anytoken_except_semicolon [...]] ';'
        states: state [state [...]]
        state: (DEFAULT | STATE IDENT) balanced_braces_or_anything_else
        """
        ret = self.funclibrary.copy() # The library functions go here too.

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
                    typ = self.tok[1]
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
                            params.append(self.tok[1])
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
                    ret[name] = {'Kind':'f','Type':typ,'ParamTypes':params}

                elif typ is None:
                    return ret # A variable needs a type
                else:
                    # No location info but none is necessary for forward
                    # declarations.
                    ret[name] = {'Kind':'v','Type':typ,'Scope':0}
                    while self.tok[0] != ';': # Don't stop to analyze what's before the ending ';'
                        if self.tok[0] == 'EOF':
                            return ret
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
                name = 'default'

            # No location info but none is necessary for forward declarations.
            ret[name] = {'Kind':'s'}
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


    def parse(self, script, options = ()):
        """Parse the given stream with the given options.

        This function also builds the temporary globals table.
        """

        if type(script) is unicode:
            script = script.encode('utf8')

        self.script = script
        self.length = len(script)

        self.keywords = self.base_keywords

        self.labelcnt = 0

        # Options

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

        # Process preprocessor directives (especially #pragma and #line).
        self.processpre = 'processpre' in options

        # TODO: Allow pure C-style string escapes. This is low-priority.
        #self.allowcescapes = 'allowcescapes' in options

        # Enable switch statements.
        self.enableswitch = 'enableswitch' in options
        if self.enableswitch:
            self.keywords |= self.switch_keywords

        # Broken behaviour in the absence of a default: label in a switch stmt.
        self.errmissingdefault = 'errmissingdefault' in options

        # Allow brackets for assignment of list elements e.g. mylist[5]=4
        self.lazylists = 'lazylists' in options

        # This was once an idea, but it has been discarded because
        # llListReplaceList requires the argument to be evaluated twice,
        # so the function is unavoidable. Consider e.g. L[x++] = 3 expanded to
        # L = llListReplaceList(L, [3], x++, x++).
        # # Extend the list with integer zeros when lazylists is active and the
        # # index is greater than the end of the list.
        # self.lazylistcompat = 'lazylistcompat' in options

        # Enable break/continue
        self.breakcont = 'breakcont' in options
        if self.breakcont:
            self.keywords |= self.brkcont_keywords

        # Stack to track the labels for break targets, their scope table index,
        # and whether they are used.
        self.breakstack = []
        # Stack to track the labels for continue targets, their scope index,
        # and whether they are used.
        self.continuestack = []

        # Enable use of local labels with duplicate names
        self.duplabels = 'duplabels' in options

        # Shrink names. Activates duplabels automatically.
        self.shrinknames = 'shrinknames' in options

        # Allow a duplicate function definition to override the former,
        # rather than reporting a duplicate identifier error.
        self.funcoverride = 'funcoverride' in options

        # This was an idea, but functions must return a type, and making the
        # type suitable for the context is too much work.
        # # Allow referencing undefined functions inside function definitions.
        # self.allowundeffn = 'allowundeffn' in options

        # Symbol table:
        # This is a list of all local and global symbol tables.
        # The first element (0) is the global scope. Each symbol table is a
        # dictionary. Element -1 of the dictionary is the parent index. The
        # rest of entries are dictionaries. Each has a 'Kind', which can be
        # 'v' for variable, 'f' for function, 'l' for label, 's' for state,
        # or 'e' for event. Some have a 'Loc' indicating the location (index)
        # of the definition in the tree root.
        #   Variables have 'Scope' and 'Type' (a string).
        #     Global variables also have 'Loc'.
        #   Functions have 'Type' (return type, a string) and 'ParamTypes' (a list of strings).
        #     User-defined functions also have 'Loc' and 'ParamNames' (a list of strings).
        #   Labels only have 'Scope'.
        #   States only have 'Loc'.
        #   Events have 'ParamTypes' and 'ParamNames', just like UDFs.
        # Other modules may add information if they need.

        # Incorporate the library into the initial symbol table.
        self.symtab = [self.funclibrary.copy()]
        self.symtab[0][-1] = None
        self.scopeindex = 0

        # This is a small hack to prevent circular definitions in globals when
        # extended expressions are enabled. When false (default), forward
        # globals are allowed; if true, only already seen globals are permitted.
        self.disallowglobalvars = False

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

        self.scanglobals = True # Tell the lexer not to process directives
        self.pos = self.errorpos = 0
        self.linestart = True
        self.tok = self.GetToken()

        self.globals = self.BuildTempGlobalsTable() if not lslcommon.IsCalc \
          else self.funclibrary.copy()

        # Restart

        self.scanglobals = False
        self.pos = self.errorpos = 0
        self.linestart = True
        self.tok = self.GetToken()

        # Reserve spots at the beginning for functions we add
        self.tree = [{'nt':'LAMBDA','t':None}]
        self.usedspots = 0

        # Start the parsing proper
        if lslcommon.IsCalc:
            self.Parse_single_expression()
        else:
            self.Parse_script()

        # No longer needed. The data is already in self.symtab[0].
        del self.globals

        treesymtab = self.tree, self.symtab
        del self.tree
        del self.symtab

        return treesymtab

    def parsefile(self, filename, options = set()):
        """Convenience function to parse a file"""
        f = open(filename, 'r')
        try:
            script = f.read()
        finally:
            f.close()

        return self.parse(script, options)

    def __init__(self, builtins = None, seftable = None):
        """Reads the library."""

        self.events = {}
        self.constants = {}
        self.funclibrary = {}

        if builtins is None:
            builtins = lslcommon.DataPath + 'builtins.txt'

        if seftable is None:
            seftable = lslcommon.DataPath + 'seftable.txt'

        self.parse_directive_re = None

        # Library read code

        parse_lin_re = re.compile(
            r'^\s*([a-z]+)\s+'
            r'([a-zA-Z_][a-zA-Z0-9_]*)\s*\(\s*('
                r'[a-z]+\s+[a-zA-Z_][a-zA-Z0-9_]*'
                r'(?:\s*,\s*[a-z]+\s+[a-zA-Z_][a-zA-Z0-9_]*)*'
            r')?\s*\)\s*$'
            r'|'
            r'^\s*const\s+([a-z]+)'
            r'\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*=\s*(.*?)\s*$'
            r'|'
            r'^\s*(?:#.*|//.*)?$')
        parse_arg_re = re.compile(r'^\s*([a-z]+)\s+[a-zA-Z_][a-zA-Z0-9_]*\s*$')
        parse_num_re = re.compile(r'^\s*(-?(?=[0-9]|\.[0-9])[0-9]*((?:\.[0-9]*)?(?:[Ee][+-]?[0-9]+)?))\s*$')
        parse_str_re = re.compile(ur'^"((?:[^"\\]|\\.)*)"$')

        f = open(builtins, 'rb')
        try:
            linenum = 0
            try:
                ubuiltins = builtins.decode('utf8')
            except UnicodeDecodeError:
                ubuiltins = builtins.decode('iso-8859-15')
            while True:
                linenum += 1
                line = f.readline()
                if not line: break
                if line[-1] == '\n': line = line[:-1]
                try:
                    uline = line.decode('utf8')
                except UnicodeDecodeError:
                    warning(u"Bad Unicode in %s line %d" % (ubuiltins, linenum))
                    continue
                match = parse_lin_re.search(line)
                if not match:
                    warning(u"Syntax error in %s, line %d" % (ubuiltins, linenum))
                    continue
                if match.group(1):
                    # event or function
                    typ = match.group(1)
                    if typ == 'quaternion':
                        typ = 'rotation'
                    if typ == 'void':
                        typ = None
                    elif typ != 'event' and typ not in self.types:
                        warning(u"Invalid type in %s, line %d: %s" % (ubuiltins, linenum, typ))
                        continue
                    args = []
                    arglist = match.group(3)
                    if arglist:
                        arglist = arglist.split(',')
                        bad = False
                        for arg in arglist:
                            argtyp = parse_arg_re.search(arg).group(1)
                            if argtyp not in self.types:
                                uargtyp = argtyp.decode('utf8')
                                warning(u"Invalid type in %s, line %d: %s" % (ubuiltins, linenum, uargtyp))
                                del uargtyp
                                bad = True
                                break
                            args.append(argtyp)
                        if bad:
                            continue
                    name = match.group(2)
                    if typ == 'event':
                        if name in self.events:
                            uname = name.decode('utf8')
                            warning(u"Event at line %d was already defined in %s, overwriting: %s" % (linenum, ubuiltins, uname))
                            del uname
                        self.events[name] = tuple(args)
                    else:
                        # Library functions go to the functions table. If
                        # they are implemented in lslfuncs.*, they get a
                        # reference to the implementation; otherwise None.
                        if name in self.funclibrary:
                            uname = name.decode('utf8')
                            warning(u"Function at line %d was already defined in %s, overwriting: %s" % (linenum, ubuiltins, uname))
                            del uname
                        fn = getattr(lslfuncs, name, None)
                        self.funclibrary[name] = {'Kind':'f', 'Type':typ, 'ParamTypes':args}
                        if fn is not None:
                            self.funclibrary[name]['Fn'] = fn
                elif match.group(4):
                    # constant
                    name = match.group(5)
                    if name in self.constants:
                        uname = name.decode('utf8')
                        warning(u"Global at line %d was already defined in %s, overwriting: %s" % (linenum, ubuiltins, uname))
                        del uname
                    typ = match.group(4)
                    if typ not in self.types:
                        utyp = typ.decode('utf8')
                        warning(u"Invalid type in %s, line %d: %s" % (ubuiltins, linenum, utyp))
                        del utyp
                        continue
                    if typ == 'quaternion':
                        typ = 'rotation'
                    value = match.group(6)
                    if typ == 'integer':
                        value = int(value, 0)
                    elif typ == 'float':
                        value = lslfuncs.F32(float(value))
                    elif typ == 'string':
                        value = value.decode('utf8')
                        if parse_str_re.search(value):
                            esc = False
                            tmp = value[1:-1]
                            value = u''
                            for c in tmp:
                                if esc:
                                    if c == u'n':
                                        c = u'\n'
                                    elif c == u't':
                                        c = u'    '
                                    value += c
                                    esc = False
                                elif c == u'\\':
                                    esc = True
                                else:
                                    value += c
                            #if typ == 'key':
                            #    value = Key(value)
                        else:
                            warning(u"Invalid string in %s line %d: %s" % (ubuiltins, linenum, uline))
                            value = None
                    elif typ == 'key':
                        warning(u"Key constants not supported in %s, line %d: %s" % (ubuiltins, linenum, uline))
                        value = None
                    elif typ in ('vector', 'rotation'):
                        try:
                            if value[0:1] != '<' or value[-1:] != '>':
                                raise ValueError
                            value = value[1:-1].split(',')
                            if len(value) != (3 if typ == 'vector' else 4):
                                raise ValueError
                            num = parse_num_re.search(value[0])
                            if not num:
                                raise ValueError
                            value[0] = lslfuncs.F32(float(num.group(1)))
                            num = parse_num_re.search(value[1])
                            if not num:
                                raise ValueError
                            value[1] = lslfuncs.F32(float(num.group(1)))
                            num = parse_num_re.search(value[2])
                            if not num:
                                raise ValueError
                            value[2] = lslfuncs.F32(float(num.group(1)))
                            if typ == 'vector':
                                value = Vector(value)
                            else:
                                num = parse_num_re.search(value[3])
                                if not num:
                                    raise ValueError
                                value[3] = lslfuncs.F32(float(num.group(1)))
                                value = Quaternion(value)
                        except ValueError:
                            warning(u"Invalid vector/rotation syntax in %s line %d: %s" % (ubuiltins, linenum, uline))
                    else:
                        assert typ == 'list'
                        if value[0:1] != '[' or value[-1:] != ']':
                            warning(u"Invalid list value in %s, line %d: %s" % (ubuiltins, linenum, uline))
                        elif value[1:-1].strip() != '':
                            warning(u"Non-empty list constants not supported in %s, line %d: %s" % (ubuiltins, linenum, uline))
                            value = None
                        else:
                            value = []
                    if value is not None:
                        self.constants[name] = value

        finally:
            f.close()

        # Load the side-effect-free table as well.
        # TODO: Transform the SEF Table into a function properties table
        #       that includes domain data (min, max) and possibly input
        #       parameter transformations e.g.
        #           llSensor(..., PI, ...) -> llSensor(..., 4, ...).
        f = open(seftable, 'rb')
        try:
            while True:
                line = f.readline()
                if line == '':
                    break
                line = line.strip()
                if line and line[0] != '#' and line in self.funclibrary:
                    self.funclibrary[line]['SEF'] = True
        finally:
            f.close()
