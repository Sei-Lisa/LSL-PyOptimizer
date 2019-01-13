#    (C) Copyright 2015-2019 Sei Lisa. All rights reserved.
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
#
#    This file includes an excerpt from PCPP, by Niall Douglas and David
#    Beazley. PCPP is available here: https://github.com/ned14/pcpp and
#    distributed under the following conditions:
#
#    (C) Copyright 2018-2019 Niall Douglas http://www.nedproductions.biz/
#    (C) Copyright 2007-2019 David Beazley http://www.dabeaz.com/
#
#    All rights reserved.
#
#    Redistribution and use in source and binary forms, with or without
#    modification, are permitted provided that the following conditions are
#    met:
#
#  * Redistributions of source code must retain the above copyright notice,
#    this list of conditions and the following disclaimer.
#  * Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.
#  * Neither the name of the David Beazley or Dabeaz LLC may be used to
#    endorse or promote products derived from this software without
#    specific prior written permission.
#
#    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
#    OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
#    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
#    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
#    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
#    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
#    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
#    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
#    (End of terms and conditions for the PCPP excerpt)
#
#    The particular excerpt used is this one:
# https://github.com/ned14/pcpp/blob/e1219ce157b4dfcfee3181faa6ec5129c3a41e78/pcpp/preprocessor.py#L873-L935
#
#    The following fragments of code are hereby irrevokably donated to the
#    public domain:
#    - The Evaluator class in its entirety.
#    - The evalexpr method in its entirety except for the excerpt mentioned
#      above, which remains copyright of its authors.
#    - Every line between this one and the Evaluator class.

import sys, os, re, copy

oldsyspath = sys.path
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                'pcpp'))
from pcpp import preprocessor
path = oldsyspath

# Define the number of bits to work with in expression evaluation
# (per the standard, this should be the bits in uintmax_t).
INTMAXBITS = 64

UINTMAX_MAX = (1 << INTMAXBITS) - 1
INTMAX_MIN = -(1 << (INTMAXBITS - 1))

DSYMBOLS = {'->', '-=', '--', '==', '<<', '<=', '>>', '>=', '||', '|=',
            '&&', '&=', '!=', '^=', '*=', '/=', '%=', '+=', '++'}
DIGRAPHS = {'<:':'[', ':>':']', '<%':'{', '%>':'}', '%:':'#'}
ESCAPES = {'a':7,'b':8,'f':12,'n':10,'r':13,'t':9,'v':11,
           '"':34, '\\':92, '\'':39, '?':63}

# Exception to report an evaluation error
class EvalError(Exception): pass

class uint(long): pass
class sint(long): pass

class Evaluator(object):
    """Recursive descendent parser to evaluate C preprocessor expressions."""

    # Int parser
    resolve_int_regex = re.compile(
        # Group 1: Hex
        # Group 2: Oct
        # Group 3: Dec
        # Group 4: Unsigned
        r'^(?:(0x[0-9a-f]+)|(0[0-7]*)|([1-9][0-9]*))'
        r'(?:(u(?:ll?)?|(?:ll?)?u)|(?:ll?)?)$', re.I | re.S)

    # Char parser (without the quotes)
    ctoken_regex = re.compile(
        r'\\(?:'
            r'[\?' r"'" r'"\\abfnrtv]|[Xx][0-9a-fA-F]+|[0-7]{1,3}'
            r'|u[0-9a-fA-F]{4}|U[0-9a-fA-F]{8}'
        r')'
        r'|.', re.S)

    def __init__(self, tokens):
        assert tokens, "Empty tokens list???"
        self.tokens = tokens
        self.ptr = 0
        self.evaluating = True
        self.conv = {uint: self.to_uint, sint: self.to_sint}
        self.nextToken()

    def to_uint(self, i):
        return uint(i & UINTMAX_MAX)

    def to_sint(self, i):
        return sint(((i - INTMAX_MIN) & UINTMAX_MAX) + INTMAX_MIN)

    def nextToken(self):
        """Sets self.token to the next token and advances the token pointer.
        Skips whitespace tokens. Returns a CPP_WS token with value '\n' if
        there's no next token. Returns synthesized tokens for multichar tokens
        not currently handled by PCPP.
        """
        try:
            while True:
                tok = self.token = self.tokens[self.ptr]
                self.ptr += 1
                # Eat whitespace except newlines, and /* */ comments
                if (tok.type == 'CPP_WS' and '\n' not in tok.value
                    or tok.type == 'CPP_COMMENT1'
                   ):
                    continue
                break

        except IndexError:
            # Synthesize a new CPP_WS token with a newline, to signal
            # end-of-text (we copy it from the last one in the token stream).
            tok = self.token = copy.copy(self.tokens[-1])
            tok.type = 'CPP_WS'
            tok.value = '\n'
            return

        # Single-line comments are line terminators; convert them
        if tok.type == 'CPP_COMMENT2':
            tok = self.token = copy.copy(tok)
            tok.type = 'CPP_WS'
            tok.value = '\n'
            return

        # Work around a lexing problem in PCPP
        #
        # PCPP doesn't tokenize multichar tokens except ##, so we do that job
        # here, to ease processing and report more errors (e.g. 5--3 should be
        # reported as an error because it uses the post-decrement operator,
        # instead of evaluating to 8, which is the correct result for 5- -3).
        # The tokens processed here are those in the C standard missed by PCPP:
        #   -> -= -- << <= >> >= || |= && &= == != ^= *= /= += ++ %=
        #   >>= <<=
        #   ...
        #   <: :> <% %> %:
        #   %:%:
        #
        # This is already a single token, therefore it's not processed here:
        #   ##

        try:
            next = self.tokens[self.ptr]
        except IndexError:
            return

        s = tok.type + next.type

        if s in DSYMBOLS:
            tok = self.token = copy.copy(tok)
            tok.type = s
            tok.value += next.value
            self.ptr += 1
            if s in ('<<', '>>'):
                # check for <<= >>=
                try:
                    next2 = self.tokens[self.ptr]
                    if next2.type == '=':
                        tok.type += next2.type
                        tok.value += next2.value
                        self.ptr += 1
                except IndexError:
                    pass
            return

        if s in DIGRAPHS:
            # digraph or DPOUND
            tok = self.token = copy.copy(tok)
            tok.type = DIGRAPHS[s]
            tok.value += next.value
            self.ptr += 1
            try:
                next2 = self.tokens[self.ptr]
                next3 = self.tokens[self.ptr + 1]
                if next2.type == '%' and next3.type == ':':
                    tok.type = '##'
                    tok.value += next2.value + next3.value
                    self.ptr += 2
            except IndexError:
                pass
            return

        if s == '..':
            try:
                next2 = self.tokens[self.ptr + 1]
                if next2.type == '.':
                    tok = self.token = copy.copy(tok)
                    tok.type = '...'
                    tok.value += next.value + next2.value
                    self.ptr += 2
            except IndexError:
                pass
            return

    def eat(self, *toktypes):
        """Return True and advance pointer if the current token matches. """
        if self.token.type in toktypes:
            self.nextToken()
            return True
        return False

    def expect(self, toktype):
        """Checks an expected token and eats it"""
        expect = toktype
        if toktype == 'END' and '\n' in self.token.value:
            expect = 'CPP_WS'
        if not self.eat(expect):
            raise EvalError(
                "Unexpected token %s (%s) in expression, expected %s"
                % (repr(self.token.value), self.token.type, toktype))

    def conversions(self, op1, op2):
        """Perform usual arithmetic conversions on two operands."""
        assert type(op1) in (sint, uint) and type(op2) in (sint, uint)
        if type(op1) != type(op2):
            return uint(op1), uint(op2)
        return op1, op2

    def primary_expression(self, evaluating):
        """Non-terminal: primary_expression.

        primary_expression:
            IDENTIFIER | STRING_LITERAL | CHAR_LITERAL | INTEGER
            | '(' expression ')'
        """
        tok = self.token
        if self.eat('('):
            ret = self.expression(evaluating)
            self.expect(')')
            return ret

        #if self.eat('CPP_STRING'):
        #    return tok.value

        if self.eat('CPP_CHAR'):
            charstr = tok.value
            unicode = False
            if tok.value.startswith('L'):
                unicode = True
                charstr = charstr[2:-1]
            else:
                charstr = charstr[1:-1]
            onechar = False
            for ctok in self.ctoken_regex.finditer(charstr):
                if onechar:
                    raise EvalError("Multiple characters in char literal")
                onechar = True
                c = ctok.group(0)
                if c == '\\':
                    raise EvalError("Invalid escape sequence in char literal")
                if c.startswith('\\'):
                    if c.startswith('\\u') or c.startswith('\\U'):
                        result = int(c[2:], 16)
                        if ((result < 0xA0 and result not in (0x24,0x40,0x60))
                            or 0xD800 <= result <= 0xDFFF
                           ):
                            raise EvalError("Invalid universal character %s"
                                % c)
                        if result > 0xFF and not unicode:
                            raise EvalError("Char literal out of range")
                    elif c.startswith('\\x') or c.startswith('\\X'):
                        result = int(c[2:], 16)
                        if result > 0xFF:
                            raise EvalError("Hex literal out of range")
                    elif c[1] in ESCAPES:
                        result = ESCAPES[c[1]]
                    else:
                        result = int(c[1:], 8)
                else:
                    assert len(c) == 1 and c != '\''
                    return ord(c)

            # This may need reconsideration if INTMAXBITS is < 22 (the bits
            # necessary to fit a Unicode codepoint in a signed integer).
            return sint(result)  # our char is unsigned

        if tok.type == 'CPP_ID':
            tok = self.token = copy.copy(tok)
            tok.type = 'CPP_INTEGER'
            tok.value = '0'
            # fall through to process it as CPP_INTEGER

        if self.eat('CPP_INTEGER'):
            m = self.resolve_int_regex.search(tok.value)
            if not m:
                raise EvalError("Invalid integer literal")
            val = (int(m.group(2), 8) if m.group(2)
                   else int(m.group(1) or m.group(3), 0))
            val = self.to_uint(val) if m.group(4) else self.to_sint(val)
            return val

        if tok.type == 'CPP_STRING':
            raise EvalError("Strings are not allowed in expressions")

        if tok.type == 'CPP_WS' and '\n' in tok.value:
            raise EvalError('Unexpected end of expression')

        self.expect('CPP_INTEGER')

    def factor_expression(self, evaluating):
        """Non-terminal: factor_expression

        factor_expression:
            primary_expression
            | unary_operator factor_expression
        """
        # Avoid recursing for unary operators. Apply them post-evaluation.
        k = None
        while True:
            toktype = self.token.type
            if self.eat('-', '+', '~', '!') and toktype != '+':
                k = k or []
                k.append(toktype)
            else:
                break
        result = self.primary_expression(evaluating)
        while k:
            operation = k.pop()
            if operation == '!':
                result = sint(0 if result else 1)
            else:
                result = self.conv[type(result)](-result if operation == '-'
                else ~result)
        return result

    def term_expression(self, evaluating):
        """Non-terminal: term_expression

        term_expression:
            factor_expression
            | term_expression '*' factor_expression
            | term_expression '/' factor_expression
            | term_expression '%' factor_expression
        """
        result = self.factor_expression(evaluating)
        while True:
            toktype = self.token.type
            if not self.eat('*', '/', '%'):
                return result
            operand = self.factor_expression(evaluating)
            if evaluating and operand == 0 and toktype != '*':
                raise EvalError("Division by zero")
            result, operand = self.conversions(result, operand)
            result = self.conv[type(result)](result if not evaluating
                else result * operand if toktype == '*'
                else result // operand if toktype == '/'
                else result % operand)

    def arithmetic_expression(self, evaluating):
        """Non-terminal: arithmetic_expression

        arithmetic_expression:
            term_expression
            | arithmetic_expression '+' term_expression
            | arithmetic_expression '-' term_expression
        """
        result = self.term_expression(evaluating)
        while True:
            toktype = self.token.type
            if not self.eat('+', '-'):
                return result
            operand = self.term_expression(evaluating)
            result, operand = self.conversions(result, operand)
            result = self.conv[type(result)](result + operand if toktype == '+'
                else result - operand)

    def shift_expression(self, evaluating):
        """Non-terminal: shift_expression

        shift_expression:
            arithmetic_expression
            | shift_expression '<<' arithmetic_expression
            | shift_expression '>>' arithmetic_expression
        """
        result = self.arithmetic_expression(evaluating)
        while True:
            tok = self.token
            if not self.eat('<<', '>>'):
                return result
            operand = self.arithmetic_expression(evaluating)
            # We don't want a too large intermediate result, to prevent DoS
            result = self.conv[type(result)](result << min(operand, INTMAXBITS)
                if tok.type == '<<' else result >> max(operand, 0))

    def relational_expression(self, evaluating):
        """Non-terminal: relational_expression

        relational_expression:
            shift_expression
            | relational_expression '>' shift_expression
            | relational_expression '<' shift_expression
            | relational_expression '>=' shift_expression
            | relational_expression '<=' shift_expression
        """
        result = self.shift_expression(evaluating)
        while True:
            tok = self.token
            if not self.eat('<', '>', '<=', '>='):
                return result
            operand = self.shift_expression(evaluating)
            result, operand = self.conversions(result, operand)
            # Use the fact that a < b  <->  b > a
            # Use the fact that a < b  <-> !(a >= b)
            if tok.type == '>' or tok.type == '<=':
                result, operand = operand, result
            result = sint(1 if (result < operand) == (tok.type in ('<', '>'))
                          else 0)

    def equality_expression(self, evaluating):
        """Non-terminal: equality_expression

        equality_expression:
            relational_expression
            | equality_expression '==' relational_expression
            | equality_expression '!=' relational_expression
        """
        result = self.relational_expression(evaluating)
        while True:
            tok = self.token
            if not self.eat('==', '!='):
                return result
            operand = self.relational_expression(evaluating)
            result, operand = self.conversions(result, operand)
            result = sint(1 if (result == operand) == (tok.type == '==')
                          else 0)

    def bitwise_and_expression(self, evaluating):
        """Non-terminal: bitwise_and_expression

        bitwise_and_expression:
            equality_expression
            | bitwise_and_expression '&' equality_expression
        """
        result = self.equality_expression(evaluating)
        while True:
            if not self.eat('&'):
                return result
            operand = self.equality_expression(evaluating)
            result, operand = self.conversions(result, operand)
            result = self.conv[type(result)](result & operand)

    def bitwise_xor_expression(self, evaluating):
        """Non-terminal: bitwise_xor_expression

        bitwise_xor_expression:
            bitwise_and_expression
            | bitwise_xor_expression '^' bitwise_and_expression
        """
        result = self.bitwise_and_expression(evaluating)
        while True:
            if not self.eat('^'):
                return result
            operand = self.bitwise_and_expression(evaluating)
            result, operand = self.conversions(result, operand)
            result = self.conv[type(result)](result ^ operand)

    def bitwise_or_expression(self, evaluating):
        """Non-terminal: bitwise_or_expression

        bitwise_or_expression:
            bitwise_xor_expression
            | bitwise_or_expression '|' bitwise_xor_expression
        """
        result = self.bitwise_xor_expression(evaluating)
        while True:
            if not self.eat('|'):
                return result
            operand = self.bitwise_xor_expression(evaluating)
            result, operand = self.conversions(result, operand)
            result = self.conv[type(result)](result | operand)

    def logical_and_expression(self, evaluating):
        """Non-terminal: logical_and_expression

        logical_and_expression:
            bitwise_or_expression
            | logical_and_expression '&&' bitwise_or_expression
        """
        result = self.bitwise_or_expression(evaluating)
        while True:
            if not self.eat('&&'):
                return result
            evaluating = evaluating and not not result
            operand = self.bitwise_or_expression(evaluating)
            result = sint(1 if result and (not evaluating or operand) else 0)

    def logical_or_expression(self, evaluating):
        """Non-terminal: logical_or_expression

        logical_or_expression:
            logical_and_expression
            | logical_or_expression '||' logical_and_expression
        """
        result = self.logical_and_expression(evaluating)
        while True:
            if not self.eat('||'):
                return result
            evaluating = evaluating and not result
            operand = self.logical_and_expression(evaluating)
            result = sint(1 if result or (evaluating and operand) else 0)

    def conditional_expression(self, evaluating):
        """Non-terminal: conditional_expression.

        conditional_expression:
            logical_or_expression
            | logical_or_expression '?' expression ':' conditional_expression
        """
        result = self.logical_or_expression(evaluating)
        if self.eat('?'):
            if result:
                result = self.expression(evaluating)
                self.expect(':')
                operand = self.conditional_expression(False)
            else:
                operand = self.expression(False)
                self.expect(':')
                result = self.conditional_expression(evaluating)
            result, operand = self.conversions(result, operand)
        return result

    def expression(self, evaluating = True):
        """Non-terminal: expression.

        expression:
            conditional_expression                (always)
            | expression conditional_expression   (if not evaluating)
        """
        if evaluating:
            return self.conditional_expression(evaluating)
        while True:
            result = self.conditional_expression(evaluating)
            if not self.eat(','):
                return result

    def evaluate(self):
        result = self.expression(True)

        # Did we eat all tokens?
        self.expect('END')
        return result

class Preproc(preprocessor.Preprocessor):
    def __init__(self, input, defines=(), sysincpaths=(), incpaths=()):
        super(Preproc, self).__init__()
        self.auto_pragma_once_enabled = False
        for define in defines:
            self.define('%s %s' % define)

        for v in sysincpaths:
            self.add_path(v)
        for v in incpaths:
            self.add_path(v)

        self.ignore = set()
        self.parser = self.parsegen(input, '<stdin>', '<stdin>')

    def get(self):
        try:
            import StringIO
        except ImportError:
            import io as StringIO
        ret = StringIO.StringIO()
        self.write(ret)
        return (ret.getvalue(), self.macros)

    def on_include_not_found(self, is_system_include, curdir, includepath):
        """Don't pass through the #include line if the file does not exist"""
        self.on_error(self.lastdirective.source, self.lastdirective.lineno,
            "Include file not found: %s" % includepath)

    def evalexpr(self, tokens):
        """Evaluate a sequence of tokens as an expression.

        The original uses eval(), which is unsafe for web usage. This one uses
        our own recursive-descendent parser.
        """

        # ****************************************************
        # Start of fragment copied from PCPP's preprocessor.py
        """Evaluate an expression token sequence for the purposes of evaluating
        integral expressions."""
        if not tokens:
            self.on_error('unknown', 0, "Empty expression")
            return (0, None)
        # tokens = tokenize(line)
        # Search for defined macros
        evalfuncts = {'defined' : lambda x: True}
        evalvars = {}
        def replace_defined(tokens):
            i = 0
            while i < len(tokens):
                if tokens[i].type == self.t_ID and tokens[i].value == 'defined':
                    j = i + 1
                    needparen = False
                    result = "0L"
                    while j < len(tokens):
                        if tokens[j].type in self.t_WS:
                            j += 1
                            continue
                        elif tokens[j].type == self.t_ID:
                            if tokens[j].value in self.macros:
                                result = "1L"
                            else:
                                repl = self.on_unknown_macro_in_defined_expr(tokens[j])
                                if repl is None:
                                    # Add this identifier to a dictionary of variables
                                    evalvars[tokens[j].value] = 0
                                    result = 'defined('+tokens[j].value+')'
                                else:
                                    result = "1L" if repl else "0L"
                            if not needparen: break
                        elif tokens[j].value == '(':
                            needparen = True
                        elif tokens[j].value == ')':
                            break
                        else:
                            self.on_error(tokens[i].source,tokens[i].lineno,"Malformed defined()")
                        j += 1
                    if result.startswith('defined'):
                        tokens[i].type = self.t_ID
                        tokens[i].value = result
                    else:
                        tokens[i].type = self.t_INTEGER
                        tokens[i].value = self.t_INTEGER_TYPE(result)
                    del tokens[i+1:j+1]
                i += 1
            return tokens
        # Replace any defined(macro) before macro expansion
        tokens = replace_defined(tokens)
        tokens = self.expand_macros(tokens)
        # Replace any defined(macro) after macro expansion
        tokens = replace_defined(tokens)
        if not tokens:
            return (0, None)
        for i,t in enumerate(tokens):
            if t.type == self.t_ID:
                repl = self.on_unknown_macro_in_expr(copy.copy(t))
                if repl is None:
                    # Add this identifier to a dictionary of variables
                    evalvars[t.value] = 0
                else:
                    tokens[i] = t = repl
        # End of fragment copied from PCPP's preprocessor.py
        # **************************************************

        del evalfuncts  # we don't use this

        evaluator = Evaluator(tokens)
        try:
            result = int(evaluator.evaluate())
        except EvalError as e:
            self.on_error(evaluator.token.source, evaluator.token.lineno,
                e.message)
            return (0, None)
        del evaluator

        return (result, tokens) if evalvars else (result, None)
