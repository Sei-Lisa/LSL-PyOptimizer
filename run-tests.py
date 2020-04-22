#!/usr/bin/env python2
#
#    (C) Copyright 2015-2020 Sei Lisa. All rights reserved.
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

# Unit testing program.
# Checks all files in unit_tests/*.suite/ with extensions .lsl or .run.
# When one is found, it's considered a test (if both exist, they are considered
# a single test).
#
# Extension .lsl is for source files to test. If the first line starts with
#    "// " then the rest of the line is taken as the docstring of that test
#    (visible with Eric or with option -v). The script is also fed as standard
#    input to the program.
# .run defines the command-line parameters for invocation. A test can be run
#    without a .lsl file but with a .run file. If not present, the .lsl file
#    is run with the command line 'main.py -'. The quoting rules are sh-style.
#    The executable name is ignored, but needs to be present.
# .out is for expected output to stdout. If the first line is "REGEX", then
#    the rest of the file is interpreted as a regular expression that the
#    output is matched against. Otherwise the output must exactly match.
#    If the file is not present, that's equivalent to an empty file, i.e. no
#    output is expected.
# .err is like .out but for expected output to stderr, with the same features.
# .skp is for a file that if present, will skip this test. The contents are
#    displayed as the reason for being skipped.
# .fail is for a file that, when present, marks the test as expected to fail.
#    Its contents go to the docstring if not empty, replacing the .lsl one.
#
# A test passes when the stdout output matches the .out file, and the stderr
# output matches the .err file. Both default to empty strings.
#

import unittest
import sys
import os
#import math
import main
import glob
import re
try:
    import difflib
except ImportError:
    difflib = None
if sys.version_info.major < 3:
    from StringIO import StringIO as StringStream
else:
    from io import BytesIO as StringStream
from lslopt import lslcommon,lslfuncs,lslparse,lsloutput,lslloadlib
from lslopt.lslcommon import nr
from strutil import *

class EArgError(Exception):
    pass

def parseArgs(s):
    """Parse a command line, Bourne shell-style"""
    if s is None:
        return None

    args = []

    # States
    Space = 0           # Space between args.
    SBackslash = 1      # Backslash after space. Returns to Space if followed
                        # by LF, otherwise inserts the character verbatim and
                        # goes to Normal.
    Normal = 2          # Normal argument.
    NBackslash = 3      # Backslash in Normal mode. Returns to Normal if
                        # followed by LF, otherwise inserts the character
                        # verbatim.
    DQuote = 4          # Double quote mode.
    DQBackslash = 5     # Backslash in double quote mode. Returns to DQuote if
                        # followed by LF; inserts the character verbatim if
                        # followed by '"', '`', '$' or '\' and inserts a '\'
                        # plus the character verbatim in any other case.
    SQuote = 6          # Single quote mode.

    State = Space
    p = 0
    Len = len(s)
    arg = b''

    while p < Len:
        c = s[p:p+1]
        p += 1
        if State in (Space, Normal):
            if c == b'\\':
                State = NBackslash if State == Normal else SBackslash
            elif c == b'"':
                State = DQuote
            elif c == b"'":
                State = SQuote
            elif c in (b' ', b'\t'):
                if State == Normal:
                    State = Space
                    args.append(arg)
                    arg = b''
                # else remain in the 'Space' state
            elif c == b'\n':
                break
            else:
                State = Normal
                arg += c
        elif State in (SBackslash, NBackslash, DQBackslash):
            if c == '\n':
                State = (DQuote if State == DQBackslash
                         else Space if State == SBackslash
                         else Normal)
            else:
                if State == DQBackslash and c not in (b'"', b'`', b'$', b'\\'):
                    arg += b'\\'
                arg += c
                State = DQuote if State == DQBackslash else Normal
        elif State == DQuote:
            if c == b'\\':
                State = DQBackslash
            # ` and $ are not interpreted by this parser.
            elif c == b'"':
                State = Normal
            else:
                arg += c
        elif State == SQuote:
            if c == b"'":
                State = Normal
            else:
                arg += c

    if State in (SQuote, DQuote, DQBackslash):
        raise EArgError(u"Unterminated string in .run file")
    if State in (SBackslash, NBackslash):
        raise EArgError(u"Backslash before EOF in .run file")

    if State == Normal:
        args.append(arg)
    return args


#import codecs
## sh-style argument parsing
## identify line continuations
#cont_re = re.compile( '\\\\\n'
#                      '|(?:\.|[^ \t\n\'])'
#                     r"|'[^']*'")
## separates words
#args_re = re.compile(r'(?:'
#                        r'\\.'
#                         '|[^ \t\n\'"]'
#                        r'|"(?:\\.|[^"])*"'
#                        r"|'[^']*'"
#                     r')+')
## matches types of parts of a word ('...', "...", \x, x)
#part_re = re.compile(r'(?:'
#                        r'\\.'
#                         '|[^ \t\'"]'
#                     r')+'
#                     r'|"(?:\\.|[^"])*"'
#                     r"|'[^']*'")
#
#    args = args_re.findall(s)
#    for i in range(len(args)):
#        arg = args[i]
#        argout = ''
#        for match in part_re.finditer(arg):
#            part = match.group()
#            if part[0] == '"':
#                argout += codecs.escape_decode(part[1:-1])[0]
#            elif part[0] == "'":
#                argout += part[1:-1]
#            else:
#                argout += codecs.escape_decode(part)[0]
#        args[i] = argout
#    return args

def tryRead(fn):
    result = None
    try:
        f = open(fn, 'rb')
        try:
            result = f.read()
        finally:
            f.close()
    except IOError as e:
        if e.errno != 2:
            raise
    return result

# In StringIO, mixing unicode and str causes problems with non-ASCII chars.
# Avoid it by overriding the write method, to always encode unicode as UTF-8.
class StrUTF8IO(StringStream):
    def write(self, s):
        StringStream.write(self, any2b(s))

def invokeMain(argv, stdin = None):
    """Invoke main.main, substituting stdin, stdout, stderr.
    Returns tuple with stdout and stderr."""
    # Revert globals to initial state
    lslcommon.LSO = False
    lslcommon.IsCalc = False
    lslcommon.Bugs.clear()
    lslcommon.Bugs.add(6495)
    save_stdin = sys.stdin
    save_stdout = sys.stdout
    save_stderr = sys.stderr
    stdout_output = None
    stderr_output = None
    try:
        sys.stdin = StringStream(stdin)
        sys.stdout = StrUTF8IO()
        sys.stderr = StrUTF8IO()
        sys.stdin.encoding = 'utf8'
        sys.stdout.encoding = 'utf8'
        sys.stderr.encoding = 'utf8'

        main.main(argv)

        stdout_output = sys.stdout.getvalue()
        stderr_output = sys.stderr.getvalue()
    finally:
        sys.stdin = save_stdin
        sys.stdout = save_stdout
        sys.stderr = save_stderr
        lslcommon.LSO = False
        lslcommon.IsCalc = False
        lslcommon.Bugs.clear()
        lslcommon.Bugs.add(6495)

    return (stdout_output, stderr_output)

#def tolEqual(actual, expected, tol):
#    """Strict equality. Like reallyEqual, but a tolerance can
#    be specified for comparing floats.
#    """
#    if type(actual) != type(expected):
#        return False
#
#    # Deal with floats (edge cases, tolerance)
#    if isinstance(actual, float):
#        # Signs must be equal
#        if math.copysign(1, actual) != math.copysign(1, expected):
#            return False
#        if math.isnan(actual):
#            # This compares the sign of NaN as well
#            return math.isnan(expected)
#        if math.isinf(actual) and math.isinf(expected):
#            return actual == expected
#        return abs(actual - expected) <= tol
#
#    # Deal with tuples and lists (item-by-item, recursively)
#    if isinstance(actual, (tuple, list)):
#        return all(tolEqual(i1, i2, tol)
#                   for i1, i2 in zip(actual, expected))
#
#    # Fall back to 'classic' equality
#    return actual == expected
#
#def reallyEqual(actual, expected):
#    """Strictest equality. The types must be equal. For floats, it checks
#    that the signs are equal, even for -0.0 and for NaNs. For the rest,
#    it falls back to ==.
#    """
#    return tolEqual(actual, expected, 0.0)
#
#def reprEqual(self, actual, expected):
#    """Returns whether the values are equal when comparing their repr's."""
#    return repr(actual) == repr(expected)

class UnitTestCase(unittest.TestCase):
    pass

class UnitTestRegression(UnitTestCase):
    def test_regression_misc(self):
        """Miscellaneous tests that can't be computed or are too difficult
        to compute with scripts
        """
        sys.stderr.write('\nRunning miscellaneous tests: ')
        # Test behaviour under BUG-3763
        lslcommon.Bugs.add(3763)
        self.assertEqual(lslfuncs.llXorBase64(u"ABCDABCDABCD", u"ABCD"),
                         u"AAAAAAAAABCT")
        self.assertEqual(lslfuncs.llXorBase64(u"ABCDABCDABCDABCDABCDABCDABCD",
                                              u"ABCD"),
                         u"AAAAAAAAABCTgxCDEJODAAAAABCT")
        self.assertEqual(lslfuncs.llXorBase64(u"ABCDABCDABCD", u"ABC="),
                         u"AACDEBCDEBCD")
        self.assertEqual(lslfuncs.llXorBase64(u"AQCDAQCD", u"AQC="),
                         u"AACCAQCC")
        lslcommon.Bugs.discard(3763)

        # Check that zstr returns the same type it is passed.
        self.assertEqual(type(lslfuncs.zstr(lslcommon.Key(u'x\0x'))),
                         lslcommon.Key)

    def test_regression_ll_json(self):
        from unit_tests import json
        # Patch llJsonSetValue, to allow running the test.
        json.llJsonSetValue = lambda x, y, z: u"***"
        sys.stderr.write('\nRunning JSON test module: ')
        save_stdout = sys.stdout
        save_stderr = sys.stderr
        stdout_output = False
        stderr_output = False
        try:
            sys.stdout = StringStream()
            sys.stdout.encoding = 'utf8'
            sys.stderr = StringStream()
            sys.stderr.encoding = 'utf8'
            errs = json.run_tests()
            stdout_output = sys.stdout.getvalue()
            stderr_output = sys.stderr.getvalue()
        finally:
            sys.stdout = save_stdout
            sys.stderr = save_stderr
        self.assertLessEqual(errs, 138)
        self.assertEqual(stdout_output, tryRead('unit_tests/json.out'))
        self.assertEqual(stderr_output, tryRead('unit_tests/json.err'))
        assert 'unit_tests.json' in sys.modules
        del sys.modules['unit_tests.json']

    def test_regression_parser(self):
        """Test the error cases. There are too many to make a test of each."""
        sys.stderr.write('\nRunning parser error tests: ')
        parser = lslparse.parser(lslloadlib.LoadLibrary())
        self.assertRaises(lslparse.EParseSyntax, parser.parse,
            'f(){integer i;i>>=i;}')
        self.assertRaises(lslparse.EParseCantChangeState, parser.parse,
            'f(){if(1)state default;else;}default{timer(){}}')
        self.assertRaises(lslparse.EParseCantChangeState, parser.parse,
            'f(){if(1);else state default;}default{timer(){}}')
        self.assertRaises(lslparse.EParseCantChangeState, parser.parse,
            'f(){if(1)if(1)state default;else;else;}default{timer(){}}')

        # Test behaviour of void functions
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'default{timer(){<llDie(),0,0>;}}')
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'default{timer(){[<llDie(),0,0>];}}')
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'default{timer(){key a=llDie();}}')
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'default{timer(){key a;a=llDie();}}')
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'default{timer(){do;while(llDie());}}')
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'default{timer(){for(;llDie(););}}')
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'default{timer(){while(llDie());}}')
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'default{timer(){if(llDie());}}')
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'default{timer(){if(llDie());else;}}')
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'default{timer(){[llDie()];}}', ('optimize',))
        parser.parse('default{timer(){[llDie()];}}')
        parser.parse('default{timer(){llDie();}}')
        parser.parse('default{timer(){(llDie());}}')
        parser.parse('default{timer(){for(llDie();1;llDie());}}',
            ('optimize',))
        # 'return <void expr>' works in the same situations as state changes
        self.assertRaises(lslparse.EParseReturnShouldBeEmpty, parser.parse,
            'default{timer(){return llDie();}}')
        self.assertRaises(lslparse.EParseReturnShouldBeEmpty, parser.parse,
            'default{timer(){if(1)return llDie();else;}}')
        self.assertRaises(lslparse.EParseReturnShouldBeEmpty, parser.parse,
            'default{timer(){if(1);else return llDie();}}')
        self.assertRaises(lslparse.EParseReturnShouldBeEmpty, parser.parse,
            'default{timer(){return 1;}}')
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'default{timer(){if(1)return 1;}}')

class UnitTestCoverage(UnitTestCase):
    def test_coverage_misc(self):
        """Miscellaneous tests that can't be computed or are too difficult
        to compute with scripts
        """
        sys.stderr.write('\nRunning misc coverage tests: ')
        # Doesn't accept bytes
        self.assertRaises(lslfuncs.ELSLInvalidType, lslfuncs.zstr, b"blah")
        # Can't typecast float to vector
        self.assertRaises(lslfuncs.ELSLTypeMismatch, lslfuncs.typecast,
                          lslfuncs.F32(1.2), lslcommon.Vector)
        # Can't typecast integer to vector
        self.assertRaises(lslfuncs.ELSLTypeMismatch, lslfuncs.typecast,
                          1, lslcommon.Vector)
        # Can't typecast vector to key
        self.assertRaises(lslfuncs.ELSLTypeMismatch, lslfuncs.typecast,
                          lslcommon.Vector((1.,2.,3.)), lslcommon.Key)
        # Can't typecast quaternion to key
        self.assertRaises(lslfuncs.ELSLTypeMismatch, lslfuncs.typecast,
                          lslcommon.Quaternion((1.,2.,3.,4.)), lslcommon.Key)
        # Can't typecast list to vector
        self.assertRaises(lslfuncs.ELSLTypeMismatch, lslfuncs.typecast,
                          [1, 1., lslcommon.Key(u'blah'),
                           lslcommon.Quaternion((1.,0.,0.,0.))],
                          lslcommon.Vector)
        # Can't typecast key to integer
        self.assertRaises(lslfuncs.ELSLTypeMismatch, lslfuncs.typecast,
                          lslcommon.Key(u"1"), int)
        # Can't negate string
        self.assertRaises(lslfuncs.ELSLTypeMismatch, lslfuncs.neg, u"3")
        # Can't add two keys
        self.assertRaises(lslfuncs.ELSLTypeMismatch, lslfuncs.add,
                          lslcommon.Key(u"1"), lslcommon.Key(u"2"))
        # Can't subtract two strings
        self.assertRaises(lslfuncs.ELSLTypeMismatch, lslfuncs.sub,
                          u"1", u"2")
        # Can't multiply two strings
        self.assertRaises(lslfuncs.ELSLTypeMismatch, lslfuncs.mul,
                          u"1", u"2")
        # Can't multiply quaternion and float in any order
        self.assertRaises(lslfuncs.ELSLTypeMismatch, lslfuncs.mul,
                          lslcommon.Quaternion((1.,2.,3.,4.)), 1.)
        self.assertRaises(lslfuncs.ELSLTypeMismatch, lslfuncs.mul,
                          1., lslcommon.Quaternion((1.,2.,3.,4.)))
        # Can't multiply quaternion by vector (but the opposite order is OK)
        self.assertRaises(lslfuncs.ELSLTypeMismatch, lslfuncs.mul,
                          lslcommon.Quaternion((1.,2.,3.,4.)),
                          lslcommon.Vector((1.,2.,3.)))
        # Can't divide quaternion by vector either
        self.assertRaises(lslfuncs.ELSLTypeMismatch, lslfuncs.div,
                          lslcommon.Quaternion((1.,2.,3.,4.)),
                          lslcommon.Vector((1.,2.,3.)))
        # Can't mod floats
        self.assertRaises(lslfuncs.ELSLTypeMismatch, lslfuncs.mod, 3., 3)
        # Can't compare string and integer
        self.assertRaises(lslfuncs.ELSLTypeMismatch, lslfuncs.compare, u'3', 4)
        self.assertRaises(lslfuncs.ELSLTypeMismatch, lslfuncs.less, u'3', 4)

        # Bytes is not a valid type to multiply by (in any order)
        self.assertRaises(lslfuncs.ELSLInvalidType, lslfuncs.mul, b"a", 3)
        self.assertRaises(lslfuncs.ELSLInvalidType, lslfuncs.mul,
                          lslcommon.Vector((3.,4.,5.)), b"a")
        self.assertRaises(lslfuncs.ELSLInvalidType, lslfuncs.typecast,
                          b"", unicode)

        # v2f/q2f coverage (force conversion from ints to floats)
        self.assertEqual(repr(lslfuncs.v2f(lslcommon.Vector((1,0,0)))),
                         'Vector((1.0, 0.0, 0.0))')
        self.assertEqual(repr(lslfuncs.q2f(lslcommon.Quaternion((1,0,0,0)))),
                         'Quaternion((1.0, 0.0, 0.0, 0.0))')
        # Key repr coverage
        self.assertEqual(repr(lslcommon.Key(u'')), "Key(u'')"
            if str != unicode else "Key('')")

        # string + key coverage
        self.assertEqual(lslfuncs.add(u'a', lslcommon.Key(u'b')), u'ab')
        self.assertEqual(type(lslfuncs.add(u'a', lslcommon.Key(u'b'))), unicode)

        # The SEF table prevents this assertion from being reachable via script.
        self.assertRaises(lslfuncs.ELSLCantCompute, lslfuncs.llXorBase64Strings,
                          u"AABA", u"AABA")
        self.assertRaises(lslfuncs.ELSLCantCompute, lslfuncs.llModPow,
                          3, 5, 7)
        # Check invalid type in llGetListEntryType
        self.assertRaises(lslfuncs.ELSLInvalidType, lslfuncs.llGetListEntryType,
            [b'a'], 0)

        # Check that Value2LSL raises an exception if the type is unknown.
        outmod = lsloutput.outscript()
        # Script with a single node of type Expression, containing a constant
        # of type Bytes. That's rejected by the output module.
        msg = None
        script = [nr(nt='EXPR', t='string', ch=[
            nr(nt='CONST', t='string', value=b'ab')
        ])]
        save_IsCalc = lslcommon.IsCalc
        lslcommon.IsCalc = True
        try:
            try:
                outmod.output((script, ()))
            except AssertionError as e:
                msg = str(e)
        finally:
            lslcommon.IsCalc = save_IsCalc
        self.assertEqual(msg, u"Value of unknown type in Value2LSL: 'ab'")
        del msg
        # Extended assignment in output
        script = [nr(nt='EXPR', t='integer', ch=[
            nr(nt='^=', t='integer', ch=[
                nr(nt='IDENT', t='integer', name='a', scope=0),
                nr(nt='CONST', t='integer', value=3)
            ])])]
        save_IsCalc = lslcommon.IsCalc
        lslcommon.IsCalc = True
        try:
            out = outmod.output((script, [{'a':{'Kind':'v','Loc':1,'Scope':0,
                                                'Type':'integer'}
                                         }]
                               ))
        finally:
            lslcommon.IsCalc = save_IsCalc

        self.assertEqual(out, 'a = a ^ (3)')
        del out, script, outmod, save_IsCalc

    def test_coverage_parser(self):
        """Cover the error cases. There are too many to make a test of each."""
        parser = lslparse.parser(lslloadlib.LoadLibrary(
            builtins = 'unit_tests/builtins-coverage-2.txt',
            fndata = 'unit_tests/builtins-coverage-2.txt'))
        self.assertRaises(lslparse.EParseNoConversion, parser.parse,
            'f(){list L;(integer)L[0];}', ('lazylists',))
        parser = lslparse.parser(lslloadlib.LoadLibrary())
        sys.stderr.write('\nRunning parser exception coverage tests: ')
        # Parse_unary_postfix_expression
        self.assertRaises(lslparse.EParseUEOF, parser.parse, u'f(){key x=')
        self.assertRaises(lslparse.EParseUndefined, parser.parse,
            'f(){g();}')
        self.assertRaises(lslparse.EParseUndefined, parser.parse,
            'integer g;f(){g();}')
        self.assertRaises(lslparse.EParseUndefined, parser.parse,
            'f(){f=0;}')
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'f(){integer V; V[1] = 0;}', ('lazylists',))
        self.assertRaises(lslparse.EParseFunctionMismatch, parser.parse,
            'f(){list V; V[1,1] = 0;}', ('lazylists',))
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'f(){list V; V[""] = 0;}', ('lazylists',))
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'f(){list V; V[1] = llDie();}', ('lazylists',))
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'f(){string s;s++;}')
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'f(){string s;++s;}')
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'f(){string s;s=llDie();}')
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'f(){string s;s+=(key)"";}')
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'f(){string s;s-=s;}')
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'f(){string s;s*=2;}')
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'f(){vector v;v%=1.0;}')
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'f(){string s;s>>=s;}', ('extendedassignment',))
        # Parse_unary_expression
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'f(){-"";}')
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'f(){!"";}')
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'f(){~"";}')
        self.assertRaises(lslparse.EParseUndefined, parser.parse,
            'f(){++f;}')
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'f(){(key)1;}')
        self.assertRaises(lslparse.EParseFunctionMismatch, parser.parse,
            'f(){list L;(integer)L[""];}', ('lazylists',))
        # Parse_factor
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'f(){""*2;}')
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'f(){<1,1,1>%2;}')
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'f(){<1,1,1>/<1,1,1>;}')
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'f(){<1,1,1>/"";}')
        # Parse_term
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'f(){llDie()+1;}')
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'f(){""-1;}')
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'f(){[]+llDie();}')
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'f(){(key)""+(key)"";}')
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'f(){""+(key)"";}')
        # Parse_shift
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'f(){"">>1;}')
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'f(){1<<"";}')
        # Parse_inequality
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'f(){""<"";}')
        # Parse_comparison
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'f(){llDie()==3;}')
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'f(){""==3;}')
        # Parse_bitbool_factor
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'f(){""&3;}')
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'f(){3&"";}')
        # Parse_bitxor_term
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'f(){""^3;}')
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'f(){3^"";}')
        # Parse_bitbool_term
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'f(){""|3;}')
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'f(){3|"";}')
        # Parse_expression
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'f(){3||"";}')
        self.assertRaises(lslparse.EParseTypeMismatch, parser.parse,
            'f(){""&&3;}')
        # Parse_optional_expression_list
        self.assertRaises(lslparse.EParseFunctionMismatch, parser.parse,
            'f(){llSay(0);}')
        self.assertRaises(lslparse.EParseAlreadyDefined, parser.parse,
            'f(){@x;@x;}')
        self.assertRaises(lslparse.EParseAlreadyDefined, parser.parse,
            'f(){integer x;integer x;}')
        self.assertRaises(lslparse.EParseAlreadyDefined, parser.parse,
            'f(integer x, integer x){}')
        self.assertRaises(lslparse.EParseAlreadyDefined, parser.parse,
            'default{timer(){}timer(){}}')
        self.assertRaises(lslparse.EParseSyntax, parser.parse,
            'default{timer(){state state;}}')
        self.assertRaises(lslparse.EParseUndefined, parser.parse,
            'default{timer(){state undefined;}}')
        self.assertRaises(lslparse.EParseSyntax, parser.parse,
            'default{timer(){switch(1){case 1;}}}', ('enableswitch',))
        self.assertRaises(lslparse.EParseSyntax, parser.parse,
            'default{timer(){switch(1){default;}}}', ('enableswitch',))
        self.assertRaises(lslparse.EParseInvalidBrkContArg, parser.parse,
            'default{timer(){while(1){break 0;}}}', ('breakcont',))
        self.assertRaises(lslparse.EParseInvalidBrkContArg, parser.parse,
            'default{timer(){while(1){break 2;}}}', ('breakcont',))
        self.assertRaises(lslparse.EParseInvalidBrkContArg, parser.parse,
            'default{timer(){while(1){continue 0;}}}', ('breakcont',))
        self.assertRaises(lslparse.EParseInvalidBrkContArg, parser.parse,
            'default{timer(){while(1){continue 2;}}}', ('breakcont',))
        self.assertRaises(lslparse.EParseSyntax, parser.parse,
            'integer T=-TRUE;default{timer(){}}')
        self.assertRaises(lslparse.EParseSyntax, parser.parse,
            'list L=[[]];default{timer(){}}')
        self.assertRaises(lslparse.EParseSyntax, parser.parse,
            'default{timer(integer i){}}')
        self.assertRaises(lslparse.EParseSyntax, parser.parse,
            'i = 0;',)
        self.assertRaises(lslparse.EParseSyntax, parser.parse,
            'default{timer(){}}state{timer(){}}')
        self.assertRaises(lslparse.EParseUndefined, parser.parse,
            'default{timer(){jump undefined;}}')
        # BuildTempGlobalsTable coverage
        self.assertRaises(lslparse.EParseSyntax, parser.parse,
            ';')
        self.assertRaises(lslparse.EParseSyntax, parser.parse,
            'f(;')
        self.assertRaises(lslparse.EParseSyntax, parser.parse,
            'f();')
        self.assertRaises(lslparse.EParseSyntax, parser.parse,
            'integer f=')
        self.assertRaises(lslparse.EParseUEOF, parser.parse,
            'integer /*')
        self.assertRaises(lslparse.EParseSyntax, parser.parse,
            'default{timer(){}}state e;')

class UnitTestExpr(UnitTestCase):
    pass

class UnitTestLSO(UnitTestCase):
    pass

class UnitTestPreproc(UnitTestCase):
    pass

def generateScriptTests():
    """Find all files in unit_tests/*.d/*.{lsl,run} and generate tests for
    them.
    """

    path = os.path.dirname(__file__)
    if path:
        os.chdir(path)

    testsuites = ('Regression', 'Coverage', 'Expr', 'LSO', 'Preproc')
    for testsuite in testsuites:
        files = glob.glob(os.path.join('unit_tests',
                                       testsuite.lower() + '.suite', '*.lsl')
           )  + glob.glob(os.path.join('unit_tests',
                                       testsuite.lower() + '.suite', '*.run')
           )
        files = list(set([os.path.splitext(x)[0] for x in files]))
        files.sort()
        for fbase in files:
            # Create a closure with the test data
            def makeTestFunction(fbase, suite):
                def TestFunction(self):
                    stdin = tryRead(fbase + '.lsl') or ''
                    expected_stdout = tryRead(fbase + '.out') or b''
                    expected_stderr = tryRead(fbase + '.err') or b''
                    runargs = (parseArgs(tryRead(fbase + '.run'))
                               or (['main.py', '-y', '-'] if suite != 'Expr'
                                   else ['main.py',
                                         # Defaults for Expr:
                                         '-O', 'clear,optimize,constfold'
                                               ',addstrings,expr',
                                         '-y',
                                         '-']))
                    werr(u"\nRunning test %s: " % any2u(fbase))
                    actual_stdout, actual_stderr = invokeMain(runargs, stdin)
                    actual_stdout = (actual_stdout.replace(b'\r',b'\r\n')
                                     .replace(b'\r\n\n',b'\n')
                                     .replace(b'\r\n',b'\n'))

                    actual_stderr = (actual_stderr.replace(b'\r',b'\r\n')
                                     .replace(b'\r\n\n',b'\n')
                                     .replace(b'\r\n',b'\n'))

                    try:
                        if expected_stderr.startswith(b'REGEX\n'):
                            self.assertIsNotNone(
                                re.search(expected_stderr[6:],
                                          actual_stderr.decode('utf8')
                                )
                            )
                        else:
                            self.assertTrue(expected_stderr == actual_stderr)
                    except AssertionError:
                        werr(u'Failed'
                             u'\n************ expected stderr:\n')
                        werr(expected_stderr)
                        werr(u'\n************ actual stderr:\n')
                        werr(actual_stderr)
                        if difflib and expected_stderr and actual_stderr:
                            sys.stderr.write(u'\n************ diff:\n'
                                 + u'\n'.join(difflib.unified_diff(
                                    b2u(expected_stderr).split(u'\n'),
                                    b2u(actual_stderr).split(u'\n'),
                                    'expected', 'actual', lineterm=''
                            )))
                        werr(u'\n************ ')
                        raise
                    try:
                        if expected_stdout.startswith(b'REGEX\n'):
                            self.assertIsNotNone(re.search(expected_stdout[6:],
                                                           actual_stdout))
                        else:
                            self.assertTrue(expected_stdout == actual_stdout)
                    except AssertionError:
                        werr(u'Failed'
                             u'\n************ expected stdout:\n')
                        werr(expected_stdout)
                        werr(u'\n************ actual stdout:\n')
                        werr(actual_stdout)
                        if difflib and expected_stdout and actual_stdout:
                            werr(u'\n************ diff:\n'
                                 + u'\n'.join(difflib.unified_diff(
                                    b2u(expected_stdout).split('\n'),
                                    b2u(actual_stdout).split('\n'),
                                    'expected', 'actual', lineterm=''
                            )))
                        sys.stderr.write(u'\n************ ')
                        raise
                return TestFunction
            TestFunction = makeTestFunction(fbase, testsuite)
            # __doc__ is used by Eric
            line = b''
            try:
                f = open(fbase + '.lsl', 'rb')
                try:
                    line = f.readline()
                    if line.endswith(b'\r\n'):
                        line = line[:-2]
                    elif line[-1:] in (b'\r', b'\n'):
                        line = line[:-1]
                finally:
                    f.close()
            except IOError as e:
                if e.errno != 2:
                    raise
            TestFunction.__doc__ = (b2u(line[3:]) if line.startswith(b'// ')
                                    else None)

            TestFunction.__name__ = ('test_' + testsuite + '__'
                + os.path.basename(fbase).replace('-','_'))
            fail = tryRead(fbase + '.fail')
            if fail is not None:
                if fail:
                    TestFunction.__doc__ = b2u(fail)
                TestFunction = unittest.expectedFailure(TestFunction)
            else:
                skip = tryRead(fbase + '.skp')
                if skip is not None:
                    TestFunction = unittest.skip(skip)(TestFunction)
            setattr(globals()['UnitTest' + testsuite],
                    TestFunction.__name__, TestFunction)


generateScriptTests()
if __name__ == '__main__':
    unittest.main(argv = sys.argv)
#UnitTestRegression().test_Regression__multiline_string()
