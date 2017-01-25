#!/usr/bin/env python2

#    (C) Copyright 2015-2016 Sei Lisa. All rights reserved.
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

# This is the main executable program that imports the libraries.

from lslopt.lslparse import parser,EParse,fieldpos
from lslopt.lsloutput import outscript
from lslopt.lsloptimizer import optimizer
import sys, os, getopt, re
import lslopt.lslcommon


VERSION = '0.2.1beta'


def ReportError(script, e):
    lastpos = fieldpos(script, '\n', e.lno+1)-1
    assert lastpos != -1
    if lastpos < -1: lastpos = len(script) # may hit EOF
    sys.stderr.write(script[fieldpos(script, '\n', e.lno):lastpos].decode('utf8') + u"\n")
    sys.stderr.write(u" " * e.cno + u"^\n")
    sys.stderr.write(e.args[0] + u"\n")

class UniConvScript(object):
    """Converts the script to Unicode, setting the properties required by
    EParse to report a meaningful error position.
    """
    def __init__(self, script):
        self.script = script

    def to_unicode(self):
        if type(self.script) is not unicode:
            try:
                self.script = self.script.decode('utf8')
            except UnicodeDecodeError as e:
                self.errorpos = e.start
                raise EParse(self, u"Invalid UTF-8 in script")
        return self.script

def PreparePreproc(script):
    s = ''
    nlines = 0
    col = 0

    # Trigraphs make our life really difficult.
    # We join lines that have \<return> or ??/<return> inside strings,
    # and we also replace regular <return> inside strings with \n, counting how
    # many lines we join, to add them back at the end of the string in order to
    # keep the line count exact prior to preprocessing. We also preserve the
    # original column of the text after the string, by adding as many spaces as
    # necessary.
    # We could let the preprocessor do the line joining on backslash-newline,
    # but by eliminating all newlines, we have control over the output column
    # of the text that follows the string and can report an accurate column
    # and line position in case of error.
    # The REs skip as much as possible in one go every time, only stopping to
    # analyze critical tokens.
    # We don't follow the C convention that backslash-return is analyzed first.
    # In c, the string "a\\<return>nb" is the same as "a\nb" which prints as
    # a<return>b. But in LSL, forgetting about the preprocessor, the string
    # "a\\<return>nb" is valid and stands for a\<return>nb. The principle of
    # least surprise seems to suggest to accept valid LSL strings as LSL
    # instead of reproducing that C quirk. This also matches what FS is doing
    # currently, so it's good for compatibility.
    tok = re.compile(
        r'(?:'
            r'/(?:\?\?/\n|\\\n)*\*.*?\*(?:\?\?/\n|\\\n)*/'
            r'|/(?:\?\?/\n|\\\n)*/(?:\?\?/\n|\\\n|[^\n])*\n'
            r'|[^"]'
        r')+'
        r'|"'
        , re.S)
    # RE used inside strings.
    tok2 = re.compile(
        r'(?:'
            r"\?\?[='()!<>-]"   # valid trigraph except ??/ (backslash)
            r"|(?:\?\?/|\\)(?:\?\?[/='()!<>-]|[^\n])"
                                # backslash trigraph or actual backslash,
                                # followed by any trigraph or non-newline
            r'|(?!\?\?/\n|\\\n|"|\n).'
                                # any character that doesn't start a trigraph/
                                # backslash escape followed by a newline
                                # or is a newline or double quote, as we're
                                # interested in all those individually.
        r')+'                   # as many of those as possible
        r'|\?\?/\n|\\\n|\n|"'   # or any of those individually
        )

    pos = 0
    match = tok.search(script, pos)
    while match:
        matched = match.group(0)
        pos += len(matched)
        if matched == '"':
            s += matched
            nlines = col = 0
            match2 = tok2.search(script, pos)
            while match2:
                matched2 = match2.group(0)
                pos += len(matched2)

                if matched2 == '\\\n' or matched2 == '??/\n':
                    nlines += 1
                    col = 0
                    match2 = tok2.search(script, pos)
                    continue
                if matched2 == '"':
                    if nlines:
                        if script[pos:pos+1] == '\n':
                            col = -1 # don't add spaces if not necessary
                        # col misses the quote added here, so add 1
                        s += '"' + '\n'*nlines + ' '*(col+1)
                    else:
                        s += '"'
                    break
                if matched2 == '\n':
                    nlines += 1
                    col = 0
                    s += '\\n'
                else:
                    col += len(matched2)
                    s += matched2
                match2 = tok2.search(script, pos)

        else:
            s += matched
        match = tok.search(script, pos)

    return s

def ScriptHeader(script, avname):
    if avname:
        avname = ' - ' + avname
    return ('//start_unprocessed_text\n/*'
        + re.sub(r'([*/])(?=[*|/])', r'\1|', script)
        + '*/\n//end_unprocessed_text\n//nfo_preprocessor_version 0\n'
          '//program_version LSL PyOptimizer v' + VERSION + avname
        + '\n//mono\n\n')

def Usage(progname, about = None):
    if about is None:
        sys.stderr.write(
ur"""LSL optimizer v{version}

    (C) Copyright 2015 Sei Lisa. All rights reserved.

    This program comes with ABSOLUTELY NO WARRANTY.
    This is free software, and you are welcome to redistribute it
    under certain conditions; see the file COPYING for details.
    This program is licensed under the GNU General Public License
    version 3.

Usage: {progname}
    [-O|--optimizer-options=[+|-]option[,[+|-]option[,...]]]
                                optimizer options (use '-O help' for help)
    [-h|--help]                 print this help
    [--version]                 print this program's version
    [-o|--output=<filename>]    output to file rather than stdout
    [-H|--header]               add the script as a comment in Firestorm format
    [-T|--timestamp]            add a timestamp as a comment at the beginning
    [-y|--python-exceptions]    when an exception is raised, show a stack trace
    [-p|--preproc=mode]         run external preprocessor (see below for modes)
                                (resets the preprocessor command line so far)
    [-P|--prearg=<arg>]         add parameter to preprocessor's command line
    [--precmd=<cmd>]            preprocessor command ('cpp' by default)
    [--prenodef]                no LSL specific defines (__AGENTKEY__ etc.)
    [--preshow]                 show preprocessor output, and stop
    [--avid=<UUID>]           * specify UUID of avatar saving the script
    [--avname=<name>]         * specify name of avatar saving the script
    [--assetid=<UUID>]        * specify the asset UUID of the script
    [--shortname=<name>]      * specify the script's short file name
    filename                    input file

Options marked with * are used to define the preprocessor macros __AGENTID__,
__AGENTKEY__, __AGENTIDRAW__, __AGENTNAME__, __ASSETID__ and __SHORTFILE__,
and have no effect if --prenodef is specified.

Using --prenodef before -p causes no macros whatsoever to be defined. If used
after -p, the type casting macros string(...), key(...), etc. will be defined.

If filename is a dash (-) then standard input is used.
Use: {progname} -O help for help on the optimizer control options.

Preprocessor modes:
    ext       Invoke preprocessor
    mcpp      Invoke mcpp as preprocessor, setting default parameters pertinent
              to it. Implies --precmd=mcpp
    gcpp      Invoke GNU cpp as preprocessor, setting default parameters
              pertinent to it. Implies --precmd=cpp
    none      No preprocessing (default)

Normally, running the preprocessor needs the option 'processpre' active, to
make the output readable by the optimizer. This option is active by default.
""".format(progname=progname, version=VERSION))
        return

    if about == 'optimizer-options':
        sys.stderr.write(
ur"""
Optimizer control options.
+ means active by default, - means inactive by default.
Case insensitive.

  Syntax extensions options:

  ExtendedGlobalExpr + Enables arbitrary expressions in globals (as opposed to
                       dull simple expressions allowed by regular LSL). Needs
                       constant folding active for the result to be compilable.
  BreakCont          - Allow break/continue statements for loops. Note that
                       when active, 'break' and 'continue' become reserved
                       words, but when inactive they can be used as variables.
  ExtendedTypeCast   + Allows extended typecast syntax e.g. (string)(integer)a
                       is valid with this option.
  ExtendedAssignment + Enables &=, |=, ^=, <<=, >>= assignment operators.
  AllowKeyConcat     + Allow string + key and key + string (both return string)
  AllowMultiStrings  + Allow C-like string juxtaposition, e.g. "ab" "cd" means
                       "abcd", no concatenation involved. Very useful when used
                       with a preprocessor. Similar to addstrings, but this one
                       is not an optimization, it introduces new syntax.
  DupLabels          - Normally, a duplicate label within a function is allowed
                       by the syntax by using {{}} blocks; however, the server
                       will just refuse to save the script (under Mono) or do
                       something completely unexpected (under LSO: all jumps
                       will go to the last label with that name). This flag
                       works around that limitation by replacing the names of
                       the labels in the output with unique ones.

  Deprecated / compatibility syntax extensions options:

  LazyLists          - Support syntax like mylist[index] = 5; rather than using
                       llListReplaceList. Only assignment supported. The list
                       is extended when the argument is greater than the list
                       length, by inserting integer zeros. This is implemented
                       for compatibility with Firestorm, but its use is not
                       recommended, as it adds a new function, wasting memory
                       against the very spirit of this program.
  EnableSwitch       - Support C-like switch() syntax, with some limitations.
                       Like lazylists, it's implemented for compatibility with
                       Firestorm, but not recommended. Note that the operand to
                       switch() may be evaluated more than once.
  ErrMissingDefault  + Throw an error in case the 'default:' label of a switch
                       statement is missing.
  FuncOverride       - Allow duplicate function definitions to override the
                       previous definition. For compatibility with Firestorm's
                       optimizer.

  Optimization options

  Optimize           + Runs the optimizer.
  OptSigns           + Optimize signs in float and integer constants.
  OptFloats          + Optimize floats that represent an integral value.
  ConstFold          + Fold constant expressions to their values, and simplify
                       some expressions and statements.
  DCR                + Dead code removal. This option removes several instances
                       of code that will never execute, and performs other
                       optimizations like removal of unused variables,
                       functions or expressions.
  ShrinkNames        - Reduces script memory by shrinking identifiers. In the
                       process, it turns the script into unreadable gibberish,
                       hard to debug, but this gets big savings for complex
                       scripts.
  AddStrings         - Concatenate strings together when possible. Note that
                       such an optimization can be counter-productive in some
                       cases, that's why it's disabled by default. For example:
                       string a="a"+"longstring"; string b="b"+"longstring";
                       would keep a single copy of "longstring", while if the
                       strings are added, both "alongstring" and "blongstring"
                       take memory.
  ListLength         + Optimize llGetListLength(arg) to arg!=[].

  Miscellaneous options

  FoldTabs           - Tabs can't be copy-pasted, so expressions that produce
                       tabs, e.g. llUnescapeURL("%09"), aren't optimized by
                       default. This option overrides that check, enabling
                       expansion of functions that produce strings with tabs.
                       The resulting source isn't guaranteed to be
                       copy-paste-able to the viewer.
  WarnTabs           + Warn when a function can't be optimized because it
                       generates a string or list with a tab, or when a string
                       contains a tab.
  ProcessPre         + Process some preprocessor directives in the source. This
                       enables usage of #pragma/#line preprocessor directives,
                       and is probably necessary if the script is itself the
                       output of a preprocessor. Note that this option does not
                       make the optimizer process macros.
  ExplicitCast       - Add explicit casts where they are implicit. This option
                       is useless with 'optimize' and 'optsigns', and is of
                       basically no use in general, other than to see where
                       automatic casts happen.
  Clear              - Set all options to inactive, Normally used as the first
                       option, to start afresh. Note that this sets to inactive
                       even the options that are active by default.

For example:
   {progname} -O -DCR,+BreakCont scriptname.lsl
would turn off dead code removal (which is active by default) and turn on the
break/continue syntax extension (which is inactive by default).
""".format(progname=progname))
        return

validoptions = frozenset(('extendedglobalexpr','breakcont','extendedtypecast',
    'extendedassignment','allowkeyconcat','allowmultistrings','duplabels',
    'lazylists','enableswitch','errmissingdefault','funcoverride','optimize',
    'optsigns','optfloats','constfold','dcr','shrinknames','addstrings',
    'foldtabs','warntabs','processpre','explicitcast','listlength',
    'help','lso','expr'
    # 'clear' is handled as a special case
))

def main(argv):
    """Main executable."""

    # If it's good to append the basename to it, it's good to append the
    # auxiliary files' names to it, which should be located where this file is.
    lslopt.lslcommon.DataPath = __file__[:-len(os.path.basename(__file__))]

    # Default options
    options = set(('extendedglobalexpr','extendedtypecast','extendedassignment',
        'allowkeyconcat','allowmultistrings','processpre','warntabs','optimize',
        'optsigns','optfloats','constfold','dcr','errmissingdefault',
        'listlength',
        ))

    assert not (options - validoptions), (u"Default options not present in"
        u" validoptions: '%s'"
        % (b"', '".join(options - validoptions)).decode('utf8'))

    try:
        opts, args = getopt.gnu_getopt(argv[1:], 'hO:o:p:P:HTy',
            ('optimizer-options=', 'help', 'version', 'output=', 'header',
            'timestamp','python-exceptions',
            'preproc=', 'precmd=', 'prearg=', 'prenodef', 'preshow',
            'avid=', 'avname=', 'assetid=', 'shortname='))
    except getopt.GetoptError as e:
        Usage(argv[0])
        sys.stderr.write(u"\nError: " + str(e).decode('utf8') + u"\n")
        return 1

    outfile = '-'
    avid = '00000000-0000-0000-0000-000000000000'
    avname = ''
    shortname = ''
    assetid = '00000000-0000-0000-0000-000000000000'
    preproc_cmdline = ['cpp']
    preproc = 'none'
    predefines = True
    script_header = ''
    script_timestamp = ''
    mcpp_mode = False
    preshow = False
    raise_exception = False

    for opt, arg in opts:
        if type(opt) is unicode:
            opt = opt.encode('utf8')
        if type(arg) is unicode:
            arg = arg.encode('utf8')

        if opt in ('-O', '--optimizer-options'):
            optchanges = arg.lower().split(',')
            for chg in optchanges:
                if not chg:
                    continue
                if chg in ('clear', '+clear'):
                    options = set()
                    continue
                if chg == '-clear':
                    # ignore
                    continue
                chgfix = chg
                if chgfix[0] not in ('+', '-'):
                    chgfix = '+' + chgfix
                if chgfix[1:] not in validoptions:
                    Usage(argv[0], 'optimizer-options')
                    sys.stderr.write(u"\nError: Unrecognized"
                        u" optimizer option: %s\n" % chg.decode('utf8'))
                    return 1
                if chgfix[0] == '-':
                    options.discard(chgfix[1:])
                else:
                    options.add(chgfix[1:])
                del chgfix

        elif opt in ('-h', '--help'):
            Usage(argv[0])
            return 0

        elif opt == '--version':
            sys.stdout.write('LSL PyOptimizer version %s\n' % VERSION)
            return 0

        elif opt in ('-o', '--output'):
            outfile = arg

        elif opt in ('-y', '--python-exceptions'):
            raise_exception = True

        elif opt in ('-p', '--preproc'):
            preproc = arg.lower()
            supported = ('ext', 'mcpp', 'gcpp', 'none')
            if preproc not in supported:
                Usage(argv[0])
                sys.stderr.write(u"\nUnknown --preproc option: '%s'."
                    u" Only '%s' supported.\n"
                    % (preproc, u"', '".join(supported)))
                return 1

            mcpp_mode = False
            del preproc_cmdline[1:]

            if preproc == 'gcpp':
                preproc_cmdline = [
                    'cpp', '-undef', '-x', 'c', '-std=c99', '-nostdinc',
                    '-trigraphs', '-dN', '-fno-extended-identifiers',
                    ]

            elif preproc == 'mcpp':
                mcpp_mode = True
                preproc_cmdline = [
                    'mcpp', '-e', 'UTF-8', '-I-', '-N', '-3', '-j',
                    ]

            if predefines:
                preproc_cmdline += [
                    '-Dinteger(...)=((integer)(__VA_ARGS__))',
                    '-Dfloat(...)=((float)(__VA_ARGS__))',
                    '-Dstring(...)=((string)(__VA_ARGS__))',
                    '-Dkey(...)=((key)(__VA_ARGS__))',
                    '-Drotation(...)=((rotation)(__VA_ARGS__))',
                    '-Dquaternion(...)=((quaternion)(__VA_ARGS__))',
                    '-Dvector(...)=((vector)(__VA_ARGS__))',
                    '-Dlist(...)=((list)(__VA_ARGS__))',
                    ]

        elif opt == '--precmd':
            preproc_cmdline[0] = arg

        elif opt in ('-P', '--prearg'):
            preproc_cmdline.append(arg)

        elif opt == '--prenodef':
            predefines = False

        elif opt == '--preshow':
            preshow = True

        elif opt in ('-H', '--header'):
            script_header = True

        elif opt in ('-T', '--timestamp'):
            script_timestamp = True

        elif opt == '--avid':
            avid = arg

        elif opt == '--avname':
            avname = arg

        elif opt == '--assetid':
            assetid = arg

        elif opt == '--shortname':
            shortname = arg
    del opts

    try:

        if 'lso' in options:
            lslopt.lslcommon.LSO = True
            options.remove('lso')

        if 'expr' in options:
            lslopt.lslcommon.IsCalc = True
            options.remove('expr')

        if 'help' in options:
            Usage(argv[0], 'optimizer-options')
            return 0

        fname = args[0] if args else None
        if fname is None:
            Usage(argv[0])
            sys.stderr.write(u"\nError: Input file not specified. Use -"
                u" if you want to use stdin.\n")
            return 1

        del args

        script = ''
        if fname == '-':
            script = sys.stdin.read()
        else:
            try:
                f = open(fname, 'r')
            except IOError as e:
                if e.errno == 2:
                    sys.stderr.write('Error: File not found: %s\n' % fname)
                    return 2
                raise
            try:
                script = f.read()
            finally:
                f.close()
                del f

        if script_header:
            script_header = ScriptHeader(script, avname)

        if script_timestamp:
            import time
            tmp = time.time()
            script_timestamp = time.strftime(
                '// Generated on %Y-%m-%dT%H:%M:%S.{0:06d}Z\n'
                .format(int(tmp % 1 * 1000000)), time.gmtime(tmp))
            del tmp

        if shortname == '':
            shortname = os.path.basename(fname)

        if predefines:
            preproc_cmdline.append('-D__AGENTKEY__="' + avid + '"')
            preproc_cmdline.append('-D__AGENTID__="' + avid + '"')
            preproc_cmdline.append('-D__AGENTIDRAW__=' + avid)
            preproc_cmdline.append('-D__AGENTNAME__="' + avname + '"')
            preproc_cmdline.append('-D__ASSETID__=' + assetid)
            preproc_cmdline.append('-D__SHORTFILE__="' + shortname + '"')
            preproc_cmdline.append('-D__OPTIMIZER__=LSL PyOptimizer')
            preproc_cmdline.append('-D__OPTIMIZER_VERSION__=' + VERSION)

        if type(script) is unicode:
            script = script.encode('utf8')
        else:
            try:
                # Try converting the script to Unicode, to report any encoding
                # errors with accurate line information. At this point we don't
                # need the result.
                UniConvScript(script).to_unicode()
            except EParse as e:
                # We don't call ReportError to prevent problems due to
                # displaying invalid UTF-8
                sys.stderr.write(e.args[0] + u"\n")
                return 1

        if preproc != 'none':
            # At this point, for the external preprocessor to work we need the
            # script as a byte array, not as unicode, but it should be UTF-8.
            script = PreparePreproc(script)
            if mcpp_mode:
                # As a special treatment for mcpp, we force it to output its
                # macros so we can read if USE_xxx are defined. With GCC that
                # is achieved with -dN, but mcpp has no command line option.
                script += '\n#pragma MCPP put_defines\n'

            # Invoke the external preprocessor
            import subprocess

            p = subprocess.Popen(preproc_cmdline, stdin=subprocess.PIPE,
                stdout=subprocess.PIPE)
            script = p.communicate(input=script)[0]
            status = p.wait()
            if status:
                return status
            del p, status

            # This method is very imperfect, in several senses. However, since
            # it's applied to the output of the preprocessor, all of the
            # concerns should be addressed:
            #    - \s includes \n, but \n should not be allowed.
            #    - Comments preceding the directive should not cause problems.
            #              e.g.: /* test */ #directive
            #    - #directive within a comment or string should be ignored.
            for x in re.findall(r'(?:(?<=\n)|^)\s*#\s*define\s+('
                                r'USE_SWITCHES'
                                r'|USE_LAZY_LISTS'
                                r')(?:$|[^A-Za-z0-9_])', script, re.S):
                if x == 'USE_SWITCHES':
                    options.add('enableswitch')
                elif x == 'USE_LAZY_LISTS':
                    options.add('lazylists')

        if not preshow:

            p = parser()
            try:
                ts = p.parse(script, options)
            except EParse as e:
                ReportError(script, e)
                return 1
            del p, script

            opt = optimizer()
            ts = opt.optimize(ts, options)
            del opt

            outs = outscript()
            script = script_header + script_timestamp + outs.output(ts, options)
            del outs, ts

        del script_header, script_timestamp

        if outfile == '-':
            sys.stdout.write(script)
        else:
            outf = open(outfile, 'w')
            try:
                outf.write(script)
            finally:
                outf.close()
        return 0

    except Exception as e:
        if raise_exception:
            raise
        sys.stderr.write(e.__class__.__name__ + ': ' + str(e) + '\n')
        return 1

if __name__ == '__main__':
    ret = main(sys.argv)
    if ret:
        sys.exit(ret)
