#!/usr/bin/env python2
#
#    (C) Copyright 2015-2023 Sei Lisa. All rights reserved.
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
#    If you choose to use a later version of the GPL, please modify the text
#    in the Usage() function appropriately to indicate the correct version.
#
#    Sei Lisa is the author's username in the Second Life(R) virtual world.
#    Second Life is a registered trademark of Linden Research, Inc.

# This is the main executable program that imports the libraries.

from lslopt.lslparse import parser,EParse
from lslopt.lsloutput import outscript
from lslopt.lsloptimizer import optimizer
import sys, os, getopt, re
import lslopt.lslcommon
import lslopt.lslloadlib
from strutil import *


VERSION = '0.3.0beta'


def ReportError(script, e):
    linestart = script.rfind('\n', 0, e.errorpos) + 1
    lineend = script.find('\n', e.errorpos)
    if lineend == -1: lineend = len(script) # may hit EOF

    # When the encoding of stderr is unknown (e.g. when redirected to a file),
    # output will be encoded in UTF-8; otherwise the terminal's encoding will
    # be used.
    enc = getattr(sys.stderr, 'encoding', 'utf8') or 'utf8'

    # Synchronize the UTF-8 encoded line with the output line in the
    # terminal's encoding. We need to compensate for the fact that the
    # reported column applies to the UTF-8 version of the script.
    # 1. Trim the UTF-8 line.
    err_frag = script[linestart:e.errorpos]
    # 2. Convert to Unicode; encode in the target encoding with replacing.
    err_frag = str2u(err_frag, 'utf8').encode(enc, 'backslashreplace')
    # 3. Collect our prize: the length of that in characters.
    cno = len(err_frag.decode(enc))

    # Write the whole line in the target encoding.
    err_line = script[linestart:lineend] + '\n'
    werr(err_line)
    werr(u" " * cno + u"^\n")
    werr(e.args[0] + u"\n")

class UniConvScript(object):
    """Converts the script to Unicode, setting the properties required by
    EParse to report a meaningful error position.
    """
    def __init__(self, script, options=(), filename=b'<stdin>', emap=False):
        self.linedir = []
        self.filename = filename
        self.emap = emap
        # We don't interpret #line here. In case of an encode error,
        # we're in the dark about which file it comes from. User needs
        # --preshow to view the #line directives and find the correspondence
        # themselves.
        #self.processpre = 'processpre' in options
        self.processpre = False
        self.script = script

    def to_unicode(self):
        if type(self.script) is not unicode:
            try:
                self.script = self.script.decode('utf8')
            except UnicodeDecodeError as e:
                # EParse requires str
                self.script = b2str(self.script, 'utf8')
                self.errorpos = e.start
                raise EParse(self, u"Invalid UTF-8 in script")
        return self.script

def PreparePreproc(script):
    """LSL accepts multiline strings, but the preprocessor doesn't.
    Fix that by converting newlines to "\n". But in order to report accurate
    line and column numbers for text past that point, insert blank lines to
    fill the space previously occupied by the string, and spaces in the last
    line up to the point where the string was closed. That will place the next
    token in the same line and column it previously was.
    """
    s = u''
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
    tok = re.compile(str2u(  # Python 3.5 does not recognize ur'...' literals
        r'(?:'
            r'/(?:\?\?/\n|\\\n)*\*.*?\*(?:\?\?/\n|\\\n)*/'
            r'|/(?:\?\?/\n|\\\n)*/(?:\?\?/\n|\\\n|[^\n])*\n'
            r'|[^"]'
        r')+'
        r'|"'
        , 'utf8'), re.S)
    # RE used inside strings.
    tok2 = re.compile(str2u(
        r'(?:'
            r"\?\?[='()!<>-]"  # valid trigraph except ??/ (backslash)
            r"|(?:\?\?/|\\)(?:\?\?[/='()!<>-]|[^\n])"
                               # backslash trigraph or actual backslash,
                               # followed by any trigraph or non-newline
            r'|(?!\?\?/\n|\\\n|"|\n).'
                               # any character that doesn't start a trigraph/
                               # backslash escape followed by a newline
                               # or is a newline or double quote, as we're
                               # interested in all those individually.
        r')'                   # as many of those as possible
        r'|\?\?/\n|\\\n|\n|"'  # or any of those individually
        , 'utf8'))

    pos = 0
    match = tok.search(script, pos)
    while match:
        matched = match.group(0)
        pos += len(matched)
        if matched == u'"':
            s += matched
            nlines = col = 0
            match2 = tok2.search(script, pos)
            while match2:
                matched2 = match2.group(0)
                pos += len(matched2)

                if matched2 == u'\\\n' or matched2 == u'??/\n':
                    nlines += 1
                    col = 0
                    match2 = tok2.search(script, pos)
                    continue
                if matched2 == u'"':
                    if nlines:
                        if script[pos:pos+1] == u'\n':
                            col = -1 # don't add spaces if not necessary
                        # col misses the quote added here, so add 1
                        s += u'"' + u'\n'*nlines + u' '*(col+1)
                    else:
                        s += u'"'
                    break
                if matched2 == u'\n':
                    nlines += 1
                    col = 0
                    s += u'\\n'
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
        # + re.sub(r'([*/])(?=[*|/])', r'\1|', script) # FS's algorithm
        # HACK: This won't break strings containing ** or /* or // like URLs,
        # while still being compatible with FS.
        + re.sub(r'([*/]\||\*(?=/))', r'\1|', script)
        + '*/\n//end_unprocessed_text\n//nfo_preprocessor_version 0\n'
          '//program_version LSL PyOptimizer v' + VERSION
        + avname + '\n//mono\n\n')

def Usage(progname, about = None):
    if about is None:
        werr(
u"""LSL optimizer v{version}

    (C) Copyright 2015-2023 Sei Lisa. All rights reserved.

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
    [-b|--builtins=<filename>]  use a builtins file other than builtins.txt
    [-L|--libdata=<filename>]   use a function data file other than fndata.txt
    [-H|--header]               add the script as a comment in Firestorm format
    [-T|--timestamp]            add a timestamp as a comment at the beginning
    [-y|--python-exceptions]    when an exception is raised, show a stack trace
    [-p|--preproc=mode]         run external preprocessor (see below for modes)
                                (resets the preprocessor command line so far)
    [-P|--prearg=<arg>]         add parameter to preprocessor's command line
                                before predefined ones
    [-A|--postarg=<arg>]        add parameter to preprocessor's command line
                                after predefined ones
    [--precmd=<cmd>]            preprocessor command ('cpp' by default)
    [--prenodef]                no LSL specific defines (__AGENTKEY__ etc.)
    [--preshow]                 show preprocessor output, and stop
    [--preproc-show-cmdline]    show preprocessor invocation line, and stop
    [--avid=<UUID>]           * specify UUID of avatar saving the script
    [--avname=<name>]         * specify name of avatar saving the script
    [--assetid=<UUID>]        * specify the asset UUID of the script
    [--shortname=<name>]      * specify the script's short file name
    [--prettify]                Prettify source file. Disables all -O options.
    [--bom]                     Prefix script with a UTF-8 byte-order mark
    [--emap]                    Output error messages in a format suitable for
                                automated processing
    filename                    input file

Options marked with * are used to define the preprocessor macros __AGENTID__,
__AGENTKEY__, __AGENTIDRAW__, __AGENTNAME__, __ASSETID__ and __SHORTFILE__,
and have no effect if --prenodef is specified.

If filename is a dash (-) then standard input is used.
Use: {progname} -O help for help on the optimizer control options.

Comments are always removed in the output, even when using --prettify.

Preprocessor modes:
    ext       Invoke a preprocessor with no default parameters
    mcpp      Invoke mcpp as preprocessor, setting default parameters pertinent
              to it. Implies --precmd=mcpp
    gcpp      Invoke GNU cpp as preprocessor, setting default parameters
              pertinent to it. Implies --precmd=cpp
    none      No preprocessing (default)

Normally, running the preprocessor needs the option 'processpre' active, to
make the output readable by the optimizer. This option is active by default.
""".format(progname=str2u(progname), version=str2u(VERSION)))
        return

    if about == 'optimizer-options':
        werr(
u"""
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
                       something completely unexpected (under LSO: only the
                       last jump will execute, and it will go to the last label
                       with that name). This flag works around that limitation
                       by replacing the names of the labels in the output with
                       unique ones.
  Inline             - Enable 'inline' keyword to force functions to be inlined
                       (EXPERIMENTAL)

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
                       statement is missing. Only meaningful with EnableSwitch.
  FuncOverride       - Allow duplicate function definitions to override the
                       previous definition. For compatibility with Firestorm's
                       optimizer.

  Optimization options

  Optimize           + Runs the optimizer.
  OptSigns           + Optimize signs in float and integer constants.
  OptFloats          + Optimize floats that represent an integral value.
  ConstFold          + Fold constant expressions to their values, and simplify
                       some expressions and statements.
  IfElseSwap         + In 'if' statements, when negating the condition produces
                       shorter code, do it and swap the if and else branches.
                       Requires ConstFold to be enabled.
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
  ListLength         + Optimize llGetListLength(arg) to arg!=[]. Needs constant
                       folding active to work.
  ListAdd            + Convert [a,b,c...] to (list)a + b + c... if possible.

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
""".format(progname=str2u(progname)))
        return

validoptions = frozenset({'extendedglobalexpr','breakcont','extendedtypecast',
    'extendedassignment','allowkeyconcat','allowmultistrings','duplabels',
    'lazylists','enableswitch','errmissingdefault','funcoverride','optimize',
    'optsigns','optfloats','constfold','ifelseswap','dcr','shrinknames',
    'addstrings','foldtabs','warntabs','processpre','explicitcast',
    'listlength','listadd','inline','help',
    # undocumented
    'lso','expr','rsrclimit',
    # 'clear' is handled as a special case
    # 'prettify' is internal, as it's a user flag
})

def main(argv):
    """Main executable."""

    # If it's good to append the basename to it, it's good to append the
    # auxiliary files' names to it, which should be located where this file is.
    lslopt.lslcommon.DataPath = __file__[:-len(os.path.basename(__file__))]

    # Default options
    options = set(('extendedglobalexpr','extendedtypecast',
        'extendedassignment','allowkeyconcat','allowmultistrings','processpre',
        'warntabs','optimize','optsigns','optfloats','constfold','ifelseswap',
        'dcr','errmissingdefault','listlength','listadd',
        ))

    assert not (options - validoptions), (u"Default options not present in"
        u" validoptions: '%s'"
        % (b"', '".join(options - validoptions)).decode('utf8'))

    try:
        opts, args = getopt.gnu_getopt(argv[1:], 'hO:o:p:P:HTyb:L:A:',
            ('optimizer-options=', 'help', 'version', 'output=', 'header',
            'timestamp', 'python-exceptions', 'prettify', 'bom', 'emap',
            'preproc=', 'precmd=', 'prearg=', 'prenodef', 'preshow',
            'avid=', 'avname=', 'assetid=', 'shortname=', 'builtins=',
            'libdata=', 'postarg=', 'preproc-show-cmdline'))
    except getopt.GetoptError as e:
        Usage(argv[0])
        werr(u"\nError: %s\n" % str2u(str(e), 'utf8'))
        return 1

    outfile = '-'
    avid = '00000000-0000-0000-0000-000000000000'
    avname = ''
    shortname = ''
    assetid = '00000000-0000-0000-0000-000000000000'
    preproc_command = 'cpp'
    preproc_user_preargs = []
    preproc_user_postargs = []
    preproc = 'none'
    predefines = True
    script_header = ''
    script_timestamp = ''
    preshow = False
    preproc_show_cmdline = False
    raise_exception = False
    prettify = False
    bom = False
    emap = False
    builtins = None
    libdata = None

    for opt, arg in opts:
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
                    werr(u"\nError: Unrecognized"
                         u" optimizer option: %s\n" % str2u(chg, 'utf8'))
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
            wout(u'LSL PyOptimizer version %s\n' % str2u(VERSION))
            return 0

        elif opt in ('-o', '--output'):
            outfile = arg

        elif opt in ('-b', '--builtins'):
            builtins = arg

        elif opt in ('-L', '--libdata'):
            libdata = arg

        elif opt in ('-y', '--python-exceptions'):
            raise_exception = True

        elif opt in ('-p', '--preproc'):
            preproc = arg.lower()
            supported = ('ext', 'mcpp', 'gcpp', 'none')
            if preproc not in supported:
                Usage(argv[0])
                werr(u"\nUnknown --preproc option: '%s'."
                    u" Only '%s' supported.\n"
                    % (preproc, str2u("', '".join(supported))))
                return 1

            if preproc == 'gcpp':
                preproc_command = 'cpp'

            elif preproc == 'mcpp':
                preproc_command = 'mcpp'

        elif opt == '--precmd':
            preproc_command = arg

        elif opt in ('-P', '--prearg'):
            preproc_user_preargs.append(arg)

        elif opt in ('-A', '--postarg'):
            preproc_user_postargs.append(arg)

        elif opt == '--prenodef':
            predefines = False

        elif opt == '--preshow':
            preshow = True

        elif opt == '--preproc-show-cmdline':
            preproc_show_cmdline = True

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

        elif opt == '--prettify':
            prettify = True

        elif opt == '--bom':
            bom = True

        elif opt == '--emap':
            emap = True

    del opts

    if prettify:
        options &= set(('rsrclimit',))
        options.add('prettify')

    rsrclimit = False
    try:

        if 'rsrclimit' in options:
            rsrclimit = True
            import resource
            resource.setrlimit(resource.RLIMIT_CPU, (5, 8))
            resource.setrlimit(resource.RLIMIT_STACK, (0x60000, 0x80000))
            resource.setrlimit(resource.RLIMIT_DATA, (9001000, 12001000))
            resource.setrlimit(resource.RLIMIT_AS, (61001000, 81001000))

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
            werr(u"\nError: Input file not specified. Use -"
                u" if you want to use stdin.\n")
            return 1

        del args

        if not preproc_show_cmdline:
            script = ''
            if fname == '-':
                script = sys.stdin.read()
            else:
                try:
                    f = open(fname, 'r')
                except IOError as e:
                    if e.errno == 2:
                        werr(u"Error: File not found: %s\n" % str2u(fname))
                        return 2
                    raise
                try:
                    script = f.read()
                finally:
                    f.close()
                    del f

            # Transform to str and check Unicode validity
            if type(script) is unicode:
                script = u2str(script, 'utf8')
            else:
                try:
                    # Try converting the script to Unicode, to report any encoding
                    # errors with accurate line information.
                    tmp = UniConvScript(script, options,
                                        fname if fname != '-' else '<stdin>',
                                        emap).to_unicode()
                    # For Python 2, just report any errors and ignore the result.
                    # For Python 3, use the Unicode.
                    if python3:
                        script = tmp
                    del tmp
                except EParse as e:
                    # We don't call ReportError to prevent problems due to
                    # displaying invalid UTF-8
                    werr(e.args[0] + u"\n")
                    return 1
            # Now script is in native str format.

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

        # Build preprocessor command line
        preproc_cmdline = [preproc_command] + preproc_user_preargs
        if preproc == 'gcpp':
            preproc_cmdline += [
                '-undef', '-x', 'c', '-std=c99', '-nostdinc',
                '-trigraphs', '-dN', '-fno-extended-identifiers',
                ]

        elif preproc == 'mcpp':
            preproc_cmdline += [
                '-e', 'UTF-8', '-I-', '-N', '-2', '-3', '-j',
                '-V199901L',
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
            preproc_cmdline.append('-D__AGENTKEY__="%s"' % avid)
            preproc_cmdline.append('-D__AGENTID__="%s"' % avid)
            preproc_cmdline.append('-D__AGENTIDRAW__=' + avid)
            preproc_cmdline.append('-D__AGENTNAME__="%s"' % avname)
            preproc_cmdline.append('-D__ASSETID__=' + assetid)
            preproc_cmdline.append('-D__SHORTFILE__="%s"' % shortname)
            preproc_cmdline.append('-D__OPTIMIZER__=LSL-PyOptimizer')
            preproc_cmdline.append('-D__OPTIMIZER_VERSION__=' + VERSION)

        # Append user arguments at the end to allow them to override defaults
        preproc_cmdline += preproc_user_postargs

        pperrors = False
        if preproc_show_cmdline:
            script = ' '.join(preproc_cmdline)
        elif preproc != 'none':
            # PreparePreproc uses and returns Unicode string encoding.
            script = u2b(PreparePreproc(any2u(script, 'utf8')), 'utf8')
            # At this point, for the preprocessor to work we need the script
            # as a byte array, not as unicode, but it should be UTF-8.
            if preproc == 'mcpp':
                # As a special treatment for mcpp, we force it to output its
                # macros so we can read if USE_xxx are defined. With GCC that
                # is achieved with -dN, but mcpp has no command line option.
                script += b'\n#pragma MCPP put_defines\n'

            # Invoke the external preprocessor
            import subprocess

            p = subprocess.Popen(preproc_cmdline, stdin=subprocess.PIPE,
                stdout=subprocess.PIPE)
            script = p.communicate(input=script)[0]
            status = p.wait()
            if status:
                return status
            del p, status

            script = any2str(script, 'utf8')

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

        if not preshow and not preproc_show_cmdline:

            if emap:
                options.add('emap')

            lib = lslopt.lslloadlib.LoadLibrary(builtins, libdata)
            p = parser(lib)
            assert type(script) == str
            try:
                ts = p.parse(script, options,
                             'stdin' if fname == '-' else fname)
            except EParse as e:
                ReportError(script, e)
                return 1
            del p, script

            opt = optimizer(lib)
            ts = opt.optimize(ts, options)
            del opt

            outs = outscript()
            script = script_header + script_timestamp + outs.output(ts, options)
            del outs, ts

        del script_header, script_timestamp

        if bom:
            if not script.startswith(b'\xEF\xBB\xBF'):
                script = b'\xEF\xBB\xBF' + script

        if outfile == '-':
            sys.stdout.write(script)
        else:
            outf = open(outfile, 'w')
            try:
                outf.write(script)
            finally:
                outf.close()

        if pperrors:
            sys.stderr.write(u"\n* Errors found during preprocessing\n")
            return 1

        return 0

    except Exception as e:
        if rsrclimit:
            # Raise the soft limits to hopefully prevent double exceptions
            resource.setrlimit(resource.RLIMIT_CPU, (8, 8))
            resource.setrlimit(resource.RLIMIT_STACK, (0x80000, 0x80000))
            resource.setrlimit(resource.RLIMIT_DATA, (12001000, 12001000))
            resource.setrlimit(resource.RLIMIT_AS, (81001000, 81001000))
        if raise_exception:
            raise
        werr(e.__class__.__name__ + u': ' + str(e) + u'\n')
        return 1

if __name__ == '__main__':
    ret = main(sys.argv)
    if ret:
        sys.exit(ret)
