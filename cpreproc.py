#    (C) Copyright 2015-2022 Sei Lisa. All rights reserved.
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

# Interface for Niall Douglas' and David M. Beazley's PCPP (a C preprocessor)

import sys, os

oldsyspath = sys.path
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                'pcpp'))
from pcpp import preprocessor, OutputDirective, Action
path = oldsyspath

DIRECTIVES_PASSED_THROUGH = {'warning', 'pragma', 'line'}

class Preproc(preprocessor.Preprocessor):
    def __init__(self, input, params=()):
        super(Preproc, self).__init__()
        self.auto_pragma_once_enabled = False
        for v in params:
            if v.startswith('-I'):
                self.add_path(v[2:])
            elif v.startswith('-D'):
                defn = v[2:]
                if '=' not in defn:
                    defn += '=1'
                if defn.startswith('='):
                    self.on_error("\nError: Empty macro name in definition.\n")
                    return
                defn = defn.replace('=', ' ', 1)
                self.define(defn)
            elif v.startswith('-U'):
                defn = v[2:]
                if defn in self.macros:
                    del self.macros[defn]
            else:
                self.on_error("\nError: Option for the internal"
                    " preprocessor not -D, -U or -I:\n    %s\n" % v)
                return

        self.ignore = set()
        self.parser = self.parsegen(input, '<stdin>', '<stdin>')
        self.errors_present = False

    def get(self):
        if self.errors_present:
            return True, '', {}

        try:
            import StringIO
        except ImportError:
            import io as StringIO
        ret = StringIO.StringIO()
        self.write(ret)
        return (self.errors_present, ret.getvalue(), self.macros)

    def on_error(self, *args, **kwargs):
        """Flag that errors are present when called."""
        self.errors_present = True
        return super(Preproc, self).on_error(*args, **kwargs)

    def on_include_not_found(self, is_malformed, is_system_include, curdir,
                             includepath):
        """Don't pass through the #include line if the file does not exist."""
        if is_malformed:
            self.on_error(self.lastdirective.source, self.lastdirective.lineno,
                "Malformed include file directive")
        else:
            self.on_error(self.lastdirective.source, self.lastdirective.lineno,
                "Include file not found: %s" % includepath)
        raise OutputDirective(Action.IgnoreAndRemove)

    def on_directive_unknown(self, directive, toks, ifpassthru, precedingtoks):
        """pcpp does not process #error/#warning/#pragma/#line; do it here."""
        if directive.value == 'error':
            self.on_error(directive.source, directive.lineno,
                "Error directive: \"%s\"" % ''.join(i.value for i in toks))
        elif directive.value not in DIRECTIVES_PASSED_THROUGH:
            self.on_error(directive.source, directive.lineno,
                "Unknown directive: \"%s\"" % directive.value)
