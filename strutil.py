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

# String <-> Bytes conversion and output utilities

# Microsoft again not following standards. Sigh.
import codecs
codecs.register(lambda x: codecs.lookup('utf8') if x == 'cp65001' else None)

import sys
if sys.version_info.major >= 3:
    unicode = str
    unichr = chr
    xrange = range
    python3 = True
    python2 = False

    def str2u(s, enc=None):
        """Convert a native Python3 str to Unicode. This is a NOP."""
        return s

    def str2b(s, enc=None):
        """Convert a native Python3 str to bytes, with the given encoding."""
        return s.encode(getattr(enc, 'encoding', enc) or 'utf8',
                        'backslashreplace')

    def u2str(s, enc=None):
        """Convert a Unicode string to native Python 3 str. This is a NOP."""
        return s

    def b2str(s, enc=None):
        """Convert a Bytes string to native Python 3 str."""
        return s.decode(getattr(enc, 'encoding', enc) or 'utf8',
                        'replace')

    def any2str(s, enc=None):
        """Convert Bytes or Unicode to native Python 3 str."""
        return s if type(s) == str else b2str(s, enc)

else:
    unicode = unicode
    unichr = unichr
    xrange = xrange
    python2 = True
    python3 = False

    def str2u(s, enc=None):
        """Convert a native Python2 str to Unicode."""
        return s.decode(getattr(enc, 'encoding', enc) or 'utf8',
                        'replace')

    def str2b(s, enc=None):
        """Convert a native Python2 str to bytes. This is a NOP."""
        return s

    def u2str(s, enc=None):
        """Convert a Unicode string to native Python 2 str."""
        return s.encode(getattr(enc, 'encoding', enc) or 'utf8',
                        'backslashreplace')

    def b2str(s, enc=None):
        """Convert a Bytes string to native Python 2 str. This is a NOP."""
        return s

    def any2str(s, enc=None):
        """Convert Bytes or Unicode to native Python 2 str."""
        return s if type(s) == str else u2str(s, enc)


def b2u(s, enc=None):
    """Bytes to Unicode"""
    return str2u(b2str(s, enc), enc)

def u2b(s, enc=None):
    """Unicode to Bytes"""
    return str2b(u2str(s, enc), enc)

def any2b(s, enc=None):
    """Bytes or Unicode to Bytes"""
    return s if type(s) == bytes else u2b(s, enc)

def any2u(s, enc=None):
    """Bytes or Unicode to Unicode"""
    return s if type(s) == unicode else b2u(s, enc)

def werr(s):
    """Write any string to stderr"""
    sys.stderr.write(any2str(s, sys.stderr))

def wout(s):
    """Write any string to stdout"""
    sys.stdout.write(any2str(s, sys.stdout))

strutil_used = True
