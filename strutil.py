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

# String <-> Bytes conversion and output utilities

import sys
if sys.hexversion >= 0x3000000:
    unicode = str
    unichr = chr
    def str2u(s, enc=None):
        """Convert a native Python3 str to Unicode. This is a NOP."""
        return s

    def str2b(s, enc=None):
        """Convert a native Python3 str to bytes, with the given encoding."""
        return s.encode(enc if type(enc) == str
                        else getattr(enc, 'encoding', 'utf8'),
                        'backslashreplace')

    def u2str(s, enc=None):
        """Convert a Unicode string to native Python 3 str. This is a NOP."""
        return s

    def b2str(s, enc=None):
        """Convert a Bytes string to native Python 3 str."""
        return s.decode(getattr(enc, 'encoding', enc) or 'utf8',
                        'backslashreplace')

else:
    def str2u(s, enc=None):
        """Convert a native Python2 str to Unicode."""
        return s.decode(getattr(enc, 'encoding', enc) or 'utf8',
                        'backslashreplace')

    def str2b(s, enc=None):
        """Convert a native Python2 str to bytes. This is a NOP."""
        return s

    def u2str(s, enc=None):
        """Convert a Unicode string to native Python 2 str."""
        return s.encode(enc if type(enc) == str
                        else getattr(enc, 'encoding', 'utf8'),
                        'backslashreplace')

    def b2str(s, enc=None):
        """Convert a Bytes string to native Python 2 str. This is a NOP."""
        return s

def b2u(s, enc=None):
    """Bytes to Unicode"""
    return str2u(b2str(s, enc), enc)

def u2b(s, enc=None):
    """Unicode to Bytes"""
    return u2str(str2b(s, enc), enc)

def any2b(s, enc=None):
    """Bytes or Unicode to Bytes"""
    return s if type(s) == bytes else u2b(s, enc)

def any2u(s, enc=None):
    """Bytes or Unicode to Unicode"""
    return s if type(s) == unicode else b2u(s, enc)

def werr(s):
    """Write any string to stderr"""
    sys.stderr.write(any2u(s, sys.stderr))

def wout(s):
    """Write any string to stdout"""
    sys.stdout.write(any2u(s, sys.stdout))
