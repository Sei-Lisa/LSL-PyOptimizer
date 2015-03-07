#    (C) Copyright 2015 Sei Lisa. All rights reserved.
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

# This module renames all kinds of variables. Globals and function/event
# parameters take memory space, so shrinking the identifiers as much as
# possible ensures their memory usage will be minimized. It also reuses some
# preexisting names when possible. Locals are renamed also so that they don't
# stand in the way of globals.
#
# A side effect of this change is that the script becomes unreadable gibberish.

# TODO: Rename locals to loc_<identifier> rather than random names.

class renamer(object):
    CharSet1 = '_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    CharSet2 = '0123456789_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    # TODO: Derive these from builtins.txt somehow.
    KwByLen = ((), (), ('do', 'if', 'PI'), ('for', 'key', 'EOF'),
        ('jump', 'else', 'list', 'TRUE', 'LOOP', 'case'))
    def GetNextShortest(self):
        """Generate the next shortest possible identifier"""
        while True:
            ret = self.CharSet1[self.WordFirstChar]
            for idx in self.WordRestOfChars:
                ret += self.CharSet2[idx]
            self.WordFirstChar += 1
            if self.WordFirstChar >= len(self.CharSet1):
                self.WordFirstChar = 0
                for idx in xrange(len(self.WordRestOfChars)):
                    if self.WordRestOfChars[idx] < len(self.CharSet2)-1:
                        self.WordRestOfChars[idx] += 1
                        break
                    self.WordRestOfChars[idx] = 0
                else:
                    self.WordRestOfChars.append(0)

            if ret not in self.KwByLen[len(ret)] and ret not in self.UsedNames:
                return ret

    def ShrinkNames(self):
        """Implements the shrinknames option."""
        self.WordFirstChar = 0
        self.WordRestOfChars = []

        # Names that can be reused without penalty. The initial set is there
        # since the beginning. Others are created depending on the code
        # (e.g. Key when there are keys), but we don't take too many risks.
        ReusableNames = set(['LslLibrary', 'LslUserScript', 'System'])

        # Names from ReusableNames that have already been used
        self.UsedNames = set()

        # Make a first pass to separate the symbols into three categories.
        globalvars = []
        states = []
        functions = []
        globaldefs = self.symtab[0]
        for name in globaldefs:
            if name == -1: continue
            kind = globaldefs[name]['Kind']
            if kind == 's':
                states.append(name)
            elif kind == 'f':
                if 'Loc' in globaldefs[name]:
                    functions.append(name)
            elif kind == 'v':
                globalvars.append(name)
            else:
                assert False, 'Invalid kind at this scope: ' \
                    + kind # pragma: no cover

        # We make three passes, one for states, then functions, then globals,
        # in that order.

        for name in states:
            # States have top priority. Here's why. An internal event function
            # name is made by concatenating an 'e', then the state name, then
            # the event name, e.g. edefaultstate_entry. Since a new identifier
            # having part of the state name is created for every event in that
            # state, the shortest the state name, the least bytes it will use.
            # Furthermore, a state switch instruction further adds a Unicode
            # string (all other identifier names use one-byte strings), which
            # is the more reason to shorten it as much as possible.
            #
            # Unfortunately, there isn't much that can be done about 'default'.
            #
            # The good side is that we get to reuse these names for variables
            # without using extra space and without wasting single or double
            # letter identifiers.

            entry = globaldefs[name]
            if name != 'default':
                name = entry['NewName'] = self.GetNextShortest()
            # Find also the event names it uses, to add them for reuse.
            for node in self.tree[entry['Loc']]['ch']:
                assert node['nt'] == 'FNDEF'
                ReusableNames.add('e' + name + node['name'])
        del states

        for name in functions:
            # Assign a new name. Internally, function names get a 'g' prepended
            # to them, so these are candidates for reuse too.

            # Unfortunately, we won't find any reusable name starting with 'g'
            # this early, so no point in searching.

            short = globaldefs[name]['NewName'] = self.GetNextShortest()
            ReusableNames.add('g' + short)
        del functions

        for name in globalvars:
            # First, check if we have reusable names available.
            if ReusableNames:
                short = ReusableNames.pop()
                self.UsedNames.add(short)
            else:
                short = self.GetNextShortest()
            globaldefs[name]['NewName'] = short

        # Do the same for function and event parameter names. Pure locals get
        # long distinct names.
        First = True
        restart = self.WordFirstChar
        for table in self.symtab:
            if First:
                First = False
                # Skip globals
                continue
            InParams = False
            for name,sym in table.iteritems():
                if name == -1: continue
                if sym['Kind'] != 'v':
                    assert sym['Kind'] == 'l'
                    name = name # trick python to not optimize out the jump
                    continue
                if 'Param' in sym:
                    if not InParams:
                        # Restart at every new parameter table.
                        # Parameter tables are isolated from each other.
                        InParams = True
                        self.WordFirstChar = restart
                    # Same procedure as for global vars
                    # Not the best strategy (using locally unique names would
                    # do a better job) but hey.
                    if ReusableNames:
                        short = ReusableNames.pop()
                        self.UsedNames.add(short)
                    else:
                        short = self.GetNextShortest()
                    table[name]['NewName'] = short
                else:
                    # Generate new identifier, by prepending the four character
                    # string 'loc_'. This generates identifiers with five chars
                    # or more. We assume that is enough for them to stay safe
                    # from name collisions with globals and parameter names.
                    # Four letters allow for more than 1.4 million identifiers.
                    unique = 'loc_' + name
                    table[name]['NewName'] = unique

        del globalvars
