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

# Expand inlined functions. This could perhaps be made at parse time, but that
# would obfuscate the source too much.

from lslcommon import nr

# Statement-level nodes that have at most 1 child and is of type expression
SINGLE_OPT_EXPR_CHILD_NODES = frozenset({'DECL', 'EXPR', 'RETURN',
    '@', 'STSW', 'JUMP', ';', 'LAMBDA'})

# TODO: We can do a bit better with evaluation order.

class ENameAlreadyExists(Exception):
    def __init__(self, obj, text):
        super(ENameAlreadyExists, self).__init__(text)

class EExpansionLoop(Exception):
    def __init__(self):
        super(EExpansionLoop, self).__init__(u"Loop found in expansion of"
            u" inline functions")

class inliner(object):
    def newId(self, namespace, scope, symdata):
        """Create a new identifier based on a namespace."""
        self.symCtr[namespace] = self.symCtr.get(namespace, 0) + 1
        name = '___%s__%05d' % (namespace, self.symCtr[namespace])
        if name in self.symtab[scope]:
            kinds = {'l':u"Label", 'f':u"Function", 'v':u"Variable",
                     's':u"State"}
            raise ENameAlreadyExists(self, u"%s already exists: %s"
              % (kinds[symdata['Kind']], name.decode('utf8')))
        self.symtab[scope][name] = symdata
        return name

    def newSymtab(self):
        self.symtab.append({})
        return len(self.symtab) - 1

    def FixJumps(self, node):
        """Change name and scope of JUMPs to point to the correct symtab entry
        """
        nt = node.nt
        if nt == 'JUMP':
            orig = self.symtab[node.scope][node.name]
            if 'NewSymbolName' in orig:
                node.name = orig['NewSymbolName']
                node.scope = orig['NewSymbolScope']
                self.symtab[node.scope][node.name]['ref'] += 1
            return

        if nt in SINGLE_OPT_EXPR_CHILD_NODES:
            return

        if nt == '{}':
            for i in node.ch:
                self.FixJumps(i)
            return

        if nt == 'IF' or nt == 'WHILE':
            self.FixJumps(node.ch[1])
            if len(node.ch) > 2:
                self.FixJumps(node.ch[2])
            return

        if nt == 'DO':
            self.FixJumps(node.ch[0])
            return

        if nt == 'FOR':
            self.FixJumps(node.ch[3])
            return

        assert False, u"Unexpected node type: %s" % nt.decode('utf8')

    def GetFuncCopy(self, node, scope=0):
        """Get a copy of the function's body

        Replaces 'return expr' with assignment+jump, 'return' with jump, and
        existing labels with fresh labels. Also creates new symtabs for locals
        and adjusts scopes of symbols.
        """
        nt = node.nt
        if nt == 'FNDEF':
            # We're at the top level. Check return type and create a label,
            # then recurse into the block.
            assert node.ch[0].nt == '{}'
            copy = self.GetFuncCopy(node.ch[0], node.ch[0].scope)
            assert copy.nt == '{}'
            self.FixJumps(copy)
            return copy

        if nt == '{}':
            copy = node.copy()
            copy.scope = self.newSymtab()
            copy.ch = []
            for i in node.ch:
                copy.ch.append(self.GetFuncCopy(i, node.scope))
                if i.nt == 'DECL':
                    self.symtab[copy.scope][i.name] = \
                        self.symtab[i.scope][i.name].copy()
                    self.symtab[node.scope][i.name]['NewSymbolScope'] = \
                        copy.scope
                    copy.ch[-1].scope = copy.scope
            return copy

        if nt == '@':
            copy = node.copy()
            oldscope = node.scope
            oldname = node.name
            copy.name = self.newId('lbl', scope, {'Type':'l', 'Scope':scope,
                                                  'ref':0})
            copy.scope = scope
            self.symtab[oldscope][oldname]['NewSymbolName'] = copy.name
            self.symtab[oldscope][oldname]['NewSymbolScope'] = scope
            return copy

        if nt == 'RETURN':
            newnode = nr(nt='JUMP', t=None, name=self.retlabel,
                scope=self.retlscope)
            self.symtab[self.retlscope][self.retlabel]['ref'] += 1
            if node.ch:
                # Returns a value. Wrap in {} and add an assignment.
                # BUG: We don't honour ExplicitCast here.
                newnode = nr(nt='{}', t=None, scope=self.newSymtab(), ch=[
                    nr(nt='EXPR', t=self.rettype, ch=[
                        nr(nt='=', t=self.rettype, ch=[
                            nr(nt='IDENT', t=node.ch[0].t,
                               name=self.retvar, scope=self.retscope)
                            , self.GetFuncCopy(node.ch[0])
                        ])
                    ]), newnode
                ])
            return newnode

        if nt == 'IDENT':
            copy = node.copy()
            if 'NewSymbolScope' in self.symtab[node.scope][node.name]:
                copy.scope = \
                    self.symtab[node.scope][node.name]['NewSymbolScope']
            return copy

        if not node.ch:
            return node.copy()

        copy = node.copy()
        copy.ch = []
        for i in node.ch:
            copy.ch.append(self.GetFuncCopy(i, scope))
        return copy


    def ConvertFunction(self, parent, index, scope):
        node = parent[index]
        fns = []
        for i in range(len(node.ch)):
            fns.extend(self.RecurseExpression(node.ch, i, scope))
        fnsym = self.symtab[0][node.name]
        rettype = fnsym['Type']
        self.rettype = rettype
        retvar = None
        if rettype is not None:
            # Returns a value. Create a local variable at the starting level.
            retvar = self.newId('ret', scope, {'Kind':'v', 'Scope':scope,
                                               'Type':rettype})
            # Add the declaration to the list of statements
            fns.append(nr(nt='DECL', t=rettype, name=retvar, scope=scope))

        # Begin expansion
        if node.name in self.expanding:
            raise EExpansionLoop()

        outer = None
        if fnsym['ParamNames']:
            # Add a new block + symbols + assignments for parameter values
            pscope = self.newSymtab()
            outer = nr(nt='{}', t=None, scope=pscope, ch=[])
            origpscope = self.tree[fnsym['Loc']].pscope
            for i in range(len(fnsym['ParamNames'])):
                # Add parameter assignments and symbol table entries
                pname = fnsym['ParamNames'][i]
                ptype = fnsym['ParamTypes'][i]
                value = node.ch[i]
                self.symtab[pscope][pname] = {'Kind':'v','Type':ptype,
                                              'Scope':pscope}
                self.symtab[origpscope][pname]['NewSymbolScope'] = pscope
                # BUG: We don't honour ExplicitCast here.
                outer.ch.append(nr(nt='DECL', t=ptype, name=pname, scope=pscope,
                                   ch=[value]))

        self.expanding.append(node.name)
        self.retvar = retvar
        self.retscope = scope
        self.retlscope = scope
        retlabel = self.newId('rtl', scope, {'Type':'l', 'Scope':scope,
                                             'ref':0})
        self.retlabel = retlabel

        # Get a copy of the function
        blk = [self.GetFuncCopy(self.tree[fnsym['Loc']])]
        if outer:
            outer.ch.extend(blk)
            blk = [outer]
        retused = self.symtab[scope][retlabel]['ref'] != 0

        self.RecurseStatement(blk, 0, scope)  # recursively expand functions

        # Add return label if used, otherwise remove it from the symbol table
        if retused:
            blk.append(nr(nt='@', name=retlabel, scope=scope))
        else:
            del self.symtab[scope][retlabel]
        self.expanding.pop()
        # End expansion

        fns.extend(blk)

        if rettype is None:
            del parent[index]
        else:
            parent[index] = nr(nt='IDENT', t=rettype, name=retvar, scope=scope)

        return fns

    def RecurseExpression(self, parent, index, scope):
        node = parent[index]
        nt = node.nt
        fns = []

        if nt == 'FNCALL' and self.symtab[0][node.name].get('Inline', False):
            fns.extend(self.ConvertFunction(parent, index, scope))
        elif node.ch:
            for i in range(len(node.ch)):
                fns.extend(self.RecurseExpression(node.ch, i, scope))
        return fns

    def RecurseSingleStatement(self, parent, index, scope):
        # Synthesize a block node whose child is the statement.
        newscope = self.newSymtab()
        node = nr(nt='{}', t=None, scope=newscope, ch=[parent[index]],
            SEF=parent[index].SEF)

        # Recurse into that node, so that any additions are made right there.
        self.RecurseStatement(node.ch, 0, newscope)

        # If it's no longer a single statement, promote it to a block.
        if len(node.ch) != 1:
            parent[index] = node
        else:
            # The new level won't be necessary. We can't delete the symbol
            # table, though, because that shifts any new symbol tables.
            assert not self.symtab[newscope]
            parent[index] = node.ch[0]

    def RecurseStatement(self, parent, index, scope):
        node = parent[index]
        nt = node.nt
        child = node.ch
        fns = None

        if nt in SINGLE_OPT_EXPR_CHILD_NODES:
            if child:
                fns = self.RecurseExpression(child, 0, scope)
                if nt == 'EXPR' and not node.ch:
                    del parent[index]
            else:
                return

        elif nt == '{}':
            i = -len(child)
            while i:
                self.RecurseStatement(child, len(child)+i, node.scope)
                i += 1

        elif nt == 'IF':
            fns = self.RecurseExpression(child, 0, scope)
            self.RecurseSingleStatement(child, 1, scope)
            if len(child) > 2:
                self.RecurseSingleStatement(child, 2, scope)

        # Loop handling is tricky.
        #
        # Consider this:
        #
        #   integer f()
        #   {
        #       llOwnerSay("body of f");
        #       return 1;
        #   }
        #
        #   while (f())
        #       llOwnerSay("doing stuff");
        #
        # In order to execute it every loop iteration, the while() loop must be
        # converted to an if+jump:
        #
        #   integer ___ret__00001;
        #   @___lbl__00001;
        #   {
        #       llOwnerSay("body_of_f");
        #       __ret__00001 = 1;
        #   }
        #   if (___ret__00001)
        #   {
        #       llOwnerSay("doing stuff");
        #       jump ___lbl__00001;
        #   }
        #
        # The for loop is similar, but the initializer and iterator must be
        # expanded as well, to convert it to a while loop. When expanding the
        # iterator, care must be taken to avoid name clashes. For example:
        #
        #   for (i = 0; f(i); i++)
        #   {
        #       integer i;
        #   }
        #
        # should be expanded to:
        #
        #   i = 0;
        #   @loop_label;
        #   integer ___ret__00001;
        #   {
        #       llOwnerSay("body of f");
        #       ___ret__00001 = 1,
        #   }
        #   if (___ret__00001)
        #   {
        #       {
        #           integer i;
        #       }
        #       i++;
        #   }
        #
        # The extra {} inside the 'if' are needed to protect the iterator from
        # being affected by the variables defined in the inner block.
        #
        # Do loops are different:
        #
        #   do
        #       llOwnerSay("doing stuff");
        #   while (f());
        #
        # is converted to:
        #
        #   integer ___ret__00001;
        #   do
        #   {
        #       llOwnerSay("doing stuff");
        #       {
        #           llOwnerSay("body_of_f");
        #           ___ret__00001 = 1;
        #       }
        #   }
        #   while (___ret__00001);
        #

        elif nt == 'DO':
            self.RecurseSingleStatement(child, 0, scope)
            fns = self.RecurseExpression(child, 1, scope)
            if fns:
                # Need to do some plumbing to move the bodies into the loop
                i = 0;
                while i < len(fns):
                    if fns[i].nt != 'DECL':
                        assert fns[i].nt in ('{}', '@')
                        # All bodies must be moved inside the loop
                        if child[0].nt != '{}':
                            # Needs wrapping now
                            child[0] = nr(nt='{}', t=None, ch=[child[0]],
                                          scope=self.newSymtab())
                        child[0].ch.append(fns.pop(i))
                    else:
                        i += 1

        elif nt == 'WHILE':
            # Convert to if()
            lbl = self.newId('whl', scope, {'Kind':'l','Scope':scope,'ref':1})
            parent.insert(index, nr(nt='@', t=None, name=lbl, scope=scope))
            index += 1
            node.nt = 'IF'
            if child[1].nt != '{}':
                # Needs wrapping now
                child[1] = nr(nt='{}', t=None, ch=[child[1]],
                              scope=self.newSymtab())
            child[1].ch.append(nr(nt='JUMP', t=None, name=lbl, scope=scope))
            fns = self.RecurseExpression(child, 0, scope)
            self.RecurseSingleStatement(child, 1, scope)

        elif nt == 'FOR':
            assert child[0].nt == 'EXPRLIST'
            assert child[2].nt == 'EXPRLIST'
            for i in child[0].ch:
                parent.insert(index, nr(nt='EXPR', t=i.t, ch=[i]))
                fns = self.RecurseExpression(parent, index, scope)
                parent[index:index] = fns
                index += 1 + len(fns)
            lbl = self.newId('for', scope, {'Kind':'l','Scope':scope,'ref':1})
            parent.insert(index, nr(nt='@', t=None, name=lbl, scope=scope))
            index += 1
            node.nt = 'IF'
            if child[3].nt != '{}':
                # Needs wrapping now
                child[3] = nr(nt='{}', t=None, ch=[child[3]],
                              scope=self.newSymtab())
            # Needs another wrapping if iterator is not empty
            if child[2].ch:
                child[3] = nr(nt='{}', t=None, ch=[child[3]],
                              scope=self.newSymtab())
            for i in child[2].ch:
                child[3].ch.append(nr(nt='EXPR', t=i.t, ch=[i]))
            del child[2]
            del child[0]
            child[1].ch.append(nr(nt='JUMP', t=None, name=lbl, scope=scope))
            fns.extend(self.RecurseExpression(child, 0, scope))
            self.RecurseSingleStatement(child, 1, scope)
            #assert False, parent[index]

        else:
            assert False, u"Unexpected node type: %s" % nt.decode('utf8')

        if fns:
            parent[index:index] = fns

    def inline(self, tree, symtab):
        self.tree = tree
        self.symtab = symtab
        self.symCtr = {}
        self.expanding = []
        for i in range(len(tree)):
            if tree[i].nt == 'STDEF':
                for j in range(len(tree[i].ch)):  # for each event in the state
                    self.RecurseStatement(tree[i].ch[j].ch, 0,
                                           tree[i].ch[j].ch[0].scope)
            elif (tree[i].nt == 'FNDEF'
                  and not symtab[tree[i].scope][tree[i].name].get('Inline',
                                                                  False)
                 ):
                # Must be an UDF
                self.RecurseStatement(tree[i].ch, 0, tree[i].ch[0].scope)

        # Remove all inline function definitions
        for i in range(len(tree)):
            if (tree[i].nt == 'FNDEF'
                  and symtab[tree[i].scope][tree[i].name].get('Inline', False)
               ):
                tree[i] = nr(nt='LAMBDA', t=None)
