
import lslfuncs
from lslfuncs import Key, Vector, Quaternion

from lslfoldconst import foldconst
from lslrenamer import renamer
from lsldeadcode import deadcode

class optimizer(foldconst, renamer, deadcode):

    # Default values per type when declaring variables
    DefaultValues = {'integer': 0, 'float': 0.0, 'string': u'',
        'key': lslfuncs.Key(u''), 'vector': lslfuncs.ZERO_VECTOR,
        'rotation': lslfuncs.ZERO_ROTATION, 'list': []
        }

    # explicitly exclude assignments
    binary_ops = frozenset(('+','-','*','/','%','<<','>>','<','<=','>','>=',
        '==','!=','|','^','&','||','&&'))
    assign_ops = frozenset(('=','+=','-=','*=','/=','%=','&=','|=','^=','<<=','>>='))

    LSL2PythonType = {'integer':int, 'float':float, 'string':unicode, 'key':lslfuncs.Key,
        'vector':lslfuncs.Vector, 'rotation':lslfuncs.Quaternion, 'list':list}

    PythonType2LSL = {int: 'integer', float: 'float',
        unicode: 'string', Key: 'key', Vector: 'vector',
        Quaternion: 'rotation', list: 'list'}

    def Cast(self, value, newtype):
        # Return a CAST node if the types are not equal, otherwise the
        # value unchanged
        if value['t'] == newtype:
            return value
        if value not in ('CONST','()','FLD','IDENT','FNCALL','V++','V--',
                         'VECTOR','ROTATION','LIST'):
            value = {'nt':'()', 't':newtype, 'ch':[value]}
            if 'SEF' in value['ch'][0]:
                value['SEF'] = True
            if 'X' in value['ch'][0]:
                value['X'] = value['ch'][0]['X']
        ret = {'nt':'CAST', 't':newtype, 'ch':[value]}
        if 'SEF' in value:
            ret['SEF'] = True
        if 'X' in value:
            ret['X'] = value['X']
        return ret

    def optimize(self, treesymtab, options = ('optimize','constfold','dcr')):
        """Optimize the symbolic table symtab in place. Requires a table of
        predefined functions for folding constants.
        """
        if 'optimize' not in options:
            return treesymtab

        self.foldtabs = 'foldtabs' in options

        self.shrinknames = 'shrinknames' in options

        self.constfold = 'constfold' in options
        self.dcr = 'dcr' in options

        tree, symtab = self.tree, self.symtab = treesymtab

        self.globalmode = False

        if self.constfold:
            self.FoldScript()

        if self.dcr:
            self.RemoveDeadCode()

        # Make another fold pass, since RemoveDeadCode can embed expressions
        # into other expressions and generate unoptimized code.
        if self.constfold:
            self.FoldScript()

        if self.shrinknames:
            self.ShrinkNames()

        treesymtab = (self.tree, self.symtab)
        del self.tree
        del self.symtab
        return treesymtab
