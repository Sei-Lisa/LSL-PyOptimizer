from lslopt.lslparse import parser,EParseSyntax,EParseUEOF,EParseAlreadyDefined,\
    EParseUndefined,EParseTypeMismatch,EParseReturnShouldBeEmpty,EParseReturnIsEmpty,\
    EParseInvalidField,EParseFunctionMismatch,EParseDeclarationScope,EParseUnexpected,\
    fieldpos
from lslopt.lsloutput import outscript
from lslopt import lslfuncs
import unittest
import os

class UnitTestCase(unittest.TestCase):
    pass

class Test01LibraryLoader(UnitTestCase):
    def test_coverage(self):
        os.remove('builtins.txt')
        f = open('builtins.txt', 'wb')
        f.write(r'''const key a="\t"
event ev(integer i)
event ev(integer i)
quaternion x(integer i)
void x(integer i)
blah
const vector a = <4,5,3,2>
const vector a = <4,5,3,2
const vector a = <x,4,3>
const vector a = <4,x,3>
const vector a = <3,4,x>
const rotation a = <3,4,4,x>
const list l = []
const quaternion q=<1,2,3,4>
const string v="
const string q="\t"
''')
        f.close()
        parser()
        f = open('builtins.txt.dat', 'rb')
        b = f.read()
        f.close()
        os.remove('builtins.txt')
        f = open('builtins.txt', 'wb')
        f.write(b)
        f.close()
        parser()


class Test02Compiler(UnitTestCase):
    def setUp(self):
        self.parser = parser()
        self.outscript = outscript()

    def test_coverage(self):
        try:
            os.remove('overwritten.lsl')
        except OSError:
            pass
        f = open('overwritten.lsl', 'wb')
        f.write('/*Autogenerated*/default{timer(){}}')
        f.close()
        del f
        self.parser.parsefile('overwritten.lsl')
        self.outscript.output(self.parser.parse("""default{touch(integer n){jump n;@n;}}"""))
        self.assertRaises(EParseUndefined, self.parser.parse, """default{touch(integer n){jump k;n;}}""")
        self.outscript.output(self.parser.parse("""default{touch(integer n){n;}}"""))
        print self.outscript.output(self.parser.parse(r"""string x="";
            vector V=ZERO_VECTOR;
            vector W = <1,2,3>;
            quaternion Q = <1,2,3,4>;
            float f;
            float ff = f;
            list L = [];
            list L2 = [2,3,4,5,6];
            integer fn(integer x){
                if (1) for (f=3,f=4,f=5;3;f++,f++) do while(0); while(0); else if (2) return 2; else;
                fn(3);
                integer j = 3||4&&5|6^7&8.==9!=10.e+01f<11<=12>13.>=14<<15>>16== ++f+-f++;
                j *= 3.0; // LSL allows this
                1+((float)2+(integer)(1+1));
                12345678901;0x000000012345678901;0x000;
                2*(V*V/4)*V*--V.x*V.x++;
                L+L2;L+1;1+L;
                <0,0,0.>0>0>*<0,0,0==0>2,3>>3>3.>%<3,4,5>;
                f -= TRUE-(integer)-1;
                f *= !FALSE;
                V %= (ZERO_VECTOR+-ZERO_VECTOR)*(ZERO_ROTATION+-ZERO_ROTATION);
                1e37;1.1e22;1.;
                print(V *= 3);
                fwd("","","");
                L"\n\t\rxxxx";
                {f;}
                [1,2,3];
            }
            fwd(string a,string b,string c){}
            default{touch(integer n){n;state default;state another;return;}timer(){}}
            state another{timer(){}}//"""))
        self.assertRaises(EParseUEOF, self.parser.parse, '')
        self.assertRaises(EParseUEOF, self.parser.parse, 'default')
        self.assertRaises(EParseSyntax, self.parser.parse, 'x')
        self.outscript.output(self.parser.parse('integer x=TRUE;integer y=x;integer j=FALSE;default{timer(){}}'))
        self.assertRaises(EParseSyntax, self.parser.parse, ';')
        self.assertRaises(EParseSyntax, self.parser.parse, 'f(){}g(integer x,key y){{}}h(;){}')
        self.assertRaises(EParseSyntax, self.parser.parse, 'f(){}g(integer x,key y){}h()}')
        self.assertRaises(EParseUEOF, self.parser.parse, 'integer "')
        self.assertRaises(EParseSyntax, self.parser.parse, 'default{timer(){}}state blah{timer(){}}state ;')
        self.assertRaises(EParseSyntax, self.parser.parse, 'default{timer(integer x){}}')
        self.assertRaises(EParseSyntax, self.parser.parse, 'default{timer(integer x){(integer)x=0}}')
        self.assertRaises(EParseSyntax, self.parser.parse, 'default{timer(){state;}}')
        self.assertRaises(EParseAlreadyDefined, self.parser.parse, 'default{timer(integer x,integer x){}}')
        self.assertRaises(EParseSyntax, self.parser.parse, 'x;')
        self.assertRaises(EParseSyntax, self.parser.parse, '1e;')
        self.assertRaises(EParseSyntax, self.parser.parse, 'integer x=-TRUE;')
        self.assertRaises(EParseSyntax, self.parser.parse, 'integer x=-3;integer y=-x;')
        self.assertRaises(EParseAlreadyDefined, self.parser.parse, '''float x=3;float x;''')
        self.assertRaises(EParseAlreadyDefined, self.parser.parse, '''default{timer(){}}
            state blah{timer(){}}
            state blah{}''')
        self.assertRaises(EParseAlreadyDefined, self.parser.parse, '''default{timer(){@x;@x;}}''')
        self.assertRaises(EParseAlreadyDefined, self.parser.parse, '''default{timer(){integer x;@x;}}''')
        self.assertRaises(EParseAlreadyDefined, self.parser.parse, '''default{timer(){@x;integer x;}}''')
        self.assertRaises(EParseUEOF, self.parser.parse, 'float x=3+3;', set(('extendedglobalexpr',)))
        self.assertRaises(EParseUndefined, self.parser.parse, '''float x=-2147483648;float y=z;''')
        self.assertRaises(EParseUndefined, self.parser.parse, '''float z(){;}float y=z;''')
        self.assertRaises(EParseUndefined, self.parser.parse, '''float y=z;float z;''')
        self.assertRaises(EParseUndefined, self.parser.parse, '''default{timer(){state blah;}}''')
        self.assertRaises(EParseUndefined, self.parser.parse, '''f(){k;}''')
        self.assertRaises(EParseReturnShouldBeEmpty, self.parser.parse, '''default{timer(){return 1;}}''')
        self.assertRaises(EParseReturnIsEmpty, self.parser.parse, '''integer f(){return;}''')
        self.assertRaises(EParseFunctionMismatch, self.parser.parse, '''f(integer i){f("");}''')
        self.assertRaises(EParseFunctionMismatch, self.parser.parse, '''f(integer i){f(1,2);}''')
        self.assertRaises(EParseFunctionMismatch, self.parser.parse, '''f(integer i){f(f(1));}''')
        self.assertRaises(EParseFunctionMismatch, self.parser.parse, '''f(integer i){f();}''')
        self.assertRaises(EParseDeclarationScope, self.parser.parse, '''f(){if (1) integer i;}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){[f()];}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){3.||2;}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){3||2.;}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){3.|2;}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){3|2.;}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){3.&2;}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){3&2.;}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){3.^2;}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){3^2.;}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){f()!=2;}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){2!=f();}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){3.<"";}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){""<"".;}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){3.<<2;}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){3>>2.;}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){""-(key)"";}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){""+f();}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){""+(key)"";}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){(key)""+"";}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){(key)""+(key)"";}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){key k;k+k;}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){3/<1,2,3>;}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){3/<1,2,3,4>;}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){""*3;}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){""%4;}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){3%<2,3,4>;}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){""%4;}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){(vector)4;}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){key k;k+=k;}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){string i;i++;}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){string i;(i-=i);}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){string i;(i*=i);}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){string i;-i;}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){string i;~i;}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){string i;!i;}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){string i;++i;}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''g(){integer k;k=g();}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''g(){@x;x;}state x{}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''g(){print(g());}state x{}''')
        self.assertRaises(EParseUndefined, self.parser.parse, '''g(){integer k;k();}''')
        self.assertRaises(EParseUndefined, self.parser.parse, '''g(){++x;}state x{}''')
        self.assertRaises(EParseUndefined, self.parser.parse, '''g(){print(x);}state x{}''')
        self.assertRaises(EParseUEOF, self.parser.parse, '''f(){(integer)''')
        self.assertRaises(EParseInvalidField, self.parser.parse, '''f(){vector v;v.s;}''')
        self.assertRaises(EParseSyntax, self.parser.parse, '''f(){<1,2,3,4==5>;}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){<1,2,3,4>"">;}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){<1,2,3,"">"">;}''')
        self.assertRaises(EParseTypeMismatch, self.parser.parse, '''f(){string i;(i&=i);}''',
            set(('extendedassignment')))

        # Force a list constant down its throat, to test coverage of LIST_VALUE
        self.parser.constants['LISTCONST']=[1,2,3]
        print self.outscript.output(self.parser.parse('default{timer(){LISTCONST;}}'))

        print self.outscript.output(self.parser.parse('''string s="1" "2";default{timer(){}}''',
            ['allowmultistrings'])) # the one below doesn't work because it uses extended global expr.
        print self.outscript.output(self.parser.parse('''
            float f=2+2;
            string s = "1" "2";
            default{timer(){
            1+([]+(integer)~1);
            list a;
            float f;
            a = 3; a += 3;
            f += 4;
            integer i;
            i |= i;
            "a" "b" "c";
            "a"+(key)"b"; (key)"a" + "b";
            i>>=i;
            }}''',
            ['explicitcast','extendedtypecast','extendedassignment',
                'extendedglobalexpr', 'allowmultistrings', 'allowkeyconcat']
            ))
        print self.parser.scopeindex
        self.assertRaises(EParseUnexpected, self.parser.PopScope)

        self.assertEqual(fieldpos("a,b",",",3),-1)
        self.assertRaises(lslfuncs.ELSLTypeMismatch, self.outscript.Value2LSL, lslfuncs.Key(u''))
        self.assertRaises(lslfuncs.ELSLTypeMismatch, self.outscript.Value2LSL, '')

    def tearDown(self):
        del self.parser
        del self.outscript

if __name__ == '__main__':
    unittest.main()