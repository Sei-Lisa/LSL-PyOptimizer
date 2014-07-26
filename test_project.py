import unittest
from lslparse.lslfuncs import *

class UnitTestCase(unittest.TestCase):
    def reallyequal(self, actual, expected, tol=None):
        if tol is None:
            return repr(actual) == repr(expected)

        if type(actual) != type(expected):
            return False

        # Deal with floats (edge cases, tolerance)
        if isinstance(actual, float):
            if actual == 0.0:
                return str(actual) == str(expected)
            elif math.isnan(actual):
                return math.isnan(expected)
            return abs(actual - expected) <= tol

        # Deal with tuples and lists (item-by-item, recursively)
        if isinstance(actual, (tuple, list)):
            return all(self.reallyequal(i1, i2, tol) for i1, i2 in zip(actual, expected))

        # Fall back to 'classic' equality
        return actual == expected

    def check(self, fn, expected, tol = None):
        nan = float('nan')
        inf = float('inf')
        assert nan # keep PyFlakes happy
        assert inf # keep PyFlakes happy
        actual = eval(fn)
        self.assertTrue(self.reallyequal(actual, expected, tol), "\nExpression: " + fn + '\nActual: ' + repr(actual) + '\nExpect: ' + repr(expected) + '\n')


class MetaTest(UnitTestCase):
    def test_reallyequal(self):
        self.assertTrue(self.reallyequal(float('nan'), float('nan')))
        self.assertFalse(self.reallyequal(-0.0, 0.0))
        self.assertFalse(self.reallyequal(-0.0, 0.0, 0.0))
        self.assertFalse(self.reallyequal(-0.1, 0.1, 0.0))
        self.assertTrue(self.reallyequal((float('nan'),), (float('nan'),)))
        self.assertTrue(self.reallyequal((float('nan'),), (float('nan'),), 0.0))
        self.assertTrue(self.reallyequal('a', 'a', 0.0))
        self.assertTrue(self.reallyequal('a', 'a', None))
        self.assertTrue(self.reallyequal([float('nan')], [float('nan')]))
        self.assertFalse(self.reallyequal(Quaternion((1.,0.,0.,0.)), (1.,0.,0.,0.)))
        self.assertFalse(self.reallyequal(Quaternion((1.,0.,0.,0.)), (1.,0.,0.,0.), 0.0))
        self.check('nan', float('nan'))

class LSLFunctionTest(UnitTestCase):
    def test_llListSort(self):
        self.check('''llListSort([Quaternion((1.,2.,3.,4.)),Quaternion((2.,3.,4.,5.)),
                                  Quaternion((5.,4.,3.,2.)),Quaternion((4.,3.,2.,1.)),
                                  Quaternion((3.,2.,1.,0.))
                                 ],1,0)''',
                   [Quaternion((3.,2.,1.,0.)),Quaternion((4.,3.,2.,1.)),
                    Quaternion((5.,4.,3.,2.)),Quaternion((2.,3.,4.,5.)),
                    Quaternion((1.,2.,3.,4.))
                   ])
        self.check('''llListSort([Quaternion((1.,2.,3.,4.)),Quaternion((2.,3.,4.,5.)),
                                  Quaternion((5.,4.,3.,2.)),Quaternion((4.,3.,2.,1.)),
                                  Quaternion((3.,2.,1.,0.))]
                                 ,1,1)''',
                    [Quaternion((1.,2.,3.,4.)),Quaternion((2.,3.,4.,5.)),Quaternion((5.,4.,3.,2.)),Quaternion((4.,3.,2.,1.)),Quaternion((3.,2.,1.,0.))]
                    )

        self.check('''llListSort([Vector((1.,0.,0.)),Vector((0.,1.,0.)),Vector((0.,0.,3.)),Vector((3.,0.,0.))]
                                 ,1,1)''',
                    [Vector((1., 0., 0.)), Vector((0., 1., 0.)), Vector((0., 0., 3.)), Vector((3., 0., 0.))]
                    )

        self.check('''llListSort([2,0,1,1,2,2,2,3,2,4,1,5,2,6], 2, 1)''',
                    [1, 1, 1, 5, 2, 2, 2, 3, 2, 4, 2, 0, 2, 6])

        self.check('''llListSort([2,0,1,1,2,2,2,3,2,4,1,5,2,6], 2, 0)''',
                    [2, 6, 2, 4, 2, 3, 2, 2, 2, 0, 1, 5, 1, 1])


if __name__ == '__main__':
    unittest.main()
