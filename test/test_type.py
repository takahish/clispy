from unittest import TestCase, main
from lispy.type import Symbol, List, Number

class UnitTestCase(TestCase):
    def testSymbol(self):
        self.assertEqual(str, Symbol)

    def testList(self):
        self.assertEqual(list, List)

    def testNumber(self):
        self.assertIn(int, Number)
        self.assertIn(float, Number)
