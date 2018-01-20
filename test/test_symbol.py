from unittest import TestCase
from lispy.symbol import Symbol

class UnitTestCase(TestCase):
    def testSymbol(self):
        self.assertEqual(str, Symbol)
