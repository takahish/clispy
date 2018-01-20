from unittest import TestCase
from lispy.symbol import _Symbol

class UnitTestCase(TestCase):
    def testSymbol(self):
        self.assertEqual(str, _Symbol)
