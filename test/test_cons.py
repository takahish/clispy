import unittest
import cons

class UnitTestCase(unittest.TestCase):
    def setUp(self):
        self.cons_cell = cons._Cons([1, 2, 3])
        self.dotted_pair = cons._DottedPair(['a', 'b'])

    def testConsSell(self):
        self.assertIsInstance(self.cons_cell, list)
        self.assertIsInstance(self.cons_cell, cons._Cons)

    def testDottedList(self):
        self.assertIsInstance(self.dotted_pair, list)
        self.assertIsInstance(self.dotted_pair, cons._Cons)
        self.assertIsInstance(self.dotted_pair, cons._DottedPair)
