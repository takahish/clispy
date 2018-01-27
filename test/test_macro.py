import unittest
import symbol
import macro

class UnitTestCase(unittest.TestCase):
    def testMacroTable(self):
        self.assertIsInstance(macro._macro_table, macro._MacroTable)

        macro._macro_table['cube'] = 'proc' # setitem
        self.assertEqual(macro._macro_table[symbol._Symbol('cube')], 'proc') # getitem
