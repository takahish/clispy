from unittest import TestCase
from clispy.symbol import _Symbol, _symbol_table
from clispy.macro import _MacroTable, _macro_table

class UnitTestCase(TestCase):
    def testMacroTable(self):
        self.assertIsInstance(_macro_table, _MacroTable)

        _macro_table['cube'] = 'proc' # setitem
        self.assertEqual(_macro_table[_Symbol('cube')], 'proc') # getitem
