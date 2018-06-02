import unittest
from clispy.symbol import Symbol, SymbolTable
from clispy.environment import VariableEnvironment, FunctionEnvironment, MacroEnvironment
from clispy.package import Package, PackageTable, PackageError


class UnitTestCase(unittest.TestCase):
    def setUp(self):
        self.package = Package()
        self.package_table = PackageTable()

        self.symbol_table = SymbolTable()
        self.symbol_table['TEST'] = Symbol('TEST')

    def testPackage(self):
        self.assertIsInstance(self.package, Package)

        self.assertIsInstance(self.package.intern, SymbolTable)
        self.package.intern = self.symbol_table
        self.assertIsInstance(self.package.intern['TEST'], Symbol)
        self.assertEqual(self.package.intern['TEST'], Symbol('TEST'))
        with self.assertRaisesRegex(PackageError, "12345 must be clispy.symbol.SymbolTable"):
            self.package.intern = 12345

        self.assertIsInstance(self.package.extern, SymbolTable)
        with self.assertRaisesRegex(PackageError, "12345 must be clispy.symbol.SymbolTable"):
            self.package.extern = 12345

        self.assertIsInstance(self.package.var_env, VariableEnvironment)
        self.assertIsInstance(self.package.func_env, FunctionEnvironment)
        self.assertIsInstance(self.package.macro_env, MacroEnvironment)

    def testPackageTable(self):
        self.assertIsInstance(self.package_table, PackageTable)

        self.assertIsInstance(self.package_table['COMMON-LISP-USER'], Package)
        self.assertIsInstance(self.package_table['SYSTEM'], Package)
        self.assertIsInstance(self.package_table['PYTHON'], Package)

        self.assertEqual(self.package_table.current_space, 'COMMON-LISP-USER')

        with self.assertRaisesRegex(KeyError, "TEST is not existed in package table"):
            self.package_table['TEST']

        with self.assertRaisesRegex(PackageError, "SYSTEM is already existed in package table"):
            self.package_table['SYSTEM'] = Package()

        with self.assertRaisesRegex(PackageError, "12345 must be clispy.package.Package"):
            self.package_table['TEST'] = 12345

        self.package_table['TEST'] = Package()
        self.package_table.current_space = 'TEST'
        self.assertIsInstance(self.package_table['TEST'], Package)
