import unittest
import symbol

class UnitTestCase(unittest.TestCase):
    def testSymbolTable(self):
        self.assertIsInstance(symbol._symbol_table['quote'], symbol._Symbol)
        self.assertIsInstance(symbol._symbol_table['if'], symbol._Symbol)
        self.assertIsInstance(symbol._symbol_table['set!'], symbol._Symbol)
        self.assertIsInstance(symbol._symbol_table['define'], symbol._Symbol)
        self.assertIsInstance(symbol._symbol_table['lambda'], symbol._Symbol)
        self.assertIsInstance(symbol._symbol_table['begin'], symbol._Symbol)
        self.assertIsInstance(symbol._symbol_table['define-macro'], symbol._Symbol)

        self.assertIsInstance(symbol._symbol_table['quasiquote'], symbol._Symbol)
        self.assertIsInstance(symbol._symbol_table['unquote'], symbol._Symbol)
        self.assertIsInstance(symbol._symbol_table['unquote-splicing'], symbol._Symbol)

    def testVariables(self):
        self.assertIsInstance(symbol._quote, symbol._Symbol)
        self.assertIsInstance(symbol._if, symbol._Symbol)
        self.assertIsInstance(symbol._set, symbol._Symbol)
        self.assertIsInstance(symbol._define, symbol._Symbol)
        self.assertIsInstance(symbol._lambda, symbol._Symbol)
        self.assertIsInstance(symbol._begin, symbol._Symbol)
        self.assertIsInstance(symbol._define_macro, symbol._Symbol)

        self.assertIsInstance(symbol._quasiquote, symbol._Symbol)
        self.assertIsInstance(symbol._unquote, symbol._Symbol)
        self.assertIsInstance(symbol._unquote_splicing, symbol._Symbol)

    def testSyntacticSuger(self):
        self.assertEqual(symbol._quotes["'"], symbol._quote)
        self.assertEqual(symbol._quotes["`"], symbol._quasiquote)
        self.assertEqual(symbol._quotes[","], symbol._unquote)
        self.assertEqual(symbol._quotes[",@"], symbol._unquote_splicing)

    def test_eof_object(self):
        self.assertIsInstance(symbol._eof_object, symbol._Symbol)
