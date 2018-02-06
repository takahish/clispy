import unittest
import symbol

class UnitTestCase(unittest.TestCase):
    def testSymbolTable(self):
        self.assertIsInstance(symbol._symbol_table['quote'], symbol._Symbol)
        self.assertIsInstance(symbol._symbol_table['if'], symbol._Symbol)
        self.assertIsInstance(symbol._symbol_table['setq'], symbol._Symbol)
        self.assertIsInstance(symbol._symbol_table['defun'], symbol._Symbol)
        self.assertIsInstance(symbol._symbol_table['lambda'], symbol._Symbol)
        self.assertIsInstance(symbol._symbol_table['progn'], symbol._Symbol)
        self.assertIsInstance(symbol._symbol_table['defmacro'], symbol._Symbol)
        self.assertIsInstance(symbol._symbol_table['funcall'], symbol._Symbol)
        self.assertIsInstance(symbol._symbol_table['function'], symbol._Symbol)
        self.assertIsInstance(symbol._symbol_table['cons'], symbol._Symbol)
        self.assertIsInstance(symbol._symbol_table['append'], symbol._Symbol)

        self.assertIsInstance(symbol._symbol_table['quasiquote'], symbol._Symbol)
        self.assertIsInstance(symbol._symbol_table['unquote'], symbol._Symbol)
        self.assertIsInstance(symbol._symbol_table['unquote-splicing'], symbol._Symbol)
        self.assertIsInstance(symbol._symbol_table['sharp-quote'], symbol._Symbol)

    def testVariables(self):
        self.assertIsInstance(symbol._quote, symbol._Symbol)
        self.assertIsInstance(symbol._if, symbol._Symbol)
        self.assertIsInstance(symbol._setq, symbol._Symbol)
        self.assertIsInstance(symbol._defun, symbol._Symbol)
        self.assertIsInstance(symbol._lambda, symbol._Symbol)
        self.assertIsInstance(symbol._progn, symbol._Symbol)
        self.assertIsInstance(symbol._defmacro, symbol._Symbol)
        self.assertIsInstance(symbol._funcall, symbol._Symbol)
        self.assertIsInstance(symbol._function, symbol._Symbol)
        self.assertIsInstance(symbol._cons, symbol._Symbol)
        self.assertIsInstance(symbol._append, symbol._Symbol)

        self.assertIsInstance(symbol._quasiquote, symbol._Symbol)
        self.assertIsInstance(symbol._unquote, symbol._Symbol)
        self.assertIsInstance(symbol._unquote_splicing, symbol._Symbol)
        self.assertIsInstance(symbol._sharpquote, symbol._Symbol)

    def testSyntacticSuger(self):
        self.assertEqual(symbol._quotes["'"], symbol._quote)
        self.assertEqual(symbol._quotes["`"], symbol._quasiquote)
        self.assertEqual(symbol._quotes[","], symbol._unquote)
        self.assertEqual(symbol._quotes[",@"], symbol._unquote_splicing)
        self.assertEqual(symbol._quotes["#'"], symbol._sharpquote)

    def test_eof_object(self):
        self.assertIsInstance(symbol._eof_object, symbol._Symbol)
