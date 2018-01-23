from unittest import TestCase
from clispy.symbol import _Symbol, _symbol_table
from clispy.symbol import _quote, _if, _set, _define, _lambda, _begin, _define_macro
from clispy.symbol import _quasiquote, _unquote, _unquote_splicing
from clispy.symbol import _append, _cons, _let
from clispy.symbol import _quotes

class UnitTestCase(TestCase):
    def testSymbolTable(self):
        self.assertIsInstance(_symbol_table['quote'], _Symbol)
        self.assertIsInstance(_symbol_table['if'], _Symbol)
        self.assertIsInstance(_symbol_table['set!'], _Symbol)
        self.assertIsInstance(_symbol_table['define'], _Symbol)
        self.assertIsInstance(_symbol_table['lambda'], _Symbol)
        self.assertIsInstance(_symbol_table['begin'], _Symbol)
        self.assertIsInstance(_symbol_table['define-macro'], _Symbol)

        self.assertIsInstance(_symbol_table['quasiquote'], _Symbol)
        self.assertIsInstance(_symbol_table['unquote'], _Symbol)
        self.assertIsInstance(_symbol_table['unquote-splicing'], _Symbol)

        self.assertIsInstance(_symbol_table['append'], _Symbol)
        self.assertIsInstance(_symbol_table['cons'], _Symbol)
        self.assertIsInstance(_symbol_table['let'], _Symbol)

    def testVariables(self):
        self.assertIsInstance(_quote, _Symbol)
        self.assertIsInstance(_if, _Symbol)
        self.assertIsInstance(_set, _Symbol)
        self.assertIsInstance(_define, _Symbol)
        self.assertIsInstance(_lambda, _Symbol)
        self.assertIsInstance(_begin, _Symbol)
        self.assertIsInstance(_define_macro, _Symbol)

        self.assertIsInstance(_quasiquote, _Symbol)
        self.assertIsInstance(_unquote, _Symbol)
        self.assertIsInstance(_unquote_splicing, _Symbol)

        self.assertIsInstance(_append, _Symbol)
        self.assertIsInstance(_cons, _Symbol)
        self.assertIsInstance(_let, _Symbol)

    def testSyntacticSuger(self):
        self.assertEqual(_quotes["'"], _quote)
        self.assertEqual(_quotes["`"], _quasiquote)
        self.assertEqual(_quotes[","], _unquote)
        self.assertEqual(_quotes[",@"], _unquote_splicing)
