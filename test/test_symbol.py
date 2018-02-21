# Copyright 2018 Takahiro Ishikawa. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# ==============================================================================

import unittest
from clispy import symbol


class UnitTestCase(unittest.TestCase):
    def testSymbolTable(self):
        self.assertIsInstance(symbol.symbol_table['quote'], symbol.Symbol)
        self.assertIsInstance(symbol.symbol_table['if'], symbol.Symbol)
        self.assertIsInstance(symbol.symbol_table['setq'], symbol.Symbol)
        self.assertIsInstance(symbol.symbol_table['defun'], symbol.Symbol)
        self.assertIsInstance(symbol.symbol_table['lambda'], symbol.Symbol)
        self.assertIsInstance(symbol.symbol_table['progn'], symbol.Symbol)
        self.assertIsInstance(symbol.symbol_table['defmacro'], symbol.Symbol)
        self.assertIsInstance(symbol.symbol_table['funcall'], symbol.Symbol)
        self.assertIsInstance(symbol.symbol_table['function'], symbol.Symbol)

        self.assertIsInstance(symbol.symbol_table['cons'], symbol.Symbol)
        self.assertIsInstance(symbol.symbol_table['.'], symbol.Symbol)
        self.assertIsInstance(symbol.symbol_table['append'], symbol.Symbol)

        self.assertIsInstance(symbol.symbol_table['quasiquote'], symbol.Symbol)
        self.assertIsInstance(symbol.symbol_table['unquote'], symbol.Symbol)
        self.assertIsInstance(symbol.symbol_table['unquote-splicing'], symbol.Symbol)
        self.assertIsInstance(symbol.symbol_table['sharp-quote'], symbol.Symbol)

    def testVariables(self):
        self.assertIsInstance(symbol.QUOTE, symbol.Symbol)
        self.assertIsInstance(symbol.IF, symbol.Symbol)
        self.assertIsInstance(symbol.SETQ, symbol.Symbol)
        self.assertIsInstance(symbol.DEFUN, symbol.Symbol)
        self.assertIsInstance(symbol.LAMBDA, symbol.Symbol)
        self.assertIsInstance(symbol.PROGN, symbol.Symbol)
        self.assertIsInstance(symbol.DEFMACRO, symbol.Symbol)
        self.assertIsInstance(symbol.FUNCALL, symbol.Symbol)
        self.assertIsInstance(symbol.FUNCTION, symbol.Symbol)

        self.assertIsInstance(symbol.CONS, symbol.Symbol)
        self.assertIsInstance(symbol.DOT, symbol.Symbol)
        self.assertIsInstance(symbol.APPEND, symbol.Symbol)

        self.assertIsInstance(symbol.QUASIQUOTE, symbol.Symbol)
        self.assertIsInstance(symbol.UNQUOTE, symbol.Symbol)
        self.assertIsInstance(symbol.UNQUOTE_SPLICING, symbol.Symbol)
        self.assertIsInstance(symbol.SHARPQUOTE, symbol.Symbol)

    def testSyntacticSuger(self):
        self.assertEqual(symbol.QUOTES["'"], symbol.QUOTE)
        self.assertEqual(symbol.QUOTES["`"], symbol.QUASIQUOTE)
        self.assertEqual(symbol.QUOTES[","], symbol.UNQUOTE)
        self.assertEqual(symbol.QUOTES[",@"], symbol.UNQUOTE_SPLICING)
        self.assertEqual(symbol.QUOTES["#'"], symbol.SHARPQUOTE)

    def test_eof_object(self):
        self.assertIsInstance(symbol.EOF_OBJECT, symbol.Symbol)
