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
from clispy.symbol import *


class UnitTestCase(unittest.TestCase):
    def testSymbolTable(self):
        self.assertIsInstance(symbol_table['quote'], Symbol)
        self.assertIsInstance(symbol_table['if'], Symbol)
        self.assertIsInstance(symbol_table['setq'], Symbol)
        self.assertIsInstance(symbol_table['defun'], Symbol)
        self.assertIsInstance(symbol_table['lambda'], Symbol)
        self.assertIsInstance(symbol_table['progn'], Symbol)
        self.assertIsInstance(symbol_table['defmacro'], Symbol)
        self.assertIsInstance(symbol_table['function'], Symbol)

        self.assertIsInstance(symbol_table['cons'], Symbol)
        self.assertIsInstance(symbol_table['.'], Symbol)
        self.assertIsInstance(symbol_table['append'], Symbol)

        self.assertIsInstance(symbol_table['quasiquote'], Symbol)
        self.assertIsInstance(symbol_table['unquote'], Symbol)
        self.assertIsInstance(symbol_table['unquote-splicing'], Symbol)
        self.assertIsInstance(symbol_table['sharp-quote'], Symbol)

    def testVariables(self):
        self.assertIsInstance(QUOTE, Symbol)
        self.assertIsInstance(IF, Symbol)
        self.assertIsInstance(SETQ, Symbol)
        self.assertIsInstance(DEFUN, Symbol)
        self.assertIsInstance(LAMBDA, Symbol)
        self.assertIsInstance(PROGN, Symbol)
        self.assertIsInstance(DEFMACRO, Symbol)
        self.assertIsInstance(FUNCTION, Symbol)

        self.assertIsInstance(CONS, Symbol)
        self.assertIsInstance(DOT, Symbol)
        self.assertIsInstance(APPEND, Symbol)

        self.assertIsInstance(QUASIQUOTE, Symbol)
        self.assertIsInstance(UNQUOTE, Symbol)
        self.assertIsInstance(UNQUOTE_SPLICING, Symbol)
        self.assertIsInstance(SHARPQUOTE, Symbol)

    def testSyntacticSuger(self):
        self.assertEqual(QUOTES["'"], QUOTE)
        self.assertEqual(QUOTES["`"], QUASIQUOTE)
        self.assertEqual(QUOTES[","], UNQUOTE)
        self.assertEqual(QUOTES[",@"], UNQUOTE_SPLICING)
        self.assertEqual(QUOTES["#'"], SHARPQUOTE)

    def test_eof_object(self):
        self.assertIsInstance(EOF_OBJECT, Symbol)
