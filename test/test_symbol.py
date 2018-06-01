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
from clispy.symbol import Symbol, SymbolTable, SymbolError


class UnitTestCase(unittest.TestCase):
    def setUp(self):
        self.symbol_table = SymbolTable()

    def testSymbol(self):
        symbol = Symbol('TEST')
        self.assertIsInstance(symbol, Symbol)
        self.assertIsInstance(symbol, str)
        self.assertEqual(symbol, 'TEST')

    def testSymbolTable(self):
        self.assertIsInstance(self.symbol_table, SymbolTable)

        with self.assertRaisesRegex(KeyError, "TEST is not existed in symbol table"):
            self.symbol_table['TEST']

        with self.assertRaisesRegex(SymbolError, "12345 must be clispy.symbol.Symbol"):
            self.symbol_table['TEST'] = 12345

        self.symbol_table['TEST'] = Symbol('TEST')

        with self.assertRaisesRegex(SymbolError, "TEST is already existed in symbol table"):
            self.symbol_table['TEST'] = Symbol('ANOTHER_TEST')

        self.assertIsInstance(self.symbol_table['TEST'], Symbol)
        self.assertEqual(self.symbol_table['TEST'], Symbol('TEST'))
