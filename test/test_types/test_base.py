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
from clispy.types.base import *


class UnitTestCase(unittest.TestCase):
    def testTObjectRegistry(self):
        t1 = T()
        t2 = T()

        self.assertTrue(t1 is t2)

    def testT(self):
        t = T()

        self.assertIsInstance(t, T)
        self.assertEqual(t.value, True)

    def testTTypeOf(self):
        t_t = T().type_of()

        self.assertIsInstance(t_t, Symbol)
        self.assertEqual(t_t.value, 'BOOLEAN')

    def testNilObjectRegistry(self):
        n1 = Nil()
        n2 = Nil()

        self.assertTrue(n1 is n2)

    def testNil(self):
        n = Nil()

        self.assertIsInstance(n, T)
        self.assertIsInstance(n, Nil)
        self.assertEqual(n.value, False)

    def testNilTypeOf(self):
        n_t = Nil().type_of()

        self.assertIsInstance(n_t, Symbol)
        self.assertEqual(n_t.value, 'NIL')

    def testSymbolObjectRegistry(self):
        s1 = Symbol('symbol_1')
        s2 = Symbol('symbol_2')
        s3 = Symbol('symbol_1')

        self.assertTrue(s1 is s3)
        self.assertFalse(s1 is s2)

    def testSymbol(self):
        s = Symbol('symbol')

        self.assertIsInstance(s, T)
        self.assertIsInstance(s, Symbol)
        self.assertEqual(s.value, 'SYMBOL')
        self.assertRaisesRegex(TypeError, "The value 100 is not of type str", Symbol, 100)

    def testSymbolTypeOf(self):
        s_t = Symbol('symbol').type_of()

        self.assertIsInstance(s_t, Symbol)
        self.assertEqual(s_t.value, 'SYMBOL')
