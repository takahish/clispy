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
from clispy.type.basecls import *
from clispy.type.keyword import *


class UnitTestCase(unittest.TestCase):
    def testKeywordObjectRegistry(self):
        k1 = Keyword('KEYWORD-1')
        k2 = Keyword('KEYWORD-2')
        k3 = Keyword('KEYWORD-1')

        self.assertTrue(k1 is k3)
        self.assertFalse(k1 is k2)

    def testKeyword(self):
        k = Keyword(':KEYWORD')

        self.assertIsInstance(k, T)
        self.assertIsInstance(k, Symbol)
        self.assertIsInstance(k, Keyword)
        self.assertEqual(str(k), ':KEYWORD')
        self.assertEqual(k.value, ':KEYWORD')
        self.assertRaisesRegex(TypeError, "The value 100 is not of type str", Keyword, 100)

    def testKeywordTypeOf(self):
        k_t = Keyword(':KEYWORD').type_of()

        self.assertIsInstance(k_t, Symbol)
        self.assertEqual(k_t.value, 'KEYWORD')

