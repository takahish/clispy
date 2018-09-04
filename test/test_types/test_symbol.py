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
from clispy.types.symbol import *


class UnitTestCase(unittest.TestCase):
    def testSymbol(self):
        s = Symbol('symbol')
        self.assertIsInstance(s, T)
        self.assertEqual(s.value, 'SYMBOL')
        self.assertRaisesRegex(TypeError, "The value 100 is not of type str", Symbol, 100)

    def testKeyword(self):
        k = Keyword(':keyword')
        self.assertIsInstance(k, T)
        self.assertIsInstance(k, Symbol)
        self.assertIsInstance(k, Keyword)
        self.assertEqual(k.value, ':KEYWORD')
        self.assertRaisesRegex(TypeError, "The value 100 is not of type str", Keyword, 100)