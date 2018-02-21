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
from clispy import cons


class UnitTestCase(unittest.TestCase):
    def setUp(self):
        self.cons_cell = cons.Cons([1, 2, 3])
        self.dotted_pair = cons.DottedPair(['a', 'b'])

    def testConsSell(self):
        self.assertIsInstance(self.cons_cell, list)
        self.assertIsInstance(self.cons_cell, cons.Cons)

    def testDottedList(self):
        self.assertIsInstance(self.dotted_pair, list)
        self.assertIsInstance(self.dotted_pair, cons.Cons)
        self.assertIsInstance(self.dotted_pair, cons.DottedPair)
