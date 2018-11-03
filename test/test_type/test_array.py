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
from clispy.type.sequence import *
from clispy.type.array import *


class UnitTestCase(unittest.TestCase):
    def testArrayObjectRegistry(self):
        na = np.array([1, 2, 3])
        nb = np.array([1, 2, 3])

        a = Array(na)
        b = Array(na)
        c = Array(nb)

        self.assertTrue(a is b)
        self.assertFalse(a is c)

    def testArray(self):
        na = np.array([1, 2, 3])
        a = Array(na)

        self.assertIsInstance(a, T)
        self.assertIsInstance(a, Array)
        self.assertEqual(str(a), '#(1 2 3)')
        self.assertTrue(a.value is na)

    def testArrayTowDimensional(self):
        nb = np.array([[1, 2, 3], [4, 5, 6]])
        b = Array(nb)

        self.assertIsInstance(b, T)
        self.assertIsInstance(b, Array)
        self.assertEqual(str(b), '#2A((1 2 3) (4 5 6))')
        self.assertTrue(b.value is nb)

    def testArrayMultiDimensional(self):
        nc = np.array([[[1, 2], [3, 4]], [[5, 6], [7, 8]]])
        c = Array(nc)

        self.assertIsInstance(c, T)
        self.assertIsInstance(c, Array)
        self.assertEqual(str(c), '#3A(((1 2) (3 4)) ((5 6) (7 8)))')
        self.assertTrue(c.value is nc)

    def testArrayTypeOf(self):
        na = np.array([1, 2, 3])
        a_t = Array(na).type_of()

        self.assertIsInstance(a_t, Symbol)
        self.assertEqual(a_t.value, 'ARRAY')

    def testArrayClassOf(self):
        na = np.array([1, 2, 3])
        a_c = Array(na).class_of()

        self.assertIsInstance(a_c, BuiltInClass)
        self.assertIsInstance(a_c.type_of(), Symbol)

    def testVectorObjectRegistry(self):
        va = np.array([1, 2, 3])
        vb = np.array([1, 2, 3])

        a = Vector(va)
        b = Vector(va)
        c = Vector(vb)

        self.assertTrue(a is b)
        self.assertFalse(a is c)

    def testVector(self):
        va = np.array([1, 2, 3])
        v = Vector(va)

        self.assertIsInstance(v, T)
        self.assertIsInstance(v, Array)
        self.assertIsInstance(v, Sequence)
        self.assertIsInstance(v, Vector)
        self.assertEqual(str(v), '#(1 2 3)')
        self.assertTrue(v.value is va)

    def testVectorTypeOf(self):
        va = np.array([1, 2, 3])
        v_t = Vector(va).type_of()

        self.assertIsInstance(v_t, Symbol)
        self.assertEqual(v_t.value, 'VECTOR')

    def testVectorClassOf(self):
        va = np.array([1, 2, 3])
        v_c = Vector(va).class_of()

        self.assertIsInstance(v_c, BuiltInClass)
        self.assertIsInstance(v_c.type_of(), Symbol)

    def testStringObjectRegistry(self):
        a = String("string_a")
        b = String("string_a")
        c = String("string_b")

        self.assertTrue(a is b)
        self.assertFalse(a is c)

    def testString(self):
        s = String('string')

        self.assertIsInstance(s, T)
        self.assertIsInstance(s, Array)
        self.assertIsInstance(s, Sequence)
        self.assertIsInstance(s, Vector)
        self.assertIsInstance(s, String)
        self.assertEqual(str(s), '"string"')
        self.assertEqual(s.value, 'string')
