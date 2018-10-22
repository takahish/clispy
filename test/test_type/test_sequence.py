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

from io import StringIO
import unittest
from clispy.type.sequence import *


class UnitTestCase(unittest.TestCase):
    def setUp(self):
        self.capture = StringIO()
        sys.stderr = self.capture

    def tearDown(self):
        sys.stderr = sys.__stderr__

    def testSequenceObjectRegistry(self):
        s1 = Sequence()
        s2 = Sequence()

        self.assertTrue(s1 is s2)

    def testSequence(self):
        s = Sequence()

        self.assertIsInstance(s, T)
        self.assertIsInstance(s, Sequence)
        self.assertEqual(str(s), 'T')
        self.assertEqual(s.value, True)

    def testSequenceTypeOf(self):
        s_t = Sequence().type_of()

        self.assertIsInstance(s_t, Symbol)
        self.assertEqual(s_t.value, 'SEQUENCE')

    def testListObjectRegistry(self):
        l1 = List()
        l2 = List()

        self.assertTrue(l1 is l2)

    def testList(self):
        l = List()

        self.assertIsInstance(l, T)
        self.assertIsInstance(l, Sequence)
        self.assertIsInstance(l, List)
        self.assertEqual(str(l), 'T')
        self.assertEqual(l.value, True)

    def testListTypeOf(self):
        l_t = List().type_of()

        self.assertIsInstance(l_t, Symbol)
        self.assertEqual(l_t.value, 'LIST')

    def testConsObjectRegistry(self):
        c1 = Cons(2, Cons(1, Null()))
        c2 = Cons(2, Cons(1, Null()))
        c3 = Cons(c1, 3)
        c4 = Cons(3, c1)

        self.assertTrue(c1 is c2)
        self.assertFalse(c1 is c3)

        # test nested Cons object equality
        self.assertTrue(c1, c3._car)
        self.assertTrue(c1, c4._cdr)

    def testCons(self):
        c1 = Cons(2, Cons(1, Null()))
        c2 = Cons(c1, Null())
        c3 = Cons(3, c1)

        self.assertIsInstance(c1, T)
        self.assertIsInstance(c1, Sequence)
        self.assertIsInstance(c1, List)
        self.assertIsInstance(c1, Cons)

        self.assertEqual(str(c1), '(2 1)')
        self.assertEqual(str(c2), '((2 1))')
        self.assertEqual(str(c3), '(3 2 1)')

    def testConsConvertedFromList(self):
        c1 = Cons([1, 2, 3])
        c2 = Cons([1, [2, [3]]])
        c3 = Cons([1])

        self.assertIsInstance(c1, T)
        self.assertIsInstance(c1, Sequence)
        self.assertIsInstance(c1, List)
        self.assertIsInstance(c2, Cons)

        self.assertEqual(str(c1), '(1 2 3)')
        self.assertEqual(str(c2), '(1 (2 (3)))')
        self.assertEqual(str(c3), '(1)')

    def testConsConvertedFromEmptyList(self):
        c = Cons([])

        self.assertIsInstance(c, T)
        self.assertIsInstance(c, Sequence)
        self.assertIsInstance(c, List)
        self.assertIsInstance(c, Symbol)

    def testConsConvertedFromAtom(self):
        c = Cons(1)

        self.assertIsInstance(c, T)
        self.assertIsInstance(c, Sequence)
        self.assertIsInstance(c, List)
        self.assertIsInstance(c, Cons)
        self.assertEqual(str(c), '(1)')

    def testDottedListConvertedFromList(self):
        c = Cons([1, 2], 3)

        self.assertIsInstance(c, T)
        self.assertIsInstance(c, Sequence)
        self.assertIsInstance(c, List)
        self.assertIsInstance(c, Cons)
        self.assertEqual(str(c), '((1 2) . 3)')

    def testDottedListConvertedFromEmptyList(self):
        c = Cons([], 3)

        self.assertIsInstance(c, T)
        self.assertIsInstance(c, Sequence)
        self.assertIsInstance(c, List)
        self.assertIsInstance(c, Cons)
        self.assertEqual(str(c), '(NIL . 3)')

    def testConsLazyEvaluation(self):
        c = Cons(1, Cons(2, Null()))

        c.value  # capture stderr string
        self.assertEqual(self.capture.getvalue(), "Info: lazy evaluation of an attribute\n")

        self.assertEqual(c.value, [1, 2])

    def testConsSpecialMethod(self):
        c = Cons(1, Cons(2, Null()))

        self.assertEqual(c.car, 1)
        self.assertEqual(c.cdr, Cons(2, Null()))

    def testConsTypeOf(self):
        c_t = Cons(1, Cons(2, Null())).type_of()

        self.assertIsInstance(c_t, Symbol)
        self.assertEqual(c_t.value, 'CONS')

    def testConsDottedList(self):
        c1 = Cons(1, 2)
        c2 = Cons(c1, 3)
        c3 = Cons(4, c1)

        self.assertIsInstance(c1, T)
        self.assertIsInstance(c1, Sequence)
        self.assertIsInstance(c1, List)
        self.assertIsInstance(c1, Cons)
        # self.assertEqual(self.value, '[1, 2]')  # test in testConsDottedListLazyEvaluation

        # test nested Cons the official string representation
        self.assertEqual(str(c1), '(1 . 2)')
        self.assertEqual(str(c2), '((1 . 2) . 3)')
        self.assertEqual(str(c3), '(4 1 . 2)')

    def testConsDottedListLazyEvaluation(self):
        c = Cons(1, 2)

        c.value  # capture stderr string
        self.assertEqual(self.capture.getvalue(), "Info: lazy evaluation of an attribute\n")

        self.assertEqual(c.value, [1, 2])

    def testConsDottedListSpecialMethods(self):
        c = Cons(1, 2)

        self.assertEqual(c.car, 1)
        self.assertEqual(c.cdr, 2)

    def testConsDottedListTypeOf(self):
        c = Cons(1, 2).type_of()

        self.assertIsInstance(c, Symbol)
        self.assertEqual(c.value, 'CONS')

    def testNullObjectRegistry(self):
        n1 = Null()
        n2 = Null()

        self.assertTrue(n1 is n2)

    def testNull(self):
        n = Null()

        self.assertIsInstance(n, T)
        self.assertIsInstance(n, Sequence)
        self.assertIsInstance(n, List)
        self.assertIsInstance(n, Symbol)
        self.assertEqual(str(n), 'NIL')
        self.assertIsInstance(n.value, Nil)
        self.assertEqual(n.value, Nil())

    def testNullTypeOf(self):
        n_t = Null().type_of()

        self.assertIsInstance(n_t, Symbol)
        self.assertEqual(n_t.value, 'NULL')
