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
from clispy.cons import DottedPair
from clispy import function


class UnitTestCase(unittest.TestCase):
    def setUp(self):
        self.builtin_operator = function.BuiltInFunction()

    def test_null(self):
        self.assertTrue(function._null(False))      # (null nil) => T
        self.assertTrue(function._null([]))         # (null '()) => T
        self.assertFalse(function._null(1))         # (null 1) => NIL
        self.assertFalse(function._null([1]))       # (null '(1)) => NIL
        self.assertFalse(function._null([1, 2, 3])) # (null '(1 2 3)) => NIL

    def test_consp(self):
        self.assertTrue(function._consp([1]))       # (consp '(1)) => T
        self.assertTrue(function._consp([1, 2, 3])) # (consp '(1 2 3)) => T
        self.assertFalse(function._consp(False))    # (consp nil) => NIL
        self.assertFalse(function._consp(1))        # (consp 1) => NIL
        self.assertFalse(function._consp([]))       # (consp '()) => NIL

    def test_listp(self):
        self.assertTrue(function._listp(False))  # (listp nil) => T
        self.assertTrue(function._listp([1, 2])) # (listp '(1 2)) => T
        self.assertFalse(function._listp(1))     # (listp 1) = NIL

    def test_atom(self):
        self.assertTrue(function._atom('a'))       # (atom 'a) => T
        self.assertTrue(function._atom(False))     # (atom nil) => T
        self.assertTrue(function._atom([])) # (atom '()) => T
        self.assertTrue(function._atom(3))         # (atom 3) => T
        self.assertFalse(function._atom(function._cons(1, False))) # (atom (cons 1 nil)) => NIL

    def test_cons(self):
        self.assertEqual(function._cons(1, False), [1])        # (cons 1 nil) => (1)
        self.assertEqual(function._cons(1, []), [1])           # (cons 1 '()) => (1)
        self.assertEqual(function._cons(1, [2, 3]), [1, 2, 3]) # (cons 1 '(2 3)) => (1 2 3)

        # dotted pair
        self.assertIsInstance(function._cons(1, 2), DottedPair)
        self.assertEqual(function._cons(1, 2), DottedPair([1, 2])) # (cons 1 2) => (1 . 2)
        self.assertIsInstance(function._cons(1, DottedPair([2, 3])), DottedPair)
        self.assertEqual(function._cons(1, DottedPair([2, 3])), DottedPair([1, 2, 3]))

    def test_car(self):
        self.assertEqual(function._car(False), False) # (car nil) => NIL
        self.assertEqual(function._car([]), False)    # (car '()) => NIL
        self.assertEqual(function._car([1]), 1)       # (car '(1)) => 1
        self.assertEqual(function._car([1, 2, 3]), 1) # (car '(1 2 3)) => 1
        self.assertEqual(function._car(DottedPair([1, 2])), 1) # (car '(1 . 2)) => 1
        self.assertRaisesRegex(TypeError, "The value 1 is not LIST.", function._car, 1) # (car 1) => Error

    def test_cdr(self):
        self.assertEqual(function._cdr(False), False) # (cdr nil) => NIL
        self.assertEqual(function._cdr([]), False)    # (cdr '()) => NIL
        self.assertEqual(function._cdr([1]), False)   # (cdr '(1)) => NIL
        self.assertEqual(function._cdr([1, 2]), [2])  # (cdr '(1 2)) => (2)
        self.assertEqual(function._cdr(DottedPair([1, 2])), 2) # (cdr '(1 . 2)) => 2

        # (cdr '(1 2 . 3)) => (2 . 3)
        self.assertIsInstance(function._cdr(DottedPair([1, 2, 3])), DottedPair)
        self.assertEqual(function._cdr(DottedPair([1, 2, 3])), DottedPair([2, 3]))
        self.assertRaisesRegex(TypeError, "The value 1 is not LIST.", function._cdr, 1) # (cdr 1) => Error

    def test_append(self):
        self.assertEqual(function._append(), False) # (append) => NIL
        self.assertEqual(function._append(1), 1)    # (append 1) => NIL

        # (append '(1) '(2) '() '(3)) => (1 2 3)
        self.assertEqual(function._append([1], [2], [], [3]), [1, 2, 3])
        # (append '(1) '(2) nil '(3)) => (1 2 3)
        self.assertEqual(function._append([1], [2], False, [3]), [1, 2, 3])

    def test_list(self):
        self.assertEqual(function._list(), False) # (list) => NIL
        self.assertEqual(function._list(1), [1])  # (list 1) => (1)
        self.assertEqual(function._list(1, 2, 3), [1, 2, 3]) # (list 1 2 3) => (1 2 3)

    def test_add(self):
        self.assertEqual(function._add(1), 1)       # (+ 1) => 1
        self.assertEqual(function._add(1, 2, 3), 6) # (+ 1 2 3) => 6
        self.assertRaises(TypeError, function._add) # (+) => Error
        self.assertRaisesRegex(TypeError, "The value must be NUMBER.", function._add, "a") # (+ "a") => Error

    def test_sub(self):
        self.assertEqual(function._sub(1), 1)        # (- 1) => 1
        self.assertEqual(function._sub(1, 2, 3), -4) # (- 1 2 3) => -4
        self.assertRaises(TypeError, function._sub)  # (-) => Error
        self.assertRaisesRegex(TypeError, "The value must be NUMBER.", function._sub, "a")  # (- "a") => Error

    def test_mul(self):
        self.assertEqual(function._mul(1), 1)       # (* 1) => 1
        self.assertEqual(function._mul(1, 2, 3), 6) # (* 1 2 3) => 6
        self.assertRaises(TypeError, function._mul) # (*) => Error
        self.assertRaisesRegex(TypeError, "The value must be NUMBER.", function._mul, "a")  # (* "a") => Error

    def test_div(self):
        self.assertEqual(function._div(1), 1)                     # (/ 1) => 1
        self.assertEqual(function._div(1, 2), 0.5)                # (/ 1 2) => 0.5
        self.assertRaises(TypeError, function._div)               # (/) => Error
        self.assertRaises(ZeroDivisionError, function._div, 1, 0) # (/ 1 0) => Error
        self.assertRaisesRegex(TypeError, "The value must be NUMBER.", function._div, "a")  # (/ "a") => Error

    def test_eq(self):
        _a = Symbol('a')
        _b = Symbol('b')

        self.assertTrue(function._eq(_a, _a))  # (eq 'a 'a) => T
        self.assertFalse(function._eq(_a, _b)) # (eq 'a 'b) => NIL
        self.assertTrue(function._eq(3, 3))    # (eq 3 3) => T
        self.assertFalse(function._eq(3, 3.0)) # (eq 3 3.0) => NIL

        # (eq (cons 'a nil) (cons 'a nil)) => NIL
        self.assertFalse(function._eq(function._cons(_a, False), function._cons(_a, False)))

    def test_eql(self):
        _a = Symbol('a')
        _b = Symbol('b')

        self.assertTrue(function._eql(_a, _a))  # (eql 'a 'a) => T
        self.assertFalse(function._eql(_a, _b)) # (eql 'a 'b) => NIL
        self.assertTrue(function._eql(3, 3))    # (eql 3 3) => T
        self.assertFalse(function._eql(3, 3.0)) # (eql 3 3.0) => NIL

        # (eql (cons 'a nil) (cons 'a nil)) => T
        self.assertTrue(function._eql(function._cons(_a, False), function._cons(_a, False)))

    def test_not(self):
        self.assertTrue(function._not(False)) # (not nil) => T
        self.assertTrue(function._not([]))    # (not '()) => T
        self.assertFalse(function._not(function._numberp(1))) # (not (numberp 1)) => NIL

    def test_numberp(self):
        self.assertTrue(function._numberp(1))    # (numberp 1) => T
        self.assertTrue(function._numberp(1.0))  # (numberp 1.0) => T
        self.assertFalse(function._numberp('a')) # (numberp 'a) => NIL

    def test_gt(self):
        self.assertTrue(function._gt(1))        # (> 1) => T
        self.assertTrue(function._gt(2, 1))     # (> 2 1) => T
        self.assertTrue(function._gt(3, 2, 1))  # (> 3 2 1) => T
        self.assertFalse(function._gt(1, 2))    # (> 1 2) => NIL
        self.assertFalse(function._gt(3, 1, 2)) # (> 3 1 2) => NIL
        self.assertRaisesRegex(TypeError, "At least 1 expected.", function._gt)            # (>) => Error
        self.assertRaisesRegex(TypeError, "The value must be NUMBER.", function._gt, "a")  # (> "a") => Error

    def test_lt(self):
        self.assertTrue(function._lt(1))        # (< 1) => T
        self.assertTrue(function._lt(1, 2))     # (< 1 2) => T
        self.assertTrue(function._lt(1, 2, 3))  # (< 1 2 3) => T
        self.assertFalse(function._lt(2, 1))    # (< 2 1) => NIL
        self.assertFalse(function._lt(2, 3, 1)) # (< 2 3 1) => NIL
        self.assertRaisesRegex(TypeError, "At least 1 expected.", function._lt)            # (>) => Error
        self.assertRaisesRegex(TypeError, "The value must be NUMBER.", function._lt, "a")  # (> "a") => Error

    def test_ge(self):
        self.assertTrue(function._ge(1))        # (>= 1) => T
        self.assertTrue(function._ge(2, 1))     # (>= 2 1) => T
        self.assertTrue(function._ge(3, 2, 1))  # (>= 3 2 1) => T
        self.assertTrue(function._ge(3, 3, 3))  # (>= 3 3 3) => T
        self.assertFalse(function._ge(1, 2))    # (>= 1 2) => NIL
        self.assertFalse(function._ge(3, 1, 2)) # (>= 3 1 2) => NIL
        self.assertRaisesRegex(TypeError, "At least 1 expected.", function._ge)            # (>=) => Error
        self.assertRaisesRegex(TypeError, "The value must be NUMBER.", function._ge, "a")  # (>= "a") => Error

    def test_lt(self):
        self.assertTrue(function._le(1))        # (<= 1) => T
        self.assertTrue(function._le(1, 2))     # (<= 1 2) => T
        self.assertTrue(function._le(1, 2, 3))  # (<= 1 2 3) => T
        self.assertTrue(function._le(1, 1, 1))  # (<= 1 1 1) => T
        self.assertFalse(function._le(2, 1))    # (<= 2 1) => NIL
        self.assertFalse(function._le(2, 3, 1)) # (<= 2 3 1) => NIL
        self.assertRaisesRegex(TypeError, "At least 1 expected.", function._le)            # (>=) => Error
        self.assertRaisesRegex(TypeError, "The value must be NUMBER.", function._le, "a")  # (>= "a") => Error

    def test_aeq(self):
        self.assertTrue(function._neq(1))       # (= 1) => T
        self.assertTrue(function._neq(1, 1))    # (= 1 1) => T
        self.assertTrue(function._neq(1, 1.0))  # (= 1 1.0) => T
        self.assertFalse(function._neq(1, 2))   # (= 1 2) => NIL
        self.assertRaisesRegex(TypeError, "At least 1 expected.", function._neq)            # (=) => Error
        self.assertRaisesRegex(TypeError, "The value must be NUMBER.", function._neq, "a")  # (= "a") => Error

    def test_max(self):
        self.assertEqual(function._max(1), 1)       # (max 1) => 1
        self.assertEqual(function._max(1, 2), 2)    # (max 1 2) => 2
        self.assertEqual(function._max(1, 2, 3), 3) # (max 1 2 3) => 3
        self.assertRaisesRegex(TypeError, "The value must be NUMBER.", function._mul, "a")  # (max "a") => Error

    def test_min(self):
        self.assertEqual(function._min(1), 1)       # (min 1) => 1
        self.assertEqual(function._min(1, 2), 1)    # (min 1 2) => 1
        self.assertEqual(function._min(1, 2, 3), 1) # (min 1 2 3) => 1
        self.assertRaisesRegex(TypeError, "The value must be NUMBER.", function._mul, "a")  # (min "a") => Error
