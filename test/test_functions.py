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
from clispy import functions


class UnitTestCase(unittest.TestCase):
    def setUp(self):
        self.builtin_operator = functions.BuiltInFunction()

    def test_null(self):
        self.assertTrue(functions._null(False))      # (null nil) => T
        self.assertTrue(functions._null([]))         # (null '()) => T
        self.assertFalse(functions._null(1))         # (null 1) => NIL
        self.assertFalse(functions._null([1]))       # (null '(1)) => NIL
        self.assertFalse(functions._null([1, 2, 3])) # (null '(1 2 3)) => NIL

    def test_consp(self):
        self.assertTrue(functions._consp([1]))       # (consp '(1)) => T
        self.assertTrue(functions._consp([1, 2, 3])) # (consp '(1 2 3)) => T
        self.assertFalse(functions._consp(False))    # (consp nil) => NIL
        self.assertFalse(functions._consp(1))        # (consp 1) => NIL
        self.assertFalse(functions._consp([]))       # (consp '()) => NIL

    def test_listp(self):
        self.assertTrue(functions._listp(False))  # (listp nil) => T
        self.assertTrue(functions._listp([1, 2])) # (listp '(1 2)) => T
        self.assertFalse(functions._listp(1))     # (listp 1) = NIL

    def test_atom(self):
        self.assertTrue(functions._atom('a'))       # (atom 'a) => T
        self.assertTrue(functions._atom(False))     # (atom nil) => T
        self.assertTrue(functions._atom([])) # (atom '()) => T
        self.assertTrue(functions._atom(3))         # (atom 3) => T
        self.assertFalse(functions._atom(functions._cons(1, False))) # (atom (cons 1 nil)) => NIL

    def test_cons(self):
        self.assertEqual(functions._cons(1, False), [1])        # (cons 1 nil) => (1)
        self.assertEqual(functions._cons(1, []), [1])           # (cons 1 '()) => (1)
        self.assertEqual(functions._cons(1, [2, 3]), [1, 2, 3]) # (cons 1 '(2 3)) => (1 2 3)

        # dotted pair
        self.assertIsInstance(functions._cons(1, 2), DottedPair)
        self.assertEqual(functions._cons(1, 2), DottedPair([1, 2])) # (cons 1 2) => (1 . 2)
        self.assertIsInstance(functions._cons(1, DottedPair([2, 3])), DottedPair)
        self.assertEqual(functions._cons(1, DottedPair([2, 3])), DottedPair([1, 2, 3]))

    def test_car(self):
        self.assertEqual(functions._car(False), False) # (car nil) => NIL
        self.assertEqual(functions._car([]), False)    # (car '()) => NIL
        self.assertEqual(functions._car([1]), 1)       # (car '(1)) => 1
        self.assertEqual(functions._car([1, 2, 3]), 1) # (car '(1 2 3)) => 1
        self.assertEqual(functions._car(DottedPair([1, 2])), 1) # (car '(1 . 2)) => 1
        self.assertRaisesRegex(TypeError, "The value 1 is not LIST.", functions._car, 1) # (car 1) => Error

    def test_cdr(self):
        self.assertEqual(functions._cdr(False), False) # (cdr nil) => NIL
        self.assertEqual(functions._cdr([]), False)    # (cdr '()) => NIL
        self.assertEqual(functions._cdr([1]), False)   # (cdr '(1)) => NIL
        self.assertEqual(functions._cdr([1, 2]), [2])  # (cdr '(1 2)) => (2)
        self.assertEqual(functions._cdr(DottedPair([1, 2])), 2) # (cdr '(1 . 2)) => 2

        # (cdr '(1 2 . 3)) => (2 . 3)
        self.assertIsInstance(functions._cdr(DottedPair([1, 2, 3])), DottedPair)
        self.assertEqual(functions._cdr(DottedPair([1, 2, 3])), DottedPair([2, 3]))
        self.assertRaisesRegex(TypeError, "The value 1 is not LIST.", functions._cdr, 1) # (cdr 1) => Error

    def test_append(self):
        self.assertEqual(functions._append(), False) # (append) => NIL
        self.assertEqual(functions._append(1), 1)    # (append 1) => NIL

        # (append '(1) '(2) '() '(3)) => (1 2 3)
        self.assertEqual(functions._append([1], [2], [], [3]), [1, 2, 3])
        # (append '(1) '(2) nil '(3)) => (1 2 3)
        self.assertEqual(functions._append([1], [2], False, [3]), [1, 2, 3])

    def test_list(self):
        self.assertEqual(functions._list(), False) # (list) => NIL
        self.assertEqual(functions._list(1), [1])  # (list 1) => (1)
        self.assertEqual(functions._list(1, 2, 3), [1, 2, 3]) # (list 1 2 3) => (1 2 3)

    def test_add(self):
        self.assertEqual(functions._add(1), 1)       # (+ 1) => 1
        self.assertEqual(functions._add(1, 2, 3), 6) # (+ 1 2 3) => 6
        self.assertRaises(TypeError, functions._add) # (+) => Error
        self.assertRaisesRegex(TypeError, "The value must be NUMBER.", functions._add, "a") # (+ "a") => Error

    def test_sub(self):
        self.assertEqual(functions._sub(1), 1)        # (- 1) => 1
        self.assertEqual(functions._sub(1, 2, 3), -4) # (- 1 2 3) => -4
        self.assertRaises(TypeError, functions._sub)  # (-) => Error
        self.assertRaisesRegex(TypeError, "The value must be NUMBER.", functions._sub, "a")  # (- "a") => Error

    def test_mul(self):
        self.assertEqual(functions._mul(1), 1)       # (* 1) => 1
        self.assertEqual(functions._mul(1, 2, 3), 6) # (* 1 2 3) => 6
        self.assertRaises(TypeError, functions._mul) # (*) => Error
        self.assertRaisesRegex(TypeError, "The value must be NUMBER.", functions._mul, "a")  # (* "a") => Error

    def test_div(self):
        self.assertEqual(functions._div(1), 1)                     # (/ 1) => 1
        self.assertEqual(functions._div(1, 2), 0.5)                # (/ 1 2) => 0.5
        self.assertRaises(TypeError, functions._div)               # (/) => Error
        self.assertRaises(ZeroDivisionError, functions._div, 1, 0) # (/ 1 0) => Error
        self.assertRaisesRegex(TypeError, "The value must be NUMBER.", functions._div, "a")  # (/ "a") => Error

    def test_eq(self):
        _a = Symbol('a')
        _b = Symbol('b')

        self.assertTrue(functions._eq(_a, _a))  # (eq 'a 'a) => T
        self.assertFalse(functions._eq(_a, _b)) # (eq 'a 'b) => NIL
        self.assertTrue(functions._eq(3, 3))    # (eq 3 3) => T
        self.assertFalse(functions._eq(3, 3.0)) # (eq 3 3.0) => NIL

        # (eq (cons 'a nil) (cons 'a nil)) => NIL
        self.assertFalse(functions._eq(functions._cons(_a, False), functions._cons(_a, False)))

    def test_eql(self):
        _a = Symbol('a')
        _b = Symbol('b')

        self.assertTrue(functions._eql(_a, _a))  # (eql 'a 'a) => T
        self.assertFalse(functions._eql(_a, _b)) # (eql 'a 'b) => NIL
        self.assertTrue(functions._eql(3, 3))    # (eql 3 3) => T
        self.assertFalse(functions._eql(3, 3.0)) # (eql 3 3.0) => NIL

        # (eql (cons 'a nil) (cons 'a nil)) => T
        self.assertTrue(functions._eql(functions._cons(_a, False), functions._cons(_a, False)))

    def test_not(self):
        self.assertTrue(functions._not(False)) # (not nil) => T
        self.assertTrue(functions._not([]))    # (not '()) => T
        self.assertFalse(functions._not(functions._numberp(1))) # (not (numberp 1)) => NIL

    def test_numberp(self):
        self.assertTrue(functions._numberp(1))    # (numberp 1) => T
        self.assertTrue(functions._numberp(1.0))  # (numberp 1.0) => T
        self.assertFalse(functions._numberp('a')) # (numberp 'a) => NIL

    def test_gt(self):
        self.assertTrue(functions._gt(1))        # (> 1) => T
        self.assertTrue(functions._gt(2, 1))     # (> 2 1) => T
        self.assertTrue(functions._gt(3, 2, 1))  # (> 3 2 1) => T
        self.assertFalse(functions._gt(1, 2))    # (> 1 2) => NIL
        self.assertFalse(functions._gt(3, 1, 2)) # (> 3 1 2) => NIL
        self.assertRaisesRegex(TypeError, "At least 1 expected.", functions._gt)            # (>) => Error
        self.assertRaisesRegex(TypeError, "The value must be NUMBER.", functions._gt, "a")  # (> "a") => Error

    def test_lt(self):
        self.assertTrue(functions._lt(1))        # (< 1) => T
        self.assertTrue(functions._lt(1, 2))     # (< 1 2) => T
        self.assertTrue(functions._lt(1, 2, 3))  # (< 1 2 3) => T
        self.assertFalse(functions._lt(2, 1))    # (< 2 1) => NIL
        self.assertFalse(functions._lt(2, 3, 1)) # (< 2 3 1) => NIL
        self.assertRaisesRegex(TypeError, "At least 1 expected.", functions._lt)            # (>) => Error
        self.assertRaisesRegex(TypeError, "The value must be NUMBER.", functions._lt, "a")  # (> "a") => Error

    def test_ge(self):
        self.assertTrue(functions._ge(1))        # (>= 1) => T
        self.assertTrue(functions._ge(2, 1))     # (>= 2 1) => T
        self.assertTrue(functions._ge(3, 2, 1))  # (>= 3 2 1) => T
        self.assertTrue(functions._ge(3, 3, 3))  # (>= 3 3 3) => T
        self.assertFalse(functions._ge(1, 2))    # (>= 1 2) => NIL
        self.assertFalse(functions._ge(3, 1, 2)) # (>= 3 1 2) => NIL
        self.assertRaisesRegex(TypeError, "At least 1 expected.", functions._ge)            # (>=) => Error
        self.assertRaisesRegex(TypeError, "The value must be NUMBER.", functions._ge, "a")  # (>= "a") => Error

    def test_lt(self):
        self.assertTrue(functions._le(1))        # (<= 1) => T
        self.assertTrue(functions._le(1, 2))     # (<= 1 2) => T
        self.assertTrue(functions._le(1, 2, 3))  # (<= 1 2 3) => T
        self.assertTrue(functions._le(1, 1, 1))  # (<= 1 1 1) => T
        self.assertFalse(functions._le(2, 1))    # (<= 2 1) => NIL
        self.assertFalse(functions._le(2, 3, 1)) # (<= 2 3 1) => NIL
        self.assertRaisesRegex(TypeError, "At least 1 expected.", functions._le)            # (>=) => Error
        self.assertRaisesRegex(TypeError, "The value must be NUMBER.", functions._le, "a")  # (>= "a") => Error

    def test_aeq(self):
        self.assertTrue(functions._neq(1))       # (= 1) => T
        self.assertTrue(functions._neq(1, 1))    # (= 1 1) => T
        self.assertTrue(functions._neq(1, 1.0))  # (= 1 1.0) => T
        self.assertFalse(functions._neq(1, 2))   # (= 1 2) => NIL
        self.assertRaisesRegex(TypeError, "At least 1 expected.", functions._neq)            # (=) => Error
        self.assertRaisesRegex(TypeError, "The value must be NUMBER.", functions._neq, "a")  # (= "a") => Error

    def test_max(self):
        self.assertEqual(functions._max(1), 1)       # (max 1) => 1
        self.assertEqual(functions._max(1, 2), 2)    # (max 1 2) => 2
        self.assertEqual(functions._max(1, 2, 3), 3) # (max 1 2 3) => 3
        self.assertRaisesRegex(TypeError, "The value must be NUMBER.", functions._mul, "a")  # (max "a") => Error

    def test_min(self):
        self.assertEqual(functions._min(1), 1)       # (min 1) => 1
        self.assertEqual(functions._min(1, 2), 1)    # (min 1 2) => 1
        self.assertEqual(functions._min(1, 2, 3), 1) # (min 1 2 3) => 1
        self.assertRaisesRegex(TypeError, "The value must be NUMBER.", functions._mul, "a")  # (min "a") => Error
