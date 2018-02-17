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
from clispy import symbol
from clispy import cons
from clispy import func


class UnitTestCase(unittest.TestCase):
    def setUp(self):
        self.builtin_operator = func._BuiltInFunction()

    def test_null(self):
        self.assertTrue(func._null(False))      # (null nil) => T
        self.assertTrue(func._null([]))         # (null '()) => T
        self.assertFalse(func._null(1))         # (null 1) => NIL
        self.assertFalse(func._null([1]))       # (null '(1)) => NIL
        self.assertFalse(func._null([1, 2, 3])) # (null '(1 2 3)) => NIL

    def test_consp(self):
        self.assertTrue(func._consp([1]))       # (consp '(1)) => T
        self.assertTrue(func._consp([1, 2, 3])) # (consp '(1 2 3)) => T
        self.assertFalse(func._consp(False))    # (consp nil) => NIL
        self.assertFalse(func._consp(1))        # (consp 1) => NIL
        self.assertFalse(func._consp([]))       # (consp '()) => NIL

    def test_listp(self):
        self.assertTrue(func._listp(False))  # (listp nil) => T
        self.assertTrue(func._listp([1, 2])) # (listp '(1 2)) => T
        self.assertFalse(func._listp(1))     # (listp 1) = NIL

    def test_atom(self):
        self.assertTrue(func._atom('a'))       # (atom 'a) => T
        self.assertTrue(func._atom(False))     # (atom nil) => T
        self.assertTrue(func._atom([])) # (atom '()) => T
        self.assertTrue(func._atom(3))         # (atom 3) => T
        self.assertFalse(func._atom(func._cons(1, False))) # (atom (cons 1 nil)) => NIL

    def test_cons(self):
        self.assertEqual(func._cons(1, False), [1])        # (cons 1 nil) => (1)
        self.assertEqual(func._cons(1, []), [1])           # (cons 1 '()) => (1)
        self.assertEqual(func._cons(1, [2, 3]), [1, 2, 3]) # (cons 1 '(2 3)) => (1 2 3)

        # dotted pair
        self.assertIsInstance(func._cons(1, 2), cons._DottedPair)
        self.assertEqual(func._cons(1, 2), cons._DottedPair([1, 2])) # (cons 1 2) => (1 . 2)

    def test_car(self):
        self.assertEqual(func._car(False), False) # (car nil) => NIL
        self.assertEqual(func._car([]), False)    # (car '()) => NIL
        self.assertEqual(func._car([1]), 1)       # (car '(1)) => 1
        self.assertEqual(func._car([1, 2, 3]), 1) # (car '(1 2 3)) => 1
        self.assertEqual(func._car(cons._DottedPair([1, 2])), 1) # (car '(1 . 2)) => 1
        self.assertRaisesRegex(TypeError, "The value 1 is not LIST.", func._car, 1) # (car 1) => Error

    def test_cdr(self):
        self.assertEqual(func._cdr(False), False) # (cdr nil) => NIL
        self.assertEqual(func._cdr([]), False)    # (cdr '()) => NIL
        self.assertEqual(func._cdr([1]), False)   # (cdr '(1)) => NIL
        self.assertEqual(func._cdr([1, 2]), [2])  # (cdr '(1 2)) => (2)
        self.assertEqual(func._cdr(cons._DottedPair([1, 2])), 2) # (cdr '(1 . 2)) => 2

        # (cdr '(1 2 . 3)) => (2 . 3)
        self.assertIsInstance(func._cdr(cons._DottedPair([1, 2, 3])), cons._DottedPair)
        self.assertEqual(func._cdr(cons._DottedPair([1, 2, 3])), cons._DottedPair([2, 3]))
        self.assertRaisesRegex(TypeError, "The value 1 is not LIST.", func._cdr, 1) # (cdr 1) => Error

    def test_append(self):
        self.assertEqual(func._append(), False) # (append) => NIL
        self.assertEqual(func._append(1), 1)    # (append 1) => NIL

        # (append '(1) '(2) '() '(3)) => (1 2 3)
        self.assertEqual(func._append([1], [2], [], [3]), [1, 2, 3])
        # (append '(1) '(2) nil '(3)) => (1 2 3)
        self.assertEqual(func._append([1], [2], False, [3]), [1, 2, 3])

    def test_list(self):
        self.assertEqual(func._list(), False) # (list) => NIL
        self.assertEqual(func._list(1), [1])  # (list 1) => (1)
        self.assertEqual(func._list(1, 2, 3), [1, 2, 3]) # (list 1 2 3) => (1 2 3)

    def test_add(self):
        self.assertEqual(func._add(1), 1)       # (+ 1) => 1
        self.assertEqual(func._add(1, 2, 3), 6) # (+ 1 2 3) => 6
        self.assertRaises(TypeError, func._add) # (+) => Error
        self.assertRaisesRegex(TypeError, "The value must be NUMBER.", func._add, "a") # (+ "a") => Error

    def test_sub(self):
        self.assertEqual(func._sub(1), 1)        # (- 1) => 1
        self.assertEqual(func._sub(1, 2, 3), -4) # (- 1 2 3) => -4
        self.assertRaises(TypeError, func._sub)  # (-) => Error
        self.assertRaisesRegex(TypeError, "The value must be NUMBER.", func._sub, "a")  # (- "a") => Error

    def test_mul(self):
        self.assertEqual(func._mul(1), 1)       # (* 1) => 1
        self.assertEqual(func._mul(1, 2, 3), 6) # (* 1 2 3) => 6
        self.assertRaises(TypeError, func._mul) # (*) => Error
        self.assertRaisesRegex(TypeError, "The value must be NUMBER.", func._mul, "a")  # (* "a") => Error

    def test_div(self):
        self.assertEqual(func._div(1), 1)                     # (/ 1) => 1
        self.assertEqual(func._div(1, 2), 0.5)                # (/ 1 2) => 0.5
        self.assertRaises(TypeError, func._div)               # (/) => Error
        self.assertRaises(ZeroDivisionError, func._div, 1, 0) # (/ 1 0) => Error
        self.assertRaisesRegex(TypeError, "The value must be NUMBER.", func._div, "a")  # (/ "a") => Error

    def test_eq(self):
        _a = symbol._Symbol('a')
        _b = symbol._Symbol('b')

        self.assertTrue(func._eq(_a, _a))  # (eq 'a 'a) => T
        self.assertFalse(func._eq(_a, _b)) # (eq 'a 'b) => NIL
        self.assertTrue(func._eq(3, 3))    # (eq 3 3) => T
        self.assertFalse(func._eq(3, 3.0)) # (eq 3 3.0) => NIL

        # (eq (cons 'a nil) (cons 'a nil)) => NIL
        self.assertFalse(func._eq(func._cons(_a, False), func._cons(_a, False)))

    def test_eql(self):
        _a = symbol._Symbol('a')
        _b = symbol._Symbol('b')

        self.assertTrue(func._eql(_a, _a))  # (eql 'a 'a) => T
        self.assertFalse(func._eql(_a, _b)) # (eql 'a 'b) => NIL
        self.assertTrue(func._eql(3, 3))    # (eql 3 3) => T
        self.assertFalse(func._eql(3, 3.0)) # (eql 3 3.0) => NIL

        # (eql (cons 'a nil) (cons 'a nil)) => T
        self.assertTrue(func._eql(func._cons(_a, False), func._cons(_a, False)))

    def test_not(self):
        self.assertTrue(func._not(False)) # (not nil) => T
        self.assertTrue(func._not([]))    # (not '()) => T
        self.assertFalse(func._not(func._numberp(1))) # (not (numberp 1)) => NIL

    def test_numberp(self):
        self.assertTrue(func._numberp(1))    # (numberp 1) => T
        self.assertTrue(func._numberp(1.0))  # (numberp 1.0) => T
        self.assertFalse(func._numberp('a')) # (numberp 'a) => NIL

    def test_gt(self):
        self.assertTrue(func._gt(1))        # (> 1) => T
        self.assertTrue(func._gt(2, 1))     # (> 2 1) => T
        self.assertTrue(func._gt(3, 2, 1))  # (> 3 2 1) => T
        self.assertFalse(func._gt(1, 2))    # (> 1 2) => NIL
        self.assertFalse(func._gt(3, 1, 2)) # (> 3 1 2) => NIL
        self.assertRaisesRegex(TypeError, "At least 1 expected.", func._gt)            # (>) => Error
        self.assertRaisesRegex(TypeError, "The value must be NUMBER.", func._gt, "a")  # (> "a") => Error

    def test_lt(self):
        self.assertTrue(func._lt(1))        # (< 1) => T
        self.assertTrue(func._lt(1, 2))     # (< 1 2) => T
        self.assertTrue(func._lt(1, 2, 3))  # (< 1 2 3) => T
        self.assertFalse(func._lt(2, 1))    # (< 2 1) => NIL
        self.assertFalse(func._lt(2, 3, 1)) # (< 2 3 1) => NIL
        self.assertRaisesRegex(TypeError, "At least 1 expected.", func._lt)            # (>) => Error
        self.assertRaisesRegex(TypeError, "The value must be NUMBER.", func._lt, "a")  # (> "a") => Error

    def test_ge(self):
        self.assertTrue(func._ge(1))        # (>= 1) => T
        self.assertTrue(func._ge(2, 1))     # (>= 2 1) => T
        self.assertTrue(func._ge(3, 2, 1))  # (>= 3 2 1) => T
        self.assertTrue(func._ge(3, 3, 3))  # (>= 3 3 3) => T
        self.assertFalse(func._ge(1, 2))    # (>= 1 2) => NIL
        self.assertFalse(func._ge(3, 1, 2)) # (>= 3 1 2) => NIL
        self.assertRaisesRegex(TypeError, "At least 1 expected.", func._ge)            # (>=) => Error
        self.assertRaisesRegex(TypeError, "The value must be NUMBER.", func._ge, "a")  # (>= "a") => Error

    def test_lt(self):
        self.assertTrue(func._le(1))        # (<= 1) => T
        self.assertTrue(func._le(1, 2))     # (<= 1 2) => T
        self.assertTrue(func._le(1, 2, 3))  # (<= 1 2 3) => T
        self.assertTrue(func._le(1, 1, 1))  # (<= 1 1 1) => T
        self.assertFalse(func._le(2, 1))    # (<= 2 1) => NIL
        self.assertFalse(func._le(2, 3, 1)) # (<= 2 3 1) => NIL
        self.assertRaisesRegex(TypeError, "At least 1 expected.", func._le)            # (>=) => Error
        self.assertRaisesRegex(TypeError, "The value must be NUMBER.", func._le, "a")  # (>= "a") => Error

    def test_aeq(self):
        self.assertTrue(func._neq(1))       # (= 1) => T
        self.assertTrue(func._neq(1, 1))    # (= 1 1) => T
        self.assertTrue(func._neq(1, 1.0))  # (= 1 1.0) => T
        self.assertFalse(func._neq(1, 2))   # (= 1 2) => NIL
        self.assertRaisesRegex(TypeError, "At least 1 expected.", func._neq)            # (=) => Error
        self.assertRaisesRegex(TypeError, "The value must be NUMBER.", func._neq, "a")  # (= "a") => Error

    def test_max(self):
        self.assertEqual(func._max(1), 1)       # (max 1) => 1
        self.assertEqual(func._max(1, 2), 2)    # (max 1 2) => 2
        self.assertEqual(func._max(1, 2, 3), 3) # (max 1 2 3) => 3
        self.assertRaisesRegex(TypeError, "The value must be NUMBER.", func._mul, "a")  # (max "a") => Error

    def test_min(self):
        self.assertEqual(func._min(1), 1)       # (min 1) => 1
        self.assertEqual(func._min(1, 2), 1)    # (min 1 2) => 1
        self.assertEqual(func._min(1, 2, 3), 1) # (min 1 2 3) => 1
        self.assertRaisesRegex(TypeError, "The value must be NUMBER.", func._mul, "a")  # (min "a") => Error
