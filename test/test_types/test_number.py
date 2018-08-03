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
from clispy.types.number import *


class UnitTestCase(unittest.TestCase):
    def testNumber(self):
        n = Number()
        self.assertIsInstance(n, T)
        self.assertIsInstance(n, Number)

    def testNumberCheck(self):
        self.assertRaisesRegex(TypeError, "The value True is not of type clispy.types.Number", Number.check_type, True)

    def testReal(self):
        r = Real()
        self.assertIsInstance(r, T)
        self.assertIsInstance(r, Number)
        self.assertIsInstance(r, Real)

    def testRational(self):
        r = Rational()
        self.assertIsInstance(r, T)
        self.assertIsInstance(r, Number)
        self.assertIsInstance(r, Real)
        self.assertIsInstance(r, Rational)

    def testInteger(self):
        i = Integer(100)
        self.assertIsInstance(i, T)
        self.assertIsInstance(i, Number)
        self.assertIsInstance(i, Real)
        self.assertIsInstance(i, Rational)
        self.assertIsInstance(i, Integer)
        self.assertIsInstance(i.value, np.int)
        self.assertEqual(i.value, np.int(100))

    def testFixnum(self):
        f = Fixnum(100)
        self.assertIsInstance(f, T)
        self.assertIsInstance(f, Number)
        self.assertIsInstance(f, Real)
        self.assertIsInstance(f, Rational)
        self.assertIsInstance(f, Integer)
        self.assertIsInstance(f, Fixnum)
        self.assertIsInstance(f.value, np.int16)
        self.assertEqual(f.value, np.int16(100))

    def testBignum(self):
        b = Bignum(100)
        self.assertIsInstance(b, T)
        self.assertIsInstance(b, Number)
        self.assertIsInstance(b, Real)
        self.assertIsInstance(b, Rational)
        self.assertIsInstance(b, Integer)
        self.assertIsInstance(b, Bignum)
        self.assertIsInstance(b.value, np.int)
        self.assertEqual(b.value, np.int(100))

    def testRaito(self):
        r = Ratio('2/3')
        self.assertIsInstance(r, T)
        self.assertIsInstance(r, Number)
        self.assertIsInstance(r, Real)
        self.assertIsInstance(r, Rational)
        self.assertIsInstance(r, Ratio)
        self.assertEqual(r.value, Fraction(2, 3))

    def testFloat(self):
        f = Float(100)
        self.assertIsInstance(f, T)
        self.assertIsInstance(f, Number)
        self.assertIsInstance(f, Real)
        self.assertIsInstance(f, Float)
        self.assertIsInstance(f.value, np.float)
        self.assertEqual(f.value, np.float(100))

    def testShortFloat(self):
        f = ShortFloat(100)
        self.assertIsInstance(f, T)
        self.assertIsInstance(f, Number)
        self.assertIsInstance(f, Real)
        self.assertIsInstance(f, Float)
        self.assertIsInstance(f, ShortFloat)
        self.assertIsInstance(f.value, np.float16)
        self.assertEqual(f.value, np.float16(100))

    def testSingleFloat(self):
        f = SingleFloat(100)
        self.assertIsInstance(f, T)
        self.assertIsInstance(f, Number)
        self.assertIsInstance(f, Real)
        self.assertIsInstance(f, Float)
        self.assertIsInstance(f, SingleFloat)
        self.assertIsInstance(f.value, np.float32)
        self.assertEqual(f.value, np.float32(100))

    def testDoubleFloat(self):
        f = DoubleFloat(100)
        self.assertIsInstance(f, T)
        self.assertIsInstance(f, Number)
        self.assertIsInstance(f, Real)
        self.assertIsInstance(f, Float)
        self.assertIsInstance(f, DoubleFloat)
        self.assertIsInstance(f.value, np.float64)
        self.assertEqual(f.value, np.float64(100))

    def testLongFloat(self):
        f = LongFloat(100)
        self.assertIsInstance(f, T)
        self.assertIsInstance(f, Number)
        self.assertIsInstance(f, Real)
        self.assertIsInstance(f, Float)
        self.assertIsInstance(f, LongFloat)
        self.assertIsInstance(f.value, np.float128)
        self.assertEqual(f.value, np.float128(100))

    def testIntegerArithmetic(self):
        a = Integer(1)
        b = Integer(2)
        c = Ratio('2/3')
        d = Float(4)

        eps = 0.000000000000001

        i = a + b
        self.assertIsInstance(i, Integer)
        self.assertEqual(i.value, np.int(3))

        i = a - b
        self.assertIsInstance(i, Integer)
        self.assertEqual(i.value, np.int(-1))

        i = a * b
        self.assertIsInstance(i, Integer)
        self.assertEqual(i.value, np.int(2))

        r = a / b
        self.assertIsInstance(r, Ratio)
        self.assertEqual(r.value, Fraction(1, 2))

        r = a + c
        self.assertIsInstance(r, Ratio)
        self.assertEqual(r.value, Fraction(5, 3))

        r = a - c
        self.assertIsInstance(r, Ratio)
        self.assertEqual(r.value, Fraction(1, 3))

        r = a * c
        self.assertIsInstance(r, Ratio)
        self.assertEqual(r.value, Fraction(2, 3))

        r = a / c
        self.assertIsInstance(r, Ratio)
        self.assertEqual(r.value, Fraction(3, 2))

        f = a + d
        self.assertIsInstance(f, Float)
        self.assertTrue(abs(f.value - np.float(5.0)) < eps)

        f = a - d
        self.assertIsInstance(f, Float)
        self.assertTrue(abs(f.value - np.float(-3.0)) < eps)

        f = a * d
        self.assertIsInstance(f, Float)
        self.assertTrue(abs(f.value - np.float(4.0)) < eps)

        f = a / f
        self.assertIsInstance(f, Float)
        self.assertTrue(abs(f.value - np.float(0.25)) < eps)

    def testIntegerArithmetic_skeleton(self):
        a = Fixnum(1)
        b = Fixnum(2)
        c = Ratio('2/3')
        d = SingleFloat(4)

        eps = 0.000001

        i = a + b
        self.assertIsInstance(i, Fixnum)
        self.assertEqual(i.value, np.int16(3))

        i = a - b
        self.assertIsInstance(i, Fixnum)
        self.assertEqual(i.value, np.int16(-1))

        i = a * b
        self.assertIsInstance(i, Fixnum)
        self.assertEqual(i.value, np.int16(2))

        r = a / b
        self.assertIsInstance(r, Ratio)
        self.assertEqual(r.value, Fraction(1, 2))

        r = a + c
        self.assertIsInstance(r, Ratio)
        self.assertEqual(r.value, Fraction(5, 3))

        r = a - c
        self.assertIsInstance(r, Ratio)
        self.assertEqual(r.value, Fraction(1, 3))

        r = a * c
        self.assertIsInstance(r, Ratio)
        self.assertEqual(r.value, Fraction(2, 3))

        r = a / c
        self.assertIsInstance(r, Ratio)
        self.assertEqual(r.value, Fraction(3, 2))

        f = a + d
        self.assertIsInstance(f, SingleFloat)
        self.assertTrue(abs(f.value - np.float32(5.0)) < eps)

        f = a - d
        self.assertIsInstance(f, SingleFloat)
        self.assertTrue(abs(f.value - np.float32(-3.0)) < eps)

        f = a * d
        self.assertIsInstance(f, SingleFloat)
        self.assertTrue(abs(f.value - np.float32(4.0)) < eps)

        f = a / f
        self.assertIsInstance(f, SingleFloat)
        self.assertTrue(abs(f.value - np.float32(0.25)) < eps)

    def testIntegerCompare(self):
        a = Integer('10')
        b = Integer('20')
        c = Integer('10')

        self.assertTrue(a == c)
        self.assertFalse(a == b)

        self.assertTrue(a != b)
        self.assertFalse(a != c)

        self.assertTrue(a < b)
        self.assertFalse(b < a)
        self.assertFalse(a < c)

        self.assertTrue(a <= b)
        self.assertFalse(b <= a)
        self.assertTrue(a <= c)

        self.assertTrue(b > a)
        self.assertFalse(a > b)
        self.assertFalse(a > c)

        self.assertTrue(b >= a)
        self.assertFalse(a >= b)
        self.assertTrue(a >= c)

    def testRatioArithmetic(self):
        a = Ratio('1/2')
        b = Ratio('1/4')
        c = Integer('1')
        d = Float('1.0')

        eps = 0.000000000000001

        r = a + b
        self.assertIsInstance(r, Ratio)
        self.assertEqual(r.value, Fraction(3, 4))

        r = a - b
        self.assertIsInstance(r, Ratio)
        self.assertEqual(r.value, Fraction(1, 4))

        r = a * b
        self.assertIsInstance(r, Ratio)
        self.assertEqual(r.value, Fraction(1, 8))

        r = b / a
        self.assertIsInstance(r, Ratio)
        self.assertEqual(r.value, Fraction(1, 2))

        # Check returning Integer value.
        i = a / b
        self.assertIsInstance(i, Integer)
        self.assertEqual(i.value, np.int(2))

        r = a + c
        self.assertIsInstance(r, Ratio)
        self.assertEqual(r.value, Fraction(3, 2))

        r = a - c
        self.assertIsInstance(r, Ratio)
        self.assertEqual(r.value, Fraction(-1, 2))

        r = a * c
        self.assertIsInstance(r, Ratio)
        self.assertEqual(r.value, Fraction(1, 2))

        r = a / c
        self.assertIsInstance(r, Ratio)
        self.assertEqual(r.value, Fraction(1, 2))

        # Check returning Integer value.
        r = c / a
        self.assertIsInstance(r, Integer)
        self.assertEqual(r.value, np.int(2))

        f = a + d
        self.assertIsInstance(f, Float)
        self.assertTrue(abs(f.value - np.float(3/2)) < eps)

        f = a - d
        self.assertIsInstance(f, Float)
        self.assertTrue(abs(f.value - np.float(-1/2)) < eps)

        f = a * d
        self.assertIsInstance(f, Float)
        self.assertTrue(abs(f.value - np.float(1/2)) < eps)

        f = a / d
        self.assertIsInstance(f, Float)
        self.assertTrue(abs(f.value - np.float(1/2)) < eps)

    def testRatioCompare(self):
        a = Ratio('1/2')
        b = Ratio('2/3')
        c = Ratio('1/2')

        self.assertTrue(a == c)
        self.assertFalse(a == b)

        self.assertTrue(a != b)
        self.assertFalse(a != c)

        self.assertTrue(a < b)
        self.assertFalse(b < a)
        self.assertFalse(a < c)

        self.assertTrue(a <= b)
        self.assertFalse(b <= a)
        self.assertTrue(a <= c)

        self.assertTrue(b > a)
        self.assertFalse(a > b)
        self.assertFalse(a > c)

        self.assertTrue(b >= a)
        self.assertFalse(a >= b)
        self.assertTrue(a >= c)

    def testFloatArithmetic(self):
        a = Float('1.0')
        b = Float('2.0')
        c = Ratio('2/3')
        d = Integer('4')

        eps = 0.000000000000001

        f = a + b
        self.assertIsInstance(f, Float)
        self.assertTrue(abs(f.value - np.float(3.0)) < eps)

        f = a - b
        self.assertIsInstance(f, Float)
        self.assertTrue(abs(f.value - np.float(-1.0)) < eps)

        f = a * b
        self.assertIsInstance(f, Float)
        self.assertTrue(abs(f.value - np.float(2.0)) < eps)

        f = a / b
        self.assertIsInstance(f, Float)
        self.assertTrue(abs(f.value - np.float(0.5)) < eps)

        f = a + c
        self.assertIsInstance(f, Float)
        self.assertTrue(abs(f.value - np.float(5/3)) < eps)

        f = a - c
        self.assertIsInstance(f, Float)
        self.assertTrue(abs(f.value - np.float(1/3)) < eps)

        f = a * c
        self.assertIsInstance(f, Float)
        self.assertTrue(abs(f.value - np.float(2/3)) < eps)

        f = a / c
        self.assertIsInstance(f, Float)
        self.assertTrue(abs(f.value - np.float(1.5)) < eps)

        f = a + d
        self.assertIsInstance(f, Float)
        self.assertTrue(abs(f.value - np.float(5.0)) < eps)

        f = a - d
        self.assertIsInstance(f, Float)
        self.assertTrue(abs(f.value - np.float(-3.0)) < eps)

        f = a * d
        self.assertIsInstance(f, Float)
        self.assertTrue(abs(f.value - np.float(4.0)) < eps)

        f = a / f
        self.assertIsInstance(f, Float)
        self.assertTrue(abs(f.value - np.float(0.25)) < eps)

    def testFloatArithmetic_skeleton(self):
        a = SingleFloat('1.0')
        b = SingleFloat('2.0')
        c = Ratio('2/3')
        d = Fixnum('4')

        eps = 0.000001

        f = a + b
        self.assertIsInstance(f, SingleFloat)
        self.assertTrue(abs(f.value - np.float32(3.0)) < eps)

        f = a - b
        self.assertIsInstance(f, SingleFloat)
        self.assertTrue(abs(f.value - np.float32(-1.0)) < eps)

        f = a * b
        self.assertIsInstance(f, SingleFloat)
        self.assertTrue(abs(f.value - np.float32(2.0)) < eps)

        f = a / b
        self.assertIsInstance(f, SingleFloat)
        self.assertTrue(abs(f.value - np.float32(0.5)) < eps)

        f = a + c
        self.assertIsInstance(f, SingleFloat)
        self.assertTrue(abs(f.value - np.float32(5/3)) < eps)

        f = a - c
        self.assertIsInstance(f, SingleFloat)
        self.assertTrue(abs(f.value - np.float32(1/3)) < eps)

        f = a * c
        self.assertIsInstance(f, SingleFloat)
        self.assertTrue(abs(f.value - np.float32(2/3)) < eps)

        f = a / c
        self.assertIsInstance(f, SingleFloat)
        self.assertTrue(abs(f.value - np.float32(1.5)) < eps)

        f = a + d
        self.assertIsInstance(f, SingleFloat)
        self.assertTrue(abs(f.value - np.float32(5.0)) < eps)

        f = a - d
        self.assertIsInstance(f, SingleFloat)
        self.assertTrue(abs(f.value - np.float32(-3.0)) < eps)

        f = a * d
        self.assertIsInstance(f, SingleFloat)
        self.assertTrue(abs(f.value - np.float32(4.0)) < eps)

        f = a / f
        self.assertIsInstance(f, SingleFloat)
        self.assertTrue(abs(f.value - np.float32(0.25)) < eps)

    def testFloatCompare(self):
        a = Float('10.0')
        b = Float('20.0')
        c = Float('10.0')

        self.assertTrue(a == c)
        self.assertFalse(a == b)

        self.assertTrue(a != b)
        self.assertFalse(a != c)

        self.assertTrue(a < b)
        self.assertFalse(b < a)
        self.assertFalse(a < c)

        self.assertTrue(a <= b)
        self.assertFalse(b <= a)
        self.assertTrue(a <= c)

        self.assertTrue(b > a)
        self.assertFalse(a > b)
        self.assertFalse(a > c)

        self.assertTrue(b >= a)
        self.assertFalse(a >= b)
        self.assertTrue(a >= c)
