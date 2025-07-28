# Copyright 2025 Takahiro Ishikawa. All Rights Reserved.
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
import os
from clispy.type.basecls import Symbol
from clispy.type.number import *


class UnitTestCase(unittest.TestCase):
    def testNumberObjectRegistry(self):
        n1 = Number()
        n2 = Number()

        self.assertTrue(n1 is n2)

    def testNumber(self):
        n = Number()

        self.assertIsInstance(n, T)
        self.assertIsInstance(n, Number)
        self.assertEqual(str(n), 'T')

    def testNumberTypeOf(self):
        n_t = Number().type_of()

        self.assertIsInstance(n_t, Symbol)
        self.assertEqual(n_t.value, 'NUMBER')

    def testRealObjectRegistry(self):
        r1 = Real()
        r2 = Real()

        self.assertTrue(r1 is r2)

    def testReal(self):
        r = Real()

        self.assertIsInstance(r, T)
        self.assertIsInstance(r, Number)
        self.assertIsInstance(r, Real)
        self.assertEqual(str(r), 'T')

    def testRealType(self):
        r_t = Real().type_of()

        self.assertIsInstance(r_t, Symbol)
        self.assertEqual(r_t.value, 'REAL')

    def testRationalObjectRegistry(self):
        r1 = Rational()
        r2 = Rational()

        self.assertTrue(r1 is r2)

    def testRational(self):
        r = Rational()

        self.assertIsInstance(r, T)
        self.assertIsInstance(r, Number)
        self.assertIsInstance(r, Real)
        self.assertIsInstance(r, Rational)
        self.assertEqual(str(r), 'T')

    def testRationalType(self):
        r_t = Rational().type_of()

        self.assertIsInstance(r_t, Symbol)
        self.assertEqual(r_t.value, 'RATIONAL')

    def testIntegerObjectRegistry(self):
        i1 = Integer(10)
        i2 = Integer(10)
        i3 = Integer(20)

        self.assertTrue(i1 is i2)
        self.assertFalse(i1 is i3)

    def testInteger(self):
        i = Integer(100)

        self.assertIsInstance(i, T)
        self.assertIsInstance(i, Number)
        self.assertIsInstance(i, Real)
        self.assertIsInstance(i, Rational)
        self.assertIsInstance(i, Integer)
        self.assertEqual(str(i), '100')
        self.assertIsInstance(i.value, int)
        self.assertEqual(i.value, int(100))

    def testIntegerType(self):
        i_t = Integer(100).type_of()

        self.assertIsInstance(i_t, Symbol)
        self.assertEqual(i_t.value, 'INTEGER')

    def testFixnumObjectRegistry(self):
        f1 = Fixnum(10)
        f2 = Fixnum(10)
        f3 = Fixnum(20)

        self.assertTrue(f1 is f2)
        self.assertFalse(f1 is f3)

    def testFixnum(self):
        f = Fixnum(100)

        self.assertIsInstance(f, T)
        self.assertIsInstance(f, Number)
        self.assertIsInstance(f, Real)
        self.assertIsInstance(f, Rational)
        self.assertIsInstance(f, Integer)
        self.assertIsInstance(f, Fixnum)
        self.assertEqual(str(f), '100')
        self.assertIsInstance(f.value, np.int16)
        self.assertEqual(f.value, np.int16(100))

    def testFixnumTypeOf(self):
        f_t = Fixnum(100).type_of()

        self.assertIsInstance(f_t, Symbol)
        self.assertEqual(f_t.value, 'FIXNUM')

    def testBignumObjectRegistry(self):
        b1 = Bignum(10)
        b2 = Bignum(10)
        b3 = Bignum(20)

        self.assertTrue(b1 is b2)
        self.assertFalse(b1 is b3)

    def testBignum(self):
        b = Bignum(100)

        self.assertIsInstance(b, T)
        self.assertIsInstance(b, Number)
        self.assertIsInstance(b, Real)
        self.assertIsInstance(b, Rational)
        self.assertIsInstance(b, Integer)
        self.assertIsInstance(b, Bignum)
        self.assertEqual(str(b), '100')
        self.assertIsInstance(b.value, int)
        self.assertEqual(b.value, int(100))

    def testBignumTypeOf(self):
        b_t = Bignum(100).type_of()

        self.assertIsInstance(b_t, Symbol)
        self.assertEqual(b_t.value, 'BIGNUM')

    def testRatioObjectRegistry(self):
        r1 = Ratio('1/3')
        r2 = Ratio('1/3')
        r3 = Ratio('2/3')

        self.assertTrue(r1 is r2)
        self.assertFalse(r1 is r3)

    def testRaito(self):
        r = Ratio('2/3')

        self.assertIsInstance(r, T)
        self.assertIsInstance(r, Number)
        self.assertIsInstance(r, Real)
        self.assertIsInstance(r, Rational)
        self.assertIsInstance(r, Ratio)
        self.assertEqual(str(r), '2/3')
        self.assertEqual(r.value, Fraction(2, 3))

    def testRatioTypeOf(self):
        r_t = Ratio('2/3').type_of()

        self.assertIsInstance(r_t, Symbol)
        self.assertEqual(r_t.value, 'RATIO')

    def testFloatObjectRegistry(self):
        f1 = Float(10)
        f2 = Float(10)
        f3 = Float(20)

        self.assertTrue(f1 is f2)
        self.assertFalse(f1 is f3)

    def testFloat(self):
        f = Float(100)

        self.assertIsInstance(f, T)
        self.assertIsInstance(f, Number)
        self.assertIsInstance(f, Real)
        self.assertIsInstance(f, Float)
        self.assertEqual(str(f), '100.0')
        self.assertIsInstance(f.value, float)
        self.assertEqual(f.value, float(100))

    def testFloatTypeOf(self):
        f_t = Float(100).type_of()

        self.assertIsInstance(f_t, Symbol)
        self.assertEqual(f_t.value, 'FLOAT')

    def testShortFloatObjectRegistry(self):
        f1 = ShortFloat(10)
        f2 = ShortFloat(10)
        f3 = ShortFloat(20)

        self.assertTrue(f1 is f2)
        self.assertFalse(f1 is f3)

    def testShortFloat(self):
        f = ShortFloat(100)

        self.assertIsInstance(f, T)
        self.assertIsInstance(f, Number)
        self.assertIsInstance(f, Real)
        self.assertIsInstance(f, Float)
        self.assertIsInstance(f, ShortFloat)
        self.assertEqual(str(f), '100.0')
        self.assertIsInstance(f.value, np.float16)
        self.assertEqual(f.value, np.float16(100))

    def testShortFloatTypeOf(self):
        f_t = ShortFloat(100).type_of()

        self.assertIsInstance(f_t, Symbol)
        self.assertEqual(f_t.value, 'SHORT-FLOAT')

    def testSingleFloatObjectRegistry(self):
        f1 = SingleFloat(10)
        f2 = SingleFloat(10)
        f3 = SingleFloat(20)

        self.assertTrue(f1 is f2)
        self.assertFalse(f1 is f3)

    def testSingleFloat(self):
        f = SingleFloat(100)

        self.assertIsInstance(f, T)
        self.assertIsInstance(f, Number)
        self.assertIsInstance(f, Real)
        self.assertIsInstance(f, Float)
        self.assertIsInstance(f, SingleFloat)
        self.assertEqual(str(f), '100.0')
        self.assertIsInstance(f.value, np.float32)
        self.assertEqual(f.value, np.float32(100))

    def testSingleFloatTypeOf(self):
        f_t = SingleFloat(100).type_of()

        self.assertIsInstance(f_t, Symbol)
        self.assertEqual(f_t.value, 'SINGLE-FLOAT')

    def testDoubleFloatObjectRegistry(self):
        f1 = DoubleFloat(10)
        f2 = DoubleFloat(10)
        f3 = DoubleFloat(20)

        self.assertTrue(f1 is f2)
        self.assertFalse(f1 is f3)

    def testDoubleFloat(self):
        f = DoubleFloat(100)

        self.assertIsInstance(f, T)
        self.assertIsInstance(f, Number)
        self.assertIsInstance(f, Real)
        self.assertIsInstance(f, Float)
        self.assertIsInstance(f, DoubleFloat)
        self.assertEqual(str(f), '100.0')
        self.assertIsInstance(f.value, np.float64)
        self.assertEqual(f.value, np.float64(100))

    def testDoubleFloatTypeOf(self):
        f_t = DoubleFloat(100).type_of()

        self.assertIsInstance(f_t, Symbol)
        self.assertEqual(f_t.value, 'DOUBLE-FLOAT')

    def testLongFloatObjectRegistry(self):
        f1 = LongFloat(10)
        f2 = LongFloat(10)
        f3 = LongFloat(20)

        self.assertTrue(f1 is f2)
        self.assertFalse(f1 is f3)

    def testLongFloat(self):
        f = LongFloat(100)

        self.assertIsInstance(f, T)
        self.assertIsInstance(f, Number)
        self.assertIsInstance(f, Real)
        self.assertIsInstance(f, Float)
        self.assertIsInstance(f, LongFloat)
        self.assertEqual(str(f), '100.0')

        # numpy.float128 isn't supported on Windows using the MS compiler
        # https://github.com/winpython/winpython/issues/613
        # https://stackoverflow.com/questions/9062562/what-is-the-internal-precision-of-numpy-float128
        if os.name == 'nt':
            self.assertIsInstance(f.value, np.float64)
            self.assertEqual(f.value, np.float64(100))
        else:
            self.assertIsInstance(f.value, np.float128)
            self.assertEqual(f.value, np.float128(100))

    def testLongFloatTypeOf(self):
        f_t = LongFloat(100).type_of()

        self.assertIsInstance(f_t, Symbol)
        self.assertEqual(f_t.value, 'LONG-FLOAT')

    def testIntegerArithmetic(self):
        a = Integer(1)
        b = Integer(2)
        c = Ratio('2/3')
        d = Float(4)

        eps = 0.000000000000001

        i = a + b
        self.assertIsInstance(i, Integer)
        self.assertEqual(i.value, int(3))

        i = a - b
        self.assertIsInstance(i, Integer)
        self.assertEqual(i.value, int(-1))

        i = a * b
        self.assertIsInstance(i, Integer)
        self.assertEqual(i.value, int(2))

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
        self.assertTrue(abs(f.value - float(5.0)) < eps)

        f = a - d
        self.assertIsInstance(f, Float)
        self.assertTrue(abs(f.value - float(-3.0)) < eps)

        f = a * d
        self.assertIsInstance(f, Float)
        self.assertTrue(abs(f.value - float(4.0)) < eps)

        f = a / f
        self.assertIsInstance(f, Float)
        self.assertTrue(abs(f.value - float(0.25)) < eps)

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
        self.assertEqual(i.value, int(2))

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
        self.assertEqual(r.value, int(2))

        f = a + d
        self.assertIsInstance(f, Float)
        self.assertTrue(abs(f.value - float(3/2)) < eps)

        f = a - d
        self.assertIsInstance(f, Float)
        self.assertTrue(abs(f.value - float(-1/2)) < eps)

        f = a * d
        self.assertIsInstance(f, Float)
        self.assertTrue(abs(f.value - float(1/2)) < eps)

        f = a / d
        self.assertIsInstance(f, Float)
        self.assertTrue(abs(f.value - float(1/2)) < eps)

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
        self.assertTrue(abs(f.value - float(3.0)) < eps)

        f = a - b
        self.assertIsInstance(f, Float)
        self.assertTrue(abs(f.value - float(-1.0)) < eps)

        f = a * b
        self.assertIsInstance(f, Float)
        self.assertTrue(abs(f.value - float(2.0)) < eps)

        f = a / b
        self.assertIsInstance(f, Float)
        self.assertTrue(abs(f.value - float(0.5)) < eps)

        f = a + c
        self.assertIsInstance(f, Float)
        self.assertTrue(abs(f.value - float(5/3)) < eps)

        f = a - c
        self.assertIsInstance(f, Float)
        self.assertTrue(abs(f.value - float(1/3)) < eps)

        f = a * c
        self.assertIsInstance(f, Float)
        self.assertTrue(abs(f.value - float(2/3)) < eps)

        f = a / c
        self.assertIsInstance(f, Float)
        self.assertTrue(abs(f.value - float(1.5)) < eps)

        f = a + d
        self.assertIsInstance(f, Float)
        self.assertTrue(abs(f.value - float(5.0)) < eps)

        f = a - d
        self.assertIsInstance(f, Float)
        self.assertTrue(abs(f.value - float(-3.0)) < eps)

        f = a * d
        self.assertIsInstance(f, Float)
        self.assertTrue(abs(f.value - float(4.0)) < eps)

        f = a / f
        self.assertIsInstance(f, Float)
        self.assertTrue(abs(f.value - float(0.25)) < eps)

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
