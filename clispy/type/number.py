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

import operator as op
import os
from fractions import Fraction
import numpy as np
from clispy.type.basecls import BuiltInClass, T


# ==============================================================================
# Defines number classes.
#
#     Number
#     Real
#     Rational
#     Integer
#     Fixnum
#     Bignum
#     Ratio
#     Float
#     ShortFloat
#     SingleFloat
#     DoubleFloat
#     LongFloat
# ==============================================================================


class Number(T):
    """The type Number contains objects which represent mathematical numbers.
    The type Real and Complex are disjoint subtypes of Number.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates Number. If an instance of Number is already existed
        in object_table, returns the instance. Otherwise, a new instance is made.
        """
        return BuiltInClass.get_instance(cls, 'NUMBER', True)

    def __eq__(self, other):
        """Hook of self == other.
        """
        return Number.__compare(op.eq, self, other)

    def __ne__(self, other):
        """Hook of self != other.
        """
        return Number.__compare(op.ne, self, other)

    def __lt__(self, other):
        """Hook of self < other.
        """
        return Number.__compare(op.lt, self, other)

    def __le__(self, other):
        """Hook of self <= other.
        """
        return Number.__compare(op.le, self, other)

    def __gt__(self, other):
        """Hook of self > other.
        """
        return Number.__compare(op.gt, self, other)

    def __ge__(self, other):
        """Hook of self >= other.
        """
        return Number.__compare(op.ge, self, other)

    @staticmethod
    def check_type(obj):
        """Check whether an object is an instance of Number.

        Args:
            obj: An object.
        """
        if not isinstance(obj, Number):
            raise TypeError("The value " + str(obj) + " is not of type clispy.type.Number")

    @staticmethod
    def __compare(operator, self, other):
        """Compare two numbers.
        """
        Number.check_type(other)
        return operator(self.value, other.value)


class Real(Number):
    """The type Real includes all numbers represent mathematical real numbers,
    though there are mathematical real numbers (e.g., irrational numbers)
    that do not have an exact representation in Common Lisp. Only can be
    ordered using the <, >, <=, and >= functions.
    The type Rational and Float are disjoint subtypes of type Real
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates Real. If an instance of Real is already existed
        in object_table, returns the instance. Otherwise, a new instance is made.
        """
        return BuiltInClass.get_instance(cls, 'REAL', True)


class Rational(Real):
    """The canonical representation of a rational is as an Integer if its
    value is integral, and otherwise as a Ratio.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates Rational. If an instance of Rational is already existed
        in object_table, returns the instance. Otherwise, a new instance is made.
        """
        return BuiltInClass.get_instance(cls, 'RATIONAL', True)


class Integer(Rational):
    """An integer is a mathematical integer. There is no limit on the
    magnitude of an integer.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates Integer. If an instance of Integer is already existed
        in object_table, returns the instance. Otherwise, a new instance is made.
        """
        return BuiltInClass.get_instance(cls, 'INTEGER', *args)

    def __init__(self, value):
        """Initializes Integer. A value is converted into np.int.

        Args:
            value: Int. It could be converted into np.int.
        """
        self.value = np.int(value)

    def __repr__(self):
        """The official string representation.
        """
        return str(self.value)

    def __add__(self, other):
        """Hook of self + other.
        """
        return Integer.__calculate(op.add, self, other)

    def __sub__(self, other):
        """Hook of self - other.
        """
        return Integer.__calculate(op.sub, self, other)

    def __mul__(self, other):
        """Hook of self * other.
        """
        return Integer.__calculate(op.mul, self, other)

    def __truediv__(self, other):
        """Hook of self / other.
        """
        if isinstance(other, Integer):
            return Ratio(str(self.value) + '/' + str(other.value))
        else:
            return Integer.__calculate(op.truediv, self, other)

    def __int__(self):
        """Hook of int(self).
        """
        return np.int(self.value)

    def __float__(self):
        """Hook of float(self).
        """
        return np.float(self.value)

    @staticmethod
    def __calculate(operator, self, other):
        """Calculate arithmetic.
        """
        Number.check_type(other)
        if isinstance(other, Ratio):
            fraction = operator(self.value, other.value)
            return Ratio(str(fraction))
        else:
            skeleton = type(other)
            return skeleton(operator(self.value, other.value))


class Fixnum(Integer):
    """Exactly which integers are fixnums is implementation-defined.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates Fixnum. If an instance of Fixnum is already existed
        in object_table, returns the instance. Otherwise, a new instance is made.
        """
        return BuiltInClass.get_instance(cls, 'FIXNUM', *args)

    def __init__(self, value):
        """Initializes Fixnum. A value is converted into np.int16.

        Args:
            value: Int. It could be converted into np.int16.
        """
        self.value = np.int16(value)


class Bignum(Integer):
    """The type bignum is defined to be exactly (and integer (not fixnum)).
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates Bignum. If an instance of Bignum is already existed
        in object_table, returns the instance. Otherwise, a new instance is made.
        """
        return BuiltInClass.get_instance(cls, 'BIGNUM', *args)

    def __init__(self, value):
        """Initializes Bignum. A value is converted into np.int.

        Args:
            value: Int. It could be converted into np.int.
        """
        self.value = np.int(value)


class Ratio(Rational):
    """A ratio is a number representing the mathematical ratio of two integers,
    the numerator and denominator, whose greatest common divisor is one, and
    of which the denominator is positive and greater than one.
    """
    def __new__(cls, ratio):
        """Calls to create a new instance of Ratio.

        Args:
            ratio: String

        Returns:
            An instance.
        """
        if '/' not in ratio:          # An argument is ['2'].
            return Integer(ratio[0])
        else:                         # An argument is ['10/5'].
            numerator, denominator = (int(i) for i in ratio.split('/'))
            if numerator % denominator == 0:
                return Integer(numerator // denominator)
            else:
                return BuiltInClass.get_instance(cls, 'RATIO', ratio)

    def __init__(self, ratio):
        """Initializes Ratio. A value is converted into Fraction.

        Args:
            numerator: Int.
            denominator: Int.
        """
        self.value = Fraction(ratio)

    def __repr__(self):
        """The official string representation.
        """
        return str(self.value)

    def __add__(self, other):
        """Hook of self + other.
        """
        return Ratio.__calculate(op.add, self, other)

    def __sub__(self, other):
        """Hook of self - other.
        """
        return Ratio.__calculate(op.sub, self, other)

    def __mul__(self, other):
        """Hook of self * other
        """
        return Ratio.__calculate(op.mul, self, other)

    def __truediv__(self, other):
        """Hook of self / other
        """
        return Ratio.__calculate(op.truediv, self, other)

    def __int__(self):
        """Hook of int(self).
        """
        return np.int(self.value)

    def __float__(self):
        """Hook of float(self).
        """
        return np.float(self.value)

    @staticmethod
    def __calculate(operator, self, other):
        """Calculate arithmetic.
        """
        Number.check_type(other)
        if isinstance(other, Float):
            skeleton = type(other)
            return skeleton(operator(self.value, other.value))
        else:
            fraction = operator(self.value, other.value)
            return Ratio(str(fraction))


class Float(Real):
    """A float is a mathematical rational (but not a Common Lisp Rational).
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates Float. If an instance of Float is already existed
        in object_table, returns the instance. Otherwise, a new instance is made.
        """
        return BuiltInClass.get_instance(cls, 'FLOAT', *args)

    def __init__(self, value):
        """Initializes Float. A value is converted into np.float.

        Args:
            value: Float.
        """
        self.value = np.float(value)

    def __repr__(self):
        """The official string representation.
        """
        return str(self.value)

    def __add__(self, other):
        """Hook of self + other.
        """
        return Float.__calculate(op.add, self, other)

    def __sub__(self, other):
        """Hook of self - other.
        """
        return Float.__calculate(op.sub, self, other)

    def __mul__(self, other):
        """Hook of self * other.
        """
        return Float.__calculate(op.mul, self, other)

    def __truediv__(self, other):
        """Hook of self / other.
        """
        return Float.__calculate(op.truediv, self, other)

    def __int__(self):
        """Hook of int(self).
        """
        return np.int(self.value)

    def __float__(self):
        """Hook of float(self).
        """
        return np.float(self.value)

    @staticmethod
    def __calculate(operator, self, other):
        """Calculate arithmetic.
        """
        Number.check_type(other)
        skeleton = type(self)
        return skeleton(operator(self.value, other.value))


class ShortFloat(Float):
    """For the four defined subtypes of type float, it is true that intermediate
    between the type short-float and the type long-float are the type single-float
    and the type double-float. The precise definition of these categories is
    implementation-defined.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates ShortFloat. If an instance of ShortFloat is already existed
        in object_table, returns the instance. Otherwise, a new instance is made.
        """
        return BuiltInClass.get_instance(cls, 'SHORT-FLOAT', *args)

    def __init__(self, value):
        """Initializes ShortFloat. A value is converted into np.float16.

        Args:
            value: Float.
        """
        self.value = np.float16(value)


class SingleFloat(Float):
    """For the four defined subtypes of type float, it is true that intermediate
    between the type short-float and the type long-float are the type single-float
    and the type double-float. The precise definition of these categories is
    implementation-defined.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates SingleFloat. If an instance of SingleFloat is already existed
        in object_table, returns the instance. Otherwise, a new instance is made.
        """
        return BuiltInClass.get_instance(cls, 'SINGLE-FLOAT', *args)

    def __init__(self, value):
        """Initializes SingleFloat. A value is converted into np.float32.

        Args:
            value: Float.
        """
        self.value = np.float32(value)


class DoubleFloat(Float):
    """For the four defined subtypes of type float, it is true that intermediate
    between the type short-float and the type long-float are the type single-float
    and the type double-float. The precise definition of these categories is
    implementation-defined.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates DoubleFloat. If an instance of DoubleFloat is already existed
        in object_table, returns the instance. Otherwise, a new instance is made.
        """
        return BuiltInClass.get_instance(cls, 'DOUBLE-FLOAT', *args)

    def __init__(self, value):
        """Initialize DoubleFloat. A value is converted into np.float64.

        Args:
            value: Float.
        """
        self.value = np.float64(value)


class LongFloat(Float):
    """For the four defined subtypes of type float, it is true that intermediate
    between the type short-float and the type long-float are the type single-float
    and the type double-float. The precise definition of these categories is
    implementation-defined.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates DoubleFloat. If an instance of DoubleFloat is already existed
        in object_table, returns the instance. Otherwise, a new instance is made.
        """
        return BuiltInClass.get_instance(cls, 'LONG-FLOAT', *args)

    def __init__(self, value):
        """Initializes DoubleFloat. A value is converted into np.float128.

        Args:
            value: Float.
        """
        # numpy.float128 isn't supported on Windows using the MS compiler
        # https://github.com/winpython/winpython/issues/613
        # https://stackoverflow.com/questions/9062562/what-is-the-internal-precision-of-numpy-float128
        if os.name == 'nt':
            self.value = np.float64(value)
        else:
            self.value = np.float128(value)
