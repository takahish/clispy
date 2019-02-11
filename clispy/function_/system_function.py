# Copyright 2019 Takahiro Ishikawa. All Rights Reserved.
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

from clispy.function_ import Function
from clispy.package import assign_helper
from clispy.type import Cons, Integer, Null, Ratio


# ==============================================================================
# Defines base classes.
#
#     SystemFunction
# ==============================================================================

class SystemFunction(Function):
    """SystemFunction provide some functions like defun, do, car or cdr etc.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates SystemFunction.
        """
        cls.__name__ = 'SYSTEM-FUNCTION'
        return object.__new__(cls)

    def __repr__(self):
        """The official string representation.
        """
        return "#<SYSTEM-FUNCTION {0} {{{1:X}}}>".format(self.__class__.__name__, id(self))


# ==============================================================================
# Defines system function classes.
# ==============================================================================

class Cons_(SystemFunction):
    """Creates a fresh cons, the car of which is object-1 and the cdr of which is object-2.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates Cons.
        """
        cls.__name__ = 'CONS'
        return object.__new__(cls)

    def __call__(self, args):
        """Behavior of Cons.
        """
        return Cons(args.car, args.cdr.car)


class Car(SystemFunction):
    """If x is a cons, car returns the car of that cons. If x is nil, car returns nil.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates Car.
        """
        cls.__name__ = 'CAR'
        return object.__new__(cls)

    def __call__(self, args):
        """Behavior of Car.
        """
        return args.car.car


class Cdr(SystemFunction):
    """If x is a cons, cdr returns the cdr of that cons. If x is nil, cdr returns nil.
    """
    def __new__(cls, *args, **kwargs):
        cls.__name__ = 'CDR'
        return object.__new__(cls)

    def __call__(self, args):
        """Behavior of Cdr.
        """
        return args.car.cdr


class Add(SystemFunction):
    """Returns the sum of numbers, performing any necessary type conversions in the process.
    If no numbers are supplied, 0 is returned.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates Add.
        """
        cls.__name__ = '+'
        return object.__new__(cls)

    def __call__(self, args):
        """Behavior of Add.
        """
        # Initializes retval.
        retval = Integer(0)

        # The sum of numbers.
        while args is not Null():
            retval = retval + args.car
            args = args.cdr

        return retval


class Sub(SystemFunction):
    """The function - performs arithmetic subtraction and negation.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates Sub.
        """
        cls.__name__ = '-'
        return object.__new__(cls)

    def __call__(self, args):
        """Behavior of Sub.
        """
        # Initializes retval.
        retval, args = args.car, args.cdr

        # Subtraction.
        while args is not Null():
            retval = retval - args.car
            args = args.cdr

        return retval


class Mul(SystemFunction):
    """Returns the product of numbers, performing any necessary type conversions in the process.
    If no numbers are supplied, 1 is returned.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates Mul.
        """
        cls.__name__ = '*'
        return object.__new__(cls)

    def __call__(self, args):
        """Behavior of Mul.
        """
        # Initializes retval.
        retval = Integer(1)

        # The product of numbers.
        while args is not Null():
            retval = retval * args.car
            args = args.cdr

        return retval


class Div(SystemFunction):
    """The function / performs division or reciprocation.
    """
    def __new__(cls, *args, **kwargs):
        """Intantiates Div.
        """
        cls.__name__ = '/'
        return object.__new__(cls)

    def __call__(self, args):
        """Behavior of Div.
        """
        # Initializes retval.
        retval, args = args.car, args.cdr

        # If there is not an argument any more, returns ratio.
        if args is Null():
            return Ratio('1/{}'.format(str(retval)))

        # Division.
        while args is not Null():
            retval = retval / args.car
            args = args.cdr

        return retval


# ==============================================================================
# Set functions related on special operators
# ==============================================================================

assign_helper(symbol_name='CONS', value=Cons_(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='CAR', value=Car(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='CDR', value=Cdr(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='+', value=Add(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='-', value=Sub(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='*', value=Mul(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='/', value=Div(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
