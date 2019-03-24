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

from clispy.function import SystemFunction
from clispy.package import assign_helper
from clispy.python.pyobject import PyObject
from clispy.type import Cons, Null, T


# ==============================================================================
# Defines base classes.
#
#     BuiltinFunction
# ==============================================================================

class BuiltinFunction(SystemFunction):
    """The Python interpreter has a number of functions and types built into
    it that are always available. They are listed here in alphabetical order.

    #    abs         delattr   hash       memoryview set
    #    all         dict      help       min        setattr
    #    any         dir       hex        next       slice
    #    ascii       divmod    id         object     sorted
    #    bin         enumerate input      oct        staticmethod
    #    bool        eval      int        open       str
    #    breakpoint  exec      isinstance ord        sum
    #    bytearray   filter    issubclass pow        super
    #    bytes       float     iter       print      tuple
    #    callable    format    len        property   type
    #    chr         frozenset list       range      vars
    #    classmethod getattr   locals     repr       zip
    #    compile     globals   map        reversed   __import__
    #    complex     hasattr   max        round
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates BuiltinFunction.
        """
        cls.__name__ = 'BUILTIN-FUNCTION'
        return object.__new__(cls)

    def __repr__(self):
        """The official string representation.
        """
        return "#<BUILTIN-FUNCTION {0} {{{1:X}}}>".format(self.__class__.__name__, id(self))

    def exec(self, py_func_name, args):
        """Executes builtin function.
        """
        # Sets args for function.
        py_args = []
        while args is not Null():
            py_args.append(args.car.value)
            args = args.cdr

        return PyObject(__builtins__[py_func_name](*py_args))


# ==============================================================================
# Defines special operator classes.
#
#    abs         delattr   hash       memoryview set
#    all         dict      help       min        setattr
#    any         dir       hex        next       slice
#    ascii       divmod    id         object     sorted
#    bin         enumerate input      oct        staticmethod
#    bool        eval      int        open       str
#    breakpoint  exec      isinstance ord        sum
#    bytearray   filter    issubclass pow        super
#    bytes       float     iter       print      tuple
#    callable    format    len        property   type
#    chr         frozenset list       range      vars
#    classmethod getattr   locals     repr       zip
#    compile     globals   map        reversed   __import__
#    complex     hasattr   max        round
# ==============================================================================

class AbsBuiltinFunction(BuiltinFunction):
    """Return the absolute value of a number. The argument may be an integer
    or a floating point number. If the argument is a complex number, its
    magnitude is returned.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates AbsBuiltinFunction.
        """
        cls.__name__ = 'ABS'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of AbsBuiltinFunction.
        """
        args = self.eval_forms(forms, var_env, func_env, macro_env)
        return self.exec(py_func_name='abs', args=args)


class PrintBuiltinFunction(BuiltinFunction):
    """Print objects to the text stream file, separated by sep and followed
    by end. sep, end, file and flush, if present, must be given as keyword
    arguments.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates PrintBuiltinFunction.
        """
        cls.__name__ = 'PRINT'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of PrintBuiltinFunction.
        """
        args = self.eval_forms(forms, var_env, func_env, macro_env)
        return self.exec(py_func_name='print', args=args)


# ==============================================================================
# Set functions related on buitin function
# ==============================================================================

assign_helper(symbol_name='ABS', value=AbsBuiltinFunction(), package_name='PYTHON', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='PRINT', value=PrintBuiltinFunction(), package_name='PYTHON', env='FUNCTION', status=':EXTERNAL')
