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

    def exec_func(self, py_func_name, args):
        """Executes builtin function.
        """
        # Sets args for function.
        py_args = []
        while args is not Null():
            py_args.append(args.car.value)
            args = args.cdr

        return __builtins__[py_func_name](*py_args)


# ==============================================================================
# Defines builtin function classes.
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
        return PyObject(self.exec_func(py_func_name='abs', args=args))


class BoolBuiltinFunction(BuiltinFunction):
    """Return a Boolean value, i.e. one of True or False. x is converted using
    the standard truth testing procedure. If x is false or omitted, this
    returns False; otherwise it returns True.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates BoolBuiltinFunction.
        """
        cls.__name__ = 'BOOL'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of BoolBuiltinFunction.
        """
        args = self.eval_forms(forms, var_env, func_env, macro_env)
        return PyObject(self.exec_func(py_func_name='bool', args=args))


class CallableBuiltinFunction(BuiltinFunction):
    """Return True if the object argument appears callable, False if not.
    If this returns true, it is still possible that a call fails, but
    if it is false, calling object will never succeed. Note that classes
    are callable (calling a class returns a new instance); instances are
    callable if their class has a __call__() method.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates CallableBuiltinFunction.
        """
        cls.__name__ = 'CALLABLE'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of CallableBuiltinFunction.
        """
        args = self.eval_forms(forms, var_env, func_env, macro_env)
        return PyObject(self.exec_func(py_func_name='callable', args=args))


class CompileBuiltinFunction(BuiltinFunction):
    """Compile the source into a code or AST object. Code objects can
    be exec_funcuted by exec_func() or eval(). source can either be a normal
    string, a byte string, or an AST object. Refer to the ast module
    documentation for information on how to work with AST objects.
    """
    def __new__(cls, *args, **kwargs):
        """Instaintiates CompileBuiltinFunction.
        """
        cls.__name__ = 'COMPILE'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of CompileBuiltinFunction.
        """
        args = self.eval_forms(forms, var_env, func_env, macro_env)
        return PyObject(self.exec_func(py_func_name='compile', args=args))


class ListBuiltinFunction(BuiltinFunction):
    """Rather than being a function, list is actually a mutable sequence type,
    as documented in Lists and Sequence Types — list, tuple, range.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates ListBuiltinFunction.
        """
        cls.__name__ = 'LIST'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of ListBuiltinFunction.
        """
        args = self.eval_forms(forms, var_env, func_env, macro_env)
        return PyObject(self.exec_func(py_func_name='list', args=args))


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
        self.exec_func(py_func_name='print', args=args)
        return Null()


class SliceBuiltinFunction(BuiltinFunction):
    """Return a slice object representing the set of indices specified by
    range(start, stop, step). The start and step arguments default to None.
    Slice objects have read-only data attributes start, stop and step which
    merely return the argument values (or their default).
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates SliceBuiltinFunction.
        """
        cls.__name__ = 'SLICE'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of SliceBuiltinFunction.
        """
        args = self.eval_forms(forms, var_env, func_env, macro_env)

        # Sets slice object for getting all elements.
        if args is Null():
            return PyObject(slice(None, None, None))

        return PyObject(self.exec_func(py_func_name='slice', args=args))


class SortedBuiltinFunction(BuiltinFunction):
    """Return a new sorted list from the items in iterable.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates SortedBuiltinFunction.
        """
        cls.__name__ = 'SORTED'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of SortedBuiltinFunction.
        """
        args = self.eval_forms(forms, var_env, func_env, macro_env)

        # If an argument is empty list.
        if args.car is Null():
            return PyObject([])

        return PyObject(self.exec_func(py_func_name='sorted', args=args))


class TupleBuiltinFunction(BuiltinFunction):
    """Rather than being a function, tuple is actually an immutable sequence type,
    as documented in Tuples and Sequence Types — list, tuple, range.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates TupleBuiltinFunction.
        """
        cls.__name__ = 'TUPLE'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of TupleBuiltinFunction.
        """
        args = self.eval_forms(forms, var_env, func_env, macro_env)
        return PyObject(self.exec_func(py_func_name='tuple', args=args))


class TypeBuiltinFunction(BuiltinFunction):
    """With one argument, return the type of an object. The return value is a type
    object and generally the same object as returned by object.__class__.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates TypeBuiltinFunction.
        """
        cls.__name__ = 'TYPE'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of TypeBuiltinFunction.
        """
        args = self.eval_forms(forms, var_env, func_env, macro_env)

        # type return value a type object, so it dose not wrap by using PyObject.
        return type(args.car)


# ==============================================================================
# Set functions related on buitin function
# ==============================================================================

assign_helper(symbol_name='ABS', value=AbsBuiltinFunction(), package_name='PYTHON', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='BOOL', value=BoolBuiltinFunction(), package_name='PYTHON', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='CALLABLE', value=CallableBuiltinFunction(), package_name='PYTHON', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='COMPILE', value=CompileBuiltinFunction(), package_name='PYTHON', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='LIST', value=ListBuiltinFunction(), package_name='PYTHON', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='PRINT', value=PrintBuiltinFunction(), package_name='PYTHON', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='SLICE', value=SliceBuiltinFunction(), package_name='PYTHON', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='SORTED', value=SortedBuiltinFunction(), package_name='PYTHON', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='TUPLE', value=TupleBuiltinFunction(), package_name='PYTHON', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='TYPE', value=TypeBuiltinFunction(), package_name='PYTHON', env='FUNCTION', status=':EXTERNAL')
