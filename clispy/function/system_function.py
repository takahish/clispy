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

from clispy.function import Function, Lambda
from clispy.package import PackageManager, assign_helper, use_package_helper
from clispy.type import Cons, Integer, Null, Ratio, String, Symbol


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

    @classmethod
    def eval_forms(cls, forms, var_env, func_env, macro_env):
        """Evaluates forms before these are given to functions as arguments.

        Returns:
            Cons.
        """
        from clispy.evaluator import Evaluator

        args = []
        while forms is not Null():
            args.append(Evaluator.eval(forms.car, var_env, func_env, macro_env))
            forms = forms.cdr

        return Cons.tocons(args)


# ==============================================================================
# Defines system function classes.
# ==============================================================================

class LambdaSystemFunction(SystemFunction):
    """This class is wrapper of clispy.function.Lambda.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates LambdaSystemFunction.
        """
        cls.__name__ = 'LAMBDA'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of LambdaSystemFunction.
        """
        return Lambda(forms, var_env, func_env, macro_env)


class DefunSystemFunction(SystemFunction):
    """Defines a new function named function-name in the global environment.
    The body of the function defined by defun consists of forms;
    they are executed as an implicit progn when the function is called.

    defun implicitly puts a block named block-name around the body forms
    (but not the forms in the lambda-list) of the function defined.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates DefunSystemFunction.
        """
        cls.__name__ = 'DEFUN'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of DefunSystemFunction.
        """
        func_symbol = forms.car
        func_name = func_symbol.value

        lambda_forms = forms

        # Lambda function.
        func = Lambda(lambda_forms.cdr, var_env, func_env, macro_env)

        # Interns symbol that represents function name into current package.
        PackageManager.intern(String(func_name))

        # Binds value to symbol into the global environment.
        try:
            # package_name=None means finding an environment from current package.
            PackageManager.find(func_name, package_name=None, status_check=False, env='FUNCTION')[func_name] = func
        except LookupError:
            PackageManager.current_package.env['FUNCTION'][func_name] = func

        return func_symbol


class DefmacroSystemFunction(SystemFunction):
    """Defines name as a macro by associating a macro function with that
    name in the global environment. The macro function is defined in
    the same lexical environment in which the defmacro form appears.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates DefmacroSystemFunction.
        """
        cls.__name__ = 'DEFMACRO'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of DefmacroSystemFunction.
        """
        macro_symbol = forms.car
        macro_name = macro_symbol.value

        lambda_forms = forms

        # Lambda function.
        macro = Lambda(lambda_forms.cdr, var_env, func_env, macro_env)

        # Interns symbol that represents macro name into current package.
        PackageManager.intern(String(macro_name))

        # Binds value to symbol into the global environment.
        try:
            # package_name=None means finding an environment from current package.
            PackageManager.find(macro_name, package_name=None, status_check=False, env='MACRO')[macro_name] = macro
        except LookupError:
            PackageManager.current_package.env['MACRO'][macro_name] = macro

        return macro_symbol


class ConsSystemFunction(SystemFunction):
    """Creates a fresh cons, the car of which is object-1 and the cdr of which is object-2.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates ConsSystemFunction.
        """
        cls.__name__ = 'CONS'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of ConsSytemFunction.
        """
        args = self.eval_forms(forms, var_env, func_env, macro_env)
        return Cons(args.car, args.cdr.car)


class CarSystemFunction(SystemFunction):
    """If x is a cons, car returns the car of that cons. If x is nil, car returns nil.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates CarSystemFunction.
        """
        cls.__name__ = 'CAR'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of CarSystemFunction.
        """
        args = self.eval_forms(forms, var_env, func_env, macro_env)
        return args.car.car


class CdrSystemFunction(SystemFunction):
    """If x is a cons, cdr returns the cdr of that cons. If x is nil, cdr returns nil.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates CdrSystemFunction.
        """
        cls.__name__ = 'CDR'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of CdrSystemFunction.
        """
        args = self.eval_forms(forms, var_env, func_env, macro_env)
        return args.car.cdr


class AppendSystemFunction(SystemFunction):
    """append returns a new list that is the concatenation of the copies.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates AppendSystemFunction.
        """
        cls.__name__ = 'APPEND'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of AppendSystemFunction.
        """
        args = self.eval_forms(forms, var_env, func_env, macro_env)

        retval = Null()
        while args.cdr is not Null():
            retval = self.accumulate(args.car, retval)
            args = args.cdr

        last = args.car

        return self.reverse(retval, last)

    @classmethod
    def accumulate(cls, args, acc):
        """Accumulates an element in args as Cons to acc.
        """
        while args is not Null():
            acc = Cons(args.car, acc)
            args = args.cdr

        return acc

    @classmethod
    def reverse(cls, args, last):
        """Reverses an element in args as Cons.
        """
        while args is not Null():
            last = Cons(args.car, last)
            args = args.cdr

        return last


class FindSymbolSystemFunction(SystemFunction):
    """FindSymbol locates a symbol whose name is symbol_designator in a package. If a symbol named
    symbol_designator is found in package, directly or by inheritance, the symbol found is returned
    as the first value; the second value is as follows:

    :INTERNAL
        If the symbol is present in package as an internal symbol.
    :EXTERNAL
        If the symbol is present in package as an external symbol.
    :INHERITED
        If the symbol is inherited by package through use-package, but is not present in package.

    If no such symbol is accessible in package, both values are nil.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates FindSymbolSystemFunction.
        """
        cls.__name__ = 'FIND-SYMBOL'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of FindSymbolSystemFunction.
        """
        args = self.eval_forms(forms, var_env, func_env, macro_env)

        symbol_designator = args.car
        package_designator = args.cdr.car

        if package_designator is Null():
            return PackageManager.find_symbol(symbol_designator=symbol_designator)
        else:
            return PackageManager.find_symbol(symbol_designator=symbol_designator, package_designator=package_designator)


class InternSystemFunction(SystemFunction):
    """The first value returned by intern, symbol_designator, is the symbol that was found or created.
    The meaning of the secondary value, status, is as follows:

    :INTERNAL
        The symbol was found and is present in package as an internal symbol.
    :EXTERNAL
        The symbol was found and is present as an external symbol.
    :INHERITED'
        The symbol was found and is inherited via use-package (which implies that the symbol is internal).
    nil
        No pre-existing symbol was found, so one was created.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates InternSystemFunction.
        """
        cls.__name__ = 'INTERN'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of InternSystemFunction.
        """
        args = self.eval_forms(forms, var_env, func_env, macro_env)

        symbol_designator = args.car
        package_designator = args.cdr.car

        if package_designator is Null():
            return PackageManager.intern(symbol_designator=symbol_designator)
        else:
            return PackageManager.intern(symbol_designator=symbol_designator, package_designator=package_designator)


class InPackageSystemFunction(SystemFunction):
    """Causes the the package named by name to become the current package---that is,
    the value of *package*. If no such package already exists, an error of package-error
    is signaled.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates InPackageSystemFunction.
        """
        cls.__name__ = 'IN-PACKAGE'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of InPackageSystemFunction.
        """
        args = self.eval_forms(forms, var_env, func_env, macro_env)
        return PackageManager.in_package(package_designator=args.car)


class AddSystemFunction(SystemFunction):
    """Returns the sum of numbers, performing any necessary type conversions in the process.
    If no numbers are supplied, 0 is returned.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates AddSystemFunction.
        """
        cls.__name__ = '+'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of AddSystemFunction.
        """
        args = self.eval_forms(forms, var_env, func_env, macro_env)

        # Initializes retval.
        retval = Integer(0)

        # The sum of numbers.
        while args is not Null():
            retval = retval + args.car
            args = args.cdr

        return retval


class SubSystemFunction(SystemFunction):
    """The function - performs arithmetic subtraction and negation.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates SubSystemFunction.
        """
        cls.__name__ = '-'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of SubSystemFunction.
        """
        args = self.eval_forms(forms, var_env, func_env, macro_env)

        # Initializes retval.
        retval, args = args.car, args.cdr

        # Subtraction.
        while args is not Null():
            retval = retval - args.car
            args = args.cdr

        return retval


class MulSystemFunction(SystemFunction):
    """Returns the product of numbers, performing any necessary type conversions in the process.
    If no numbers are supplied, 1 is returned.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates MulSystemFunction.
        """
        cls.__name__ = '*'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of MulSystemFunction.
        """
        args = self.eval_forms(forms, var_env, func_env, macro_env)

        # Initializes retval.
        retval = Integer(1)

        # The product of numbers.
        while args is not Null():
            retval = retval * args.car
            args = args.cdr

        return retval


class DivSystemFunction(SystemFunction):
    """The function / performs division or reciprocation.
    """
    def __new__(cls, *args, **kwargs):
        """Intantiates Div.
        """
        cls.__name__ = '/'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of DivSystemFunction.
        """
        args = self.eval_forms(forms, var_env, func_env, macro_env)

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

assign_helper(symbol_name='LAMBDA', value=LambdaSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='DEFUN', value=DefunSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='DEFMACRO', value=DefmacroSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='CONS', value=ConsSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='CAR', value=CarSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='CDR', value=CdrSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='APPEND', value=AppendSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='FIND-SYMBOL', value=FindSymbolSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='INTERN', value=InternSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='IN-PACKAGE', value=InPackageSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='+', value=AddSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='-', value=SubSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='*', value=MulSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='/', value=DivSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')

# COMMON-LISP-USER package
use_package_helper(package_name_to_use='COMMON-LISP', package_name='COMMON-LISP-USER')