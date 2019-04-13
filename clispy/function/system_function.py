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
from clispy.type import Cons, Integer, List, Null, Ratio, Real, Sequence, String, T, Vector
from clispy.type import Float, ShortFloat, SingleFloat, DoubleFloat, LongFloat


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


class EqSystemFunction(SystemFunction):
    """Returns true if its arguments are the same, identical object;
    otherwise, returns false.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates EqSystemFunction.
        """
        cls.__name__ = 'EQ'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of EqSystemFunction.
        """
        args = self.eval_forms(forms, var_env, func_env, macro_env)

        # Whether arguments are the same or not.
        if args.car is args.cdr.car:
            return T()
        else:
            return Null()


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

        # Checks package_designator whether it is given or not.
        if package_designator is Null():
            return PackageManager.intern(symbol_designator=symbol_designator)
        else:
            return PackageManager.intern(symbol_designator=symbol_designator, package_designator=package_designator)


class ExportSystemFunction(SystemFunction):
    """Export makes one or more symbols that are accessible in package (whether directly
    or by inheritance) be external symbols of that package.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates ExportSystemFunction.
        """
        cls.__name__ = 'EXPORT'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of ExportSystemFunction.
        """
        args = self.eval_forms(forms, var_env, func_env, macro_env)

        symbol_designator = args.car
        package_designator = args.cdr.car

        # Checks package_designator whether it is given or not.
        if package_designator is Null():
            package_designator = None

        if isinstance(symbol_designator, Cons):
            # If symbol_designator is symbol list, exports all symbols.
            while symbol_designator.cdr is not Null():
                PackageManager.export(symbol_designator=symbol_designator.car, package_designator=package_designator)
                symbol_designator = symbol_designator.cdr

            # The last symbol.
            return PackageManager.export(symbol_designator=symbol_designator.car, package_designator=package_designator)

        else:
            return PackageManager.export(symbol_designator=symbol_designator, package_designator=package_designator)


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


class UsePackageSystemFunction(SystemFunction):
    """use-package causes package to inherit all the external symbols of packages-to-use.
    The inherited symbols become accessible as internal symbols of package.

    packages-to-use are added to the use list of package if they are not there already.
    All external symbols in packages-to-use become accessible in package as internal
    symbols. use-package does not cause any new symbols to be present in package but
    only makes them accessible by inheritance.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates UsePackageSystemFunction.
        """
        cls.__name__ = 'USE-PACKAGE'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of UsePackageSystemFunction.
        """
        args = self.eval_forms(forms, var_env, func_env, macro_env)

        package_designator_to_use = args.car
        package_designator = args.cdr.car

        # Checks package_designator whether it is given or not.
        if package_designator is Null():
            package_designator = None

        if isinstance(package_designator_to_use, Cons):
            # If package_designator_to_use is symbol list, exports all symbols.
            while package_designator_to_use.cdr is not Null():
                PackageManager.use_package(
                    package_designator_to_use=package_designator_to_use.car,
                    package_designator=package_designator
                )
                package_designator_to_use = package_designator_to_use.cdr

            # The last package_dexignator_to_use.
            return PackageManager.use_package(
                package_designator_to_use=package_designator_to_use.car,
                package_designator=package_designator
            )

        else:
            return PackageManager.use_package(
                package_designator_to_use=package_designator_to_use,
                package_designator=package_designator
            )


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


class NumericalEqualSystemFunction(SystemFunction):
    """The value of = is true if all numbers are the same in value;
    otherwise it is false.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates NumericalEqualSystemFunction.
        """
        cls.__name__ = '='
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of NumericalEqualSystemFunction.
        """
        args = self.eval_forms(forms, var_env, func_env, macro_env)

        # Sets initial value.
        val, args = args.car, args.cdr

        # Whether all numbers are the same or not.
        while args is not Null():
            if args.car != val:
                return Null()
            args = args.cdr

        return T()


class NumericalNotEqualSystemFunction(SystemFunction):
    """The value of /= is true if no two numbers are the same in value;
    otherwise it is false.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates NumericalNotEqualSystemFunction.
        """
        cls.__name__ = '/='
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of NumericalNotEqualSystemFunction.
        """
        args = self.eval_forms(forms, var_env, func_env, macro_env)

        # Sets initial value.
        val, args = args.car, args.cdr

        # Whether no two numbers are the same or not.
        while args is not Null():
            rest_args = args
            while rest_args is not Null():
                if rest_args.car == val:
                    return Null()
                rest_args = rest_args.cdr

            # Updates value and args.
            val, args = args.car, args.cdr

        return T()


class LessThanSystemFunction(SystemFunction):
    """The value of < is true if the numbers are in monotonically increasing order;
    otherwise it is false.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates LessThanSystemFunction.
        """
        cls.__name__ = '<'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of LessThanSystemFunction.
        """
        args = self.eval_forms(forms, var_env, func_env, macro_env)

        # Sets initial value.
        val, args = args.car, args.cdr

        # Whether the numbers are in monotonically increasing order or not.
        while args is not Null():
            if val >= args.car:
                return Null()

            # Updates value and args.
            val, args = args.car, args.cdr

        return T()


class GreaterThanSystemFunction(SystemFunction):
    """The value of > is true if the numbers are in monotonically decreasing order;
    otherwise it is false.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates GreaterThanSystemFunction.
        """
        cls.__name__ = '>'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of GreaterThanSystemFunction.
        """
        args = self.eval_forms(forms, var_env, func_env, macro_env)

        # Sets initial value.
        val, args = args.car, args.cdr

        # Whether the numbers are in monotonically decreasing order or not.
        while args is not Null():
            if val <= args.car:
                return Null()

            # Updates value and args.
            val, args = args.car, args.cdr

        return T()


class LessThanEqualSystemFunction(SystemFunction):
    """The value of <= is true if the numbers are in monotonically nondecreasing order;
    otherwise it is false.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates LessThanEqualSystemFunction.
        """
        cls.__name__ = '<='
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of LessThanEqualSystemFunction.
        """
        args = self.eval_forms(forms, var_env, func_env, macro_env)

        # Sets initial value.
        val, args = args.car, args.cdr

        # Whether the numbers are in monotonically increasing order or not.
        while args is not Null():
            if val > args.car:
                return Null()

            # Updates value and args.
            val, args = args.car, args.cdr

        return T()


class GreaterThanEqualSystemFunction(SystemFunction):
    """The value of >= is true if the numbers are in monotonically nonincreasing order;
    otherwise it is false.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates GreaterThanSystemFunction.
        """
        cls.__name__ = '>='
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of GreaterThanSystemFunction.
        """
        args = self.eval_forms(forms, var_env, func_env, macro_env)

        # Sets initial value.
        val, args = args.car, args.cdr

        # Whether the numbers are in monotonically decreasing order or not.
        while args is not Null():
            if val < args.car:
                return Null()

            # Updates value and args.
            val, args = args.car, args.cdr

        return T()


class QuitSystemFunction(SystemFunction):
    """Quit interpreter.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates QuitSystemFunction.
        """
        cls.__name__ = 'QUIT'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of QuitSystemFunction.
        """
        # Import Interrupt exception class from clispy.interpreter.
        from clispy.interpreter import Interrupt

        # Raises Interrupt.
        raise Interrupt()


class CoerceSystemFunction(SystemFunction):
    """The following function may be used to convert an object to
    an equivalent object of another type.
    """
    # Type specified.
    type_specified = {
        'SEQUENCE': Sequence,
        'LIST': List,
        'NULL': Null,
        'CONS': Cons,
        'VECTOR': Vector,
        'STRING': String,
        'FLOAT': Float,
        'SHORT-FLOAT': ShortFloat,
        'SINGLE-FLOAT': SingleFloat,
        'DOUBLE-FLOAT': DoubleFloat,
        'LONG-FLOAT': LongFloat,
        'T': T
    }

    def __new__(cls, *args, **kwargs):
        """Instantiates CoerceSystemFunction.
        """
        cls.__name__ = 'COERCE'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of CoerceSystemFunction.
        """
        args = self.eval_forms(forms, var_env, func_env, macro_env)

        # Sets an object_ and a result type.
        object_ = args.car
        result_type = args.cdr.car

        # If object_ is already of the specified type, then it is simply returned.
        if isinstance(object_, self.type_specified[result_type.value]):
            return object_

        # Converts object_.
        if issubclass(self.type_specified[result_type.value], Sequence):
            return self.to_sequence(object_, result_type)
        elif issubclass(self.type_specified[result_type.value], Float):
            return self.to_float(object_, result_type)
        elif self.type_specified[result_type.value] is T:
            return self.to_t(object_, result_type)

    @classmethod
    def to_sequence(cls, object_, result_type):
        """Converts object_ to Sequence.
        """
        from clispy.python import PyObject

        # Checks object_ classes.
        if (not isinstance(object_, Sequence)) and (not isinstance(object_, PyObject)):
            raise SimpleTypeError("{} can't be converted to type {}.".format(str(object_), str(result_type)))

        # Converts object_ to result_type that is subclass of Sequence.
        if cls.type_specified[result_type.value] is Null:
            return cls.to_null(object_, result_type)
        elif cls.type_specified[result_type.value] is Cons:
            return cls.to_cons(object_, result_type)
        elif cls.type_specified[result_type.value] is Vector:
            return cls.to_vector(object_, result_type)

    @classmethod
    def to_null(cls, object_, result_type):
        """Converts object_ to Null.
        """
        # Checks a length of sequence.
        sequence_length = len(object_.value)

        # If sequence_length is greater than zero, SIMPLE-TYPE-ERROR is occured.
        if sequence_length > 0:
            raise SimpleTypeError("The requested length ({}) does not match the specified type {}.".format(
                str(sequence_length),
                str(result_type)
            ))
        return Null()

    @classmethod
    def to_cons(cls, object_, result_type):
        """Converts object_ to Cons.
        """
        # Checks a length of sequence.
        if isinstance(object_, Null):
            sequence_length = 0
        else:
            sequence_length = len(object_.value)

        # If sequence_length is zero, SIMPLE-TYPE-ERROR is occured.
        if sequence_length == 0:
            raise SimpleTypeError("The requested length ({}) does not match the specified type {}.".format(
                str(sequence_length),
                str(result_type)
            ))

        if isinstance(object_.value, list):
            return Cons.tocons(object_.value)
        else: # If object_.value is not list, it is converted to list in advance.
            return Cons.tocons(list(object_.value))

    @classmethod
    def to_vector(cls, object_, result_type):
        # Converts object_ to Vector.
        return cls.type_specified[result_type.value](object_.value)

    @classmethod
    def to_float(cls, object_, result_type):
        # Converts object_ to float.
        from clispy.python import PyObject

        # Checks object_ classes.
        if (not isinstance(object_, Real)) and (not isinstance(object_, PyObject)):
            raise SimpleTypeError("{} can't be converted to type {}.".format(str(object_), str(result_type)))

        # Converts object_ to result_type that is subclass of Float.
        return cls.type_specified[result_type.value](object_.value)

    @classmethod
    def to_t(cls, object_, result_type):
        # Converts object_ to t.
        # In this case, the object is simply returned.
        return object_


class TypeOfSystemFunction(SystemFunction):
    """Returns a type specifier, typespec, for a type that has the object as an element.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates TypeOfSystemFunction.
        """
        cls.__name__ = 'TYPE-OF'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of TypeOfSystemFunction.
        """
        args = self.eval_forms(forms, var_env, func_env, macro_env)

        # Sets an object_.
        object_ = args.car

        return object_.type_of()


class ClassOfSystemFunction(SystemFunction):
    """Returns the class of which the object is a direct instance.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates ClassOfSystemFunction.
        """
        cls.__name__ = 'CLASS-OF'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of ClassOfSystemFunction.
        """
        args = self.eval_forms(forms, var_env, func_env, macro_env)

        # Sets an object_.
        object_ = args.car

        return object_.class_of()


# ==============================================================================
# Defines some exception classes
# ==============================================================================

class SimpleTypeError(Exception):
    def __new__(cls, *args, **kwargs):
        """Instantiates SimpleTypeError.
        """
        cls.__name__ = 'SIMPLE-TYPE-ERROR'
        return Exception.__new__(cls)


# ==============================================================================
# Set functions related on special operators
# ==============================================================================

assign_helper(symbol_name='LAMBDA', value=LambdaSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='DEFUN', value=DefunSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='DEFMACRO', value=DefmacroSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='EQ', value=EqSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='EQL', value=EqSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='CONS', value=ConsSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='CAR', value=CarSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='CDR', value=CdrSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='APPEND', value=AppendSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='FIND-SYMBOL', value=FindSymbolSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='INTERN', value=InternSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='EXPORT', value=ExportSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='IN-PACKAGE', value=InPackageSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='USE-PACKAGE', value=UsePackageSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='+', value=AddSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='-', value=SubSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='*', value=MulSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='/', value=DivSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='=', value=NumericalEqualSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='/=', value=NumericalNotEqualSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='<', value=LessThanSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='>', value=GreaterThanSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='<=', value=LessThanEqualSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='>=', value=GreaterThanEqualSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='QUIT', value=QuitSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='COERCE', value=CoerceSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='TYPE-OF', value=TypeOfSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='CLASS-OF', value=ClassOfSystemFunction(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')

# COMMON-LISP-USER package
use_package_helper(package_name_to_use='COMMON-LISP', package_name='COMMON-LISP-USER')