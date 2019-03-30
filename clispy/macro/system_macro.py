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

from clispy.macro import Macro
from clispy.package import assign_helper, use_package_helper
from clispy.type import Cons, Null, Symbol


# ==============================================================================
# Defines base classes.
#
#     SystemMacro
# ==============================================================================

class SystemMacro(Macro):
    """SystemMacro provide some macros for defmacro, defun, and lambda etc.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates SystemMacro.
        """
        cls.__name__ = 'SYSTEM-MACRO'
        return object.__new__(cls)

    def __repr__(self):
        """The official string representation.
        """
        return "#<SYSTEM-MACRO {0} {{{1:X}}}>".format(self.__class__.__name__, id(self))


# ==============================================================================
# Defines system macro classes.
# ==============================================================================

class BlockSystemMacro(SystemMacro):
    """block establishes a block and then evaluates forms as an implicit progn.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates BlockSystemMacro.
        """
        cls.__name__ = 'BLOCK'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of BlockSystemMacro.
        """
        from clispy.expander import Expander

        name, body = forms.car, forms.cdr

        # Expands body recursively.
        body = Expander.expand(body, var_env, func_env, macro_env)

        # The body of a block has an implicit progn.
        forms = Cons(Symbol('BLOCK'), Cons(name, Cons(Cons(Symbol('PROGN'), body), Null())))

        return forms


class FletSystemMacro(SystemMacro):
    """flet, labels, and macrolet define local functions and macros, and execute
    forms using the local definitions. forms are executed in order of occurence.

    The body forms (but not the lambda list) of each function created by flet
    and labels and each macro created by macrolet are enclosed in an implicit
    block whose name is the function block name of the function-name or name,
    as appropriate.

    flet defines locally named functions and executes a series of forms with
    these definition bindings. Any number of such local functions can be defined.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates FletSystemMacro.
        """
        cls.__name__ = 'FLET'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of FletSystemMacro.
        """
        from clispy.expander import Expander

        bindings, body = forms.car, forms.cdr

        # Expands body recursively.
        body = Expander.expand(body, var_env, macro_env, macro_env)

        # The body of flet has an implicit progn.
        forms = Cons(Symbol('FLET'), Cons(bindings, Cons(Cons(Symbol('PROGN'), body), Null())))

        return forms


class IfSystemMacro(SystemMacro):
    """if allows the execution of a form to be dependent on a single test-form.

    First test-form is evaluated. If the result is true, then then-form is selected;
    otherwise else-form is selected. Whichever form is selected is then evaluated.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates IfSystemMacro.
        """
        cls.__name__ = 'IF'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of IfSystemMacro.
        """
        from clispy.expander import Expander

        # If else_form is Null, then else_form is set to Null.
        test_form, then_form, else_form = forms.car, forms.cdr.car, forms.cdr.cdr.car

        # Expands body recursively.
        test_form = Expander.expand(test_form, var_env, func_env, macro_env)
        then_form = Expander.expand(then_form, var_env, func_env, macro_env)
        else_form = Expander.expand(else_form, var_env, func_env, macro_env)

        forms = Cons(Symbol('IF'), Cons(test_form, Cons(then_form, Cons(else_form, Null()))))
        return forms


class LabelsSystemMacro(SystemMacro):
    """flet, labels, and macrolet define local functions and macros, and execute
    forms using the local definitions. forms are executed in order of occurence.

    The body forms (but not the lambda list) of each function created by flet
    and labels and each macro created by macrolet are enclosed in an implicit
    block whose name is the function block name of the function-name or name,
    as appropriate.

    labels is equivalent to flet except that the scope of the defined function
    names for labels encompasses the function definitions themselves as well
    as the body.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates LabelsSystemMacro.
        """
        cls.__name__ = 'LABELS'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of LablesSystemMacro.
        """
        from clispy.expander import Expander

        bindings, body = forms.car, forms.cdr

        # Expands body recursively.
        body = Expander.expand(body, var_env, func_env, macro_env)

        # The body of labels has an implicit progn.
        forms = Cons(Symbol('LABELS'), Cons(bindings, Cons(Cons(Symbol('PROGN'), body), Null())))

        return forms


class LetSystemMacro(SystemMacro):
    """let and let* create new variable bindings and execute a series of forms
    that use these bindings. let performs the bindings in parallel and let* does
    them sequentially.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates LetSystemMacro.
        """
        cls.__name__ = 'LET'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of LetSystemMacro.
        """
        from clispy.expander import Expander

        bindings, body = forms.car, forms.cdr

        # Expands body recursively.
        body = Expander.expand(body, var_env, func_env, macro_env)

        # The body of let has an implicit progn.
        forms = Cons(Symbol('LET'), Cons(bindings, Cons(Cons(Symbol('PROGN'), body), Null())))

        return forms


class LetAsterSystemMacro(SystemMacro):
    """let and let* create new variable bindings and execute a series of forms
    that use these bindings. let performs the bindings in parallel and let* does
    them sequentially.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates LetAsterSytemMacro.
        """
        cls.__name__ = 'LET*'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of LetAsterSystemMacro.
        """
        from clispy.expander import Expander

        bindings, body = forms.car, forms.cdr

        # Expands body recursively.
        body = Expander.expand(body, var_env, func_env, macro_env)

        # The body of let* has an implicit progn
        forms = Cons(Symbol('LET*'), Cons(bindings, Cons(Cons(Symbol('PROGN'), body), Null())))

        return forms


class QuoteSystemMacro(SystemMacro):
    """The quote special operator just returns object.
    """
    def __new__(cls, *args, **kwargs):
        cls.__name__ = 'QUOTE'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of QuoteSystemMacro.
        """
        # Retruns itself.
        return Cons(Symbol('QUOTE'), forms)


class LambdaSystemMacro(SystemMacro):
    """Provides a shorthand notation for a function special form involving a lambda expression.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates LambdaSystemMacro.
        """
        cls.__name__ = 'LAMBDA'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of LambdaSystemMacro.
        """
        from clispy.expander import Expander

        params, body = forms.car, forms.cdr

        # Expands body recursively.
        body = Expander.expand(body, var_env, func_env, macro_env)

        # The body of a lambda has an implicit progn.
        forms = Cons(Symbol('LAMBDA'), Cons(params, Cons(Cons(Symbol('PROGN'), body), Null())))

        return forms


class DefunSystemMacro(SystemMacro):
    """defun implicitly puts a block named block-name around the body forms
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates DefunSystemMacro.
        """
        cls.__name__ = 'DEFUN'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of DefunSystemMacro.
        """
        from clispy.expander import Expander

        name, params, body = forms.car, forms.cdr.car, forms.cdr.cdr

        # Expands body, recursively.
        body = Expander.expand(body, var_env, func_env, macro_env)

        # The body of a defun has an implicit progn.
        body = Cons(Cons(Symbol('PROGN'), body), Null())

        # The body of a defun has an implicit block.
        forms = Cons(Symbol('DEFUN'), Cons(name, Cons(params, Cons(Cons(Symbol('BLOCK'), Cons(name, body)), Null()))))

        return forms


class DefmacroSystemMacro(SystemMacro):
    """Defines name as a macro by associating a macro function with that
    name in the global environment. The macro function is defined in
    the same lexical environment in which the defmacro form appears.

    The expansion function accepts two arguments, a form and an
    environment. The expansion function returns a form. The body of
    the expansion function is specified by forms. Forms are executed
    in order. The value of the last form executed is returned as the
    expansion of the macro. The body forms of the expansion function
    (but not the lambda-list) are implicitly enclosed in a block whose
    name is name.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates DefmacroSystemMacro.
        """
        cls.__name__ = 'DEFMACRO'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of DefmacroSystemMacro.
        """
        from clispy.expander import Expander

        name, params, body = forms.car, forms.cdr.car, forms.cdr.cdr

        # Expands body, recursively.
        body = Expander.expand(body, var_env, func_env, macro_env)

        # The value of the last form executed is returned as the expansion of the macro.
        body = Cons(Cons(Symbol('PROGN'), body), Null())

        # The body of a defmacro has an implicit block.
        forms = Cons(Symbol('DEFMACRO'), Cons(name, Cons(params, Cons(Cons(Symbol('BLOCK'), Cons(name, body)), Null()))))

        return forms


class BackquoteSystemMacro(SystemMacro):
    """The backquote introduces a template of a data structure to be built.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates BackquoteSystemMacro.
        """
        cls.__name__ = 'BACKQUOTE'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of BackquoteSystemMacro.
        """
        return self.expand_hepler(forms.car)

    @classmethod
    def expand_hepler(cls, forms):
        """Expand quotes recursively.
        """
        if not isinstance(forms, Cons): # An argument is not an instance of Cons, it is quoted.
            return Cons(Symbol('QUOTE'), Cons(forms, Null()))

        if forms.car is Symbol('UNQUOTE'): # Unquote (,).
            return forms.cdr.car
        elif isinstance(forms.car, Cons) and forms.car.car is Symbol('UNQUOTE-SPLICING'): # Unquote-splicing (,@).
            return Cons(Symbol('APPEND'), Cons(forms.car.cdr.car, Cons(cls.expand_hepler(forms.cdr), Null())))
        else: # Expands recursively and returns cons.
            return Cons(Symbol('CONS'), Cons(cls.expand_hepler(forms.car), Cons(cls.expand_hepler(forms.cdr), Null())))


# ==============================================================================
# Set functions related on special operators
# ==============================================================================

# For special operators
assign_helper(symbol_name='BLOCK', value=BlockSystemMacro(), package_name='COMMON-LISP', env='MACRO', status=':EXTERNAL')
assign_helper(symbol_name='FLET', value=FletSystemMacro(), package_name='COMMON-LISP', env='MACRO', status=':EXTERNAL')
assign_helper(symbol_name='IF', value=IfSystemMacro(), package_name='COMMON-LISP', env='MACRO', status=':EXTERNAL')
assign_helper(symbol_name='LABELS', value=LabelsSystemMacro(), package_name='COMMON-LISP', env='MACRO', status=':EXTERNAL')
assign_helper(symbol_name='LET', value=LetSystemMacro(), package_name='COMMON-LISP', env='MACRO', status=':EXTERNAL')
assign_helper(symbol_name='LET*', value=LetAsterSystemMacro(), package_name='COMMON-LISP', env='MACRO', status=':EXTERNAL')
assign_helper(symbol_name='QUOTE', value=QuoteSystemMacro(), package_name='COMMON-LISP', env='MACRO', status='EXTERNAL')

# For system functions
assign_helper(symbol_name='LAMBDA', value=LambdaSystemMacro(), package_name='COMMON-LISP', env='MACRO', status=':EXTERNAL')
assign_helper(symbol_name='DEFUN', value=DefunSystemMacro(), package_name='COMMON-LISP', env='MACRO', status=':EXTERNAL')
assign_helper(symbol_name='DEFMACRO', value=DefmacroSystemMacro(), package_name='COMMON-LISP', env='MACRO', status=':EXTERNAL')
assign_helper(symbol_name='BACKQUOTE', value=BackquoteSystemMacro(), package_name='COMMON-LISP', env='MACRO', status=':EXTERNAL')

# COMMON-LISP-USER package
use_package_helper(package_name_to_use='COMMON-LISP', package_name='COMMON-LISP-USER')