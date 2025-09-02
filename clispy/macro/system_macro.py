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
#
#     Quote
#     Backquote
# ==============================================================================


class QuoteSystemMacro(SystemMacro):
    """The quote special operator just returns object.
    """
    def __new__(cls, *args, **kwargs):
        cls.__name__ = 'QUOTE'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of QuoteSystemMacro.
        """
        # Returns itself.
        return Cons(Symbol('QUOTE'), forms)


class BackquoteSystemMacro(SystemMacro):
    """The backquote introduces a template of a data structure to be built."""

    def __new__(cls, *args, **kwargs):
        cls.__name__ = 'BACKQUOTE'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Expand backquote forms."""
        return self.expand_helper(forms.car)

    @classmethod
    def expand_helper(cls, forms):
        """Expand quotes recursively."""
        if not isinstance(forms, Cons):
            # An argument is not an instance of Cons, it is quoted.
            return Cons(Symbol('QUOTE'), Cons(forms, Null()))

        if forms.car is Symbol('UNQUOTE'):
            # Unquote (,)
            return forms.cdr.car
        elif isinstance(forms.car, Cons) and forms.car.car is Symbol('UNQUOTE-SPLICING'):
            # Unquote-splicing (,@)
            return Cons(
                Symbol('APPEND'),
                Cons(forms.car.cdr.car, Cons(cls.expand_helper(forms.cdr), Null())),
            )
        else:
            # Expands recursively and returns cons.
            return Cons(
                Symbol('CONS'),
                Cons(cls.expand_helper(forms.car), Cons(cls.expand_helper(forms.cdr), Null())),
            )


class SetfSystemMacro(SystemMacro):
    """A minimal SETF macro supporting documentation updates."""

    def __new__(cls, *args, **kwargs):
        cls.__name__ = 'SETF'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Expand (setf (documentation ...) value) into a function call.

        For unsupported places this falls back to ``setq`` semantics.
        """

        place = forms.car
        value_form = forms.cdr.car

        if isinstance(place, Cons) and place.car is Symbol('DOCUMENTATION'):
            # (setf (documentation obj type) value) =>
            #   (SETF-DOCUMENTATION value obj type)
            # TODO: Once CLOS is implemented, dispatch to the generic
            # (SETF DOCUMENTATION) instead of this helper.
            return Cons(Symbol('SETF-DOCUMENTATION'), Cons(value_form, place.cdr))

        # Default: behave like SETQ for simple variables
        return Cons(Symbol('SETQ'), Cons(place, Cons(value_form, Null())))


# ==============================================================================
# Set macro for special operators and system functions.
# ==============================================================================

# For special operators
assign_helper(symbol_name='QUOTE', value=QuoteSystemMacro(), package_name='COMMON-LISP', env='MACRO', status='EXTERNAL')
assign_helper(symbol_name='SETF', value=SetfSystemMacro(), package_name='COMMON-LISP', env='MACRO', status='EXTERNAL')

# For system functions
assign_helper(symbol_name='BACKQUOTE', value=BackquoteSystemMacro(), package_name='COMMON-LISP', env='MACRO', status=':EXTERNAL')

# COMMON-LISP-USER package
use_package_helper(package_name_to_use='COMMON-LISP', package_name='COMMON-LISP-USER')
