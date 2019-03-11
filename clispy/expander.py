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

from clispy.macro import *
from clispy.package import PackageManager
from clispy.type import Cons, Null, Symbol


class Expander(object):
    @classmethod
    def expand(cls, forms, var_env, func_env, macro_env):
        if not isinstance(forms, Cons):
            # If an atom is given, returns itself.
            return forms

        elif isinstance(forms.car, Symbol): # and forms.car.value in macro_env:
            # Gets symbol_name, package_name, and status_check.
            macro_name, package_name, status_check = PackageManager.split_symbol_name(forms.car.value)

            # Gets the macro binded by the symbol.
            # When package_name is None, the package becomes PackageManager.current_package.
            try:
                # Tries to get the function from lexical environment in current package.
                macro = macro_env.find(macro_name)[macro_name]
            except LookupError:
                try:
                    macro = PackageManager.find(macro_name, package_name, status_check, env='MACRO')[macro_name]
                except LookupError:
                    return Cons(forms.car, cls.recusive_expand(forms.cdr, var_env, func_env, macro_env))

            return macro(forms.cdr, var_env, func_env, macro_env)

        else:
            return Cons(cls.expand(forms.car, var_env, func_env, macro_env), cls.expand(forms.cdr, var_env, func_env, macro_env))

    @classmethod
    def recusive_expand(cls, rest_forms, var_env, func_env, macro_env):
        """Expand forms recursively.
        """
        expanded_forms = Null()

        while rest_forms is not Null():
            expanded_forms = Cons(cls.expand(rest_forms.car, var_env, func_env, macro_env), expanded_forms)
            rest_forms = rest_forms.cdr

        return cls.reverse(expanded_forms, Null())

    @classmethod
    def reverse(cls, args, last):
        """Reverses an element in args as Cons.
        """
        while args is not Null():
            last = Cons(args.car, last)
            args = args.cdr

        return last