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

from clispy.function import *
from clispy.package import PackageManager, assign_helper
from clispy.python import *
from clispy.type import Cons, Keyword, Null, Symbol


class Evaluator(object):
    @classmethod
    def eval(cls, forms, var_env, func_env, macro_env):
        if isinstance(forms, Null):
            # Returns nil.
            return forms

        elif isinstance(forms, Symbol):
            # Gets symbol_name, package_name, and status_check.
            symbol_name, package_name, status_check = PackageManager.split_symbol_name(forms.value)

            # Gets the value binded by the symbol.
            # When package_name is None, the package becomes PackageManager.current_package.
            try:
                # Tries to get the value from lexical environment in current package.
                return var_env.find(symbol_name)[symbol_name]
            except LookupError:
                # Tries to get the value from another package.
                return PackageManager.find(symbol_name, package_name, status_check, env='VARIABLE')[symbol_name]
            finally:
                # If package_name is 'KEYWORD', sets keyword represented by symbol_name.
                if package_name == 'KEYWORD':
                    assign_helper(symbol_name, Keyword(symbol_name), package_name, 'VARIABLE', ':EXTERNAL')
                    return PackageManager.find(symbol_name, package_name, status_check, env='VARIABLE')[symbol_name]

        elif not isinstance(forms, Cons):
            # If an atom is given, returns itself.
            return forms

        else:
            # Binds function object.
            if isinstance(forms.car, Cons):
                # If lambda expression is given, it is bound as an anonymous function.
                # Other than that, it is bound from predefined functions involving special
                func = cls.eval(forms.car, var_env, func_env, macro_env)

            else:
                # Gets symbol_name, package_name, and status_check.
                func_name, package_name, status_check = PackageManager.split_symbol_name(forms.car.value)

                # Gets the function binded by the symbol.
                # When package_name is None, the package becomes PackageManager.current_package.
                try:
                    # Tries to get the function from lexical environment in current package.
                    func = func_env.find(func_name)[func_name]
                except LookupError:
                    # Tries to get the function from another package.
                    func = PackageManager.find(func_name, package_name, status_check, env='FUNCTION')[func_name]

            return func(forms.cdr, var_env, func_env, macro_env)
