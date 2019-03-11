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

import copy
from clispy.function import Function
from clispy.package import PackageManager
from clispy.type import Null, String, Symbol


# ==============================================================================
# Defines base classes.
#
#     Function
# ==============================================================================


class Lambda(Function):
    """Lambda expression is a function definition that is not bound to an identifier.
    Anonymous functions are often arguments being passed to higher-order functions,
    or used for constructing the result of a higher-order function that needs to
    return a function. If the function is only used once, or a limited number of
    times, an anonymous function may be syntactically lighter than using a named
    function.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates Lambda.
        """
        cls.__name__ = 'LAMBDA'
        return object.__new__(cls)

    def __init__(self, forms, var_env, func_env, macro_env):
        """Initialize Lambda.
        """
        params, body = forms.car, forms.cdr.car

        # Binds params and args.
        self.params = []
        while params is not Null():
            self.params.append(params.car.value)
            params = params.cdr

        # Finds accessor and checks index.
        self.accessor_index = {}
        if '&OPTIONAL' in self.params:
            self.accessor_index['&OPTIONAL'] = self.params.index('&OPTIONAL')
            self.params.pop(self.accessor_index['&OPTIONAL'])

        if '&REST' in self.params:
            self.accessor_index['&REST'] = self.params.index('&REST')
            self.params.pop(self.accessor_index['&REST'])

        # Binds forms.
        self.forms = body

        # Lexical environments
        self.var_env = var_env
        self.func_env = func_env
        self.macro_env = macro_env

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of Lambda.
        """
        from clispy.evaluator import Evaluator
        from clispy.expander import Expander

        # # Expands and evaluates arguments.
        args = []
        while forms is not Null():
            exp = Expander.expand(forms.car, var_env, func_env, macro_env)
            args.append(Evaluator.eval(exp, var_env, func_env, macro_env))
            forms = forms.cdr

        # Sets args for parameter type.
        if '&OPTIONAL' in self.accessor_index:
            while len(args[self.accessor_index['&OPTIONAL']:]) < len(self.params[self.accessor_index['&OPTIONAL']:]):
                args.append(Null())

        if '&REST' in self.accessor_index:
            if len(args) > self.accessor_index['&REST']:
                args[self.accessor_index['&REST']] = args[self.accessor_index['&REST']:]
                args = args[:self.accessor_index['&REST']+1]
            else:
                args.append(Null())

        # Interns symbol names of params to lexical scope.
        for symbol_name in self.params:
            PackageManager.intern(String(symbol_name))

        # Binds args to lexical scope.
        local_var_env = self.var_env.extend(params=self.params, args=args)
        local_func_env = self.func_env.extend()
        local_macro_env = self.macro_env.extend()

        # Expands and evaluates lambda expression.
        exp = Expander.expand(self.forms, local_var_env, local_func_env, local_macro_env)
        retval = Evaluator.eval(exp, local_var_env, local_func_env, local_macro_env)

        return retval
