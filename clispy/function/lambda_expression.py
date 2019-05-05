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
from clispy.type import Cons, Keyword, Null, String, Symbol


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
        from clispy.evaluator import Evaluator
        from clispy.expander import Expander

        params, body = forms.car, forms.cdr.car

        # Binds params and args.
        self.params = []
        while params is not Null():
            if isinstance(params.car, Cons):
                self.params.append(params.car)
            else:
                self.params.append(params.car.value)
            params = params.cdr


        ##################################################
        # Accesser parameters
        ##################################################

        # Finds accessor and checks index.
        self.accessor_index = {}

        # Optional parameters.
        self.default_optional_args = []
        if '&OPTIONAL' in self.params:
            self.accessor_index['&OPTIONAL'] = self.params.index('&OPTIONAL')
            self.params.pop(self.accessor_index['&OPTIONAL'])

            for param in self.params[self.accessor_index['&OPTIONAL']:]:
                if isinstance(param, Cons):
                    # Expands and evaluates default value.
                    exp = Expander.expand(param.cdr.car, var_env, func_env, macro_env)
                    x = Evaluator.eval(exp, var_env, func_env, macro_env)

                    # Sets a default value as an arguments
                    self.default_optional_args.append(x)

                else:
                    self.default_optional_args.append(Null())

        # Rest parameters.
        if '&REST' in self.params:
            self.accessor_index['&REST'] = self.params.index('&REST')
            self.params.pop(self.accessor_index['&REST'])

        # Keyword parameters.
        self.default_keyword_args = {}
        if '&KEY' in self.params:
            self.accessor_index['&KEY'] = self.params.index('&KEY')
            self.params.pop(self.accessor_index['&KEY'])

            for param in self.params[self.accessor_index['&KEY']:]:
                if isinstance(param, Cons):
                    # Expand and evaluate default value.
                    exp = Expander.expand(param.cdr.car, var_env, func_env, macro_env)
                    x = Evaluator.eval(exp, var_env, func_env, macro_env)

                    # Sets a default value as an arguments
                    self.default_keyword_args[param.car.value] = x

                else:
                    # If param is not Cons, it is already value (str) of symbol.
                    self.default_keyword_args[param] = Null()

        # Remove cons objects.
        for i, param in enumerate(self.params):
            if isinstance(param, Cons):
                self.params[i] = param.car.value


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
        tmp_args = []
        while forms is not Null():
            exp = Expander.expand(forms.car, var_env, func_env, macro_env)
            tmp_args.append(Evaluator.eval(exp, var_env, func_env, macro_env))
            forms = forms.cdr

        args = tmp_args


        ##################################################
        # Accesser parameters
        ##################################################

        # Optional parameters.
        if '&OPTIONAL' in self.accessor_index:
            args = tmp_args[:self.accessor_index['&OPTIONAL']]
            optional_args = tmp_args[self.accessor_index['&OPTIONAL']:]

            for i, _ in enumerate(self.params[self.accessor_index['&OPTIONAL']:]):
                try:
                    args.append(optional_args[i])
                except IndexError:
                    args.append(self.default_optional_args[i])

        # Rest parameters.
        if '&REST' in self.accessor_index:
            if len(args) > self.accessor_index['&REST']:
                args[self.accessor_index['&REST']] = args[self.accessor_index['&REST']:]
                args = args[:self.accessor_index['&REST']+1]
            else:
                args.append(Null())

        # Keyword parameters
        if '&KEY' in self.accessor_index:
            args = tmp_args[:self.accessor_index['&KEY']]
            keyword_args = tmp_args[self.accessor_index['&KEY']:]

            kargs = {}
            for i, karg in enumerate(keyword_args):
                if isinstance(karg, Keyword):
                    # A value of Keyword has ':' at the first character.
                    kargs[karg.value[1:]] = keyword_args[i+1]

            for param in self.params[self.accessor_index['&KEY']:]:
                args.append(kargs.get(param, self.default_keyword_args.get(param)))


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
