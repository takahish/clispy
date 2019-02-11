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
from clispy.function_ import Function
from clispy.type import Null, String


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

    def __init__(self, forms, package_manager):
        """Initialize Lambda.
        """
        params, body = forms.car, forms.cdr.car

        # Bind params and args.
        self.params = []
        while params is not Null():
            self.params.append(params.car.value)
            params = params.cdr

        # Bind forms.
        self.forms = body

        # Deepcopy current scope for lexical scope.
        self.lexical_package_manager = copy.deepcopy(package_manager)

    def __call__(self, args):
        """Behavior of Lambda.
        """
        from clispy.evaluator_ import Evaluator

        # Converts args into python list.
        args = args.value

        # Intern symbol names of params to lexical scope.
        for symbol_name in self.params:
            self.lexical_package_manager.intern(String(symbol_name))

        # Binds args to lexical scope.
        local_variable_scope = self.lexical_package_manager.current_package.space['VARIABLE'].extend(params=self.params, args=args)
        self.lexical_package_manager.current_package.space['VARIABLE'] = local_variable_scope

        # Evaluate lambda expression
        return Evaluator.eval(
            self.forms,
            self.lexical_package_manager
        )
