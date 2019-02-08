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


from clispy.function_ import Lambda, SpecialOperator
from clispy.type import Cons, Null, Symbol


class Evaluator(object):
    @classmethod
    def eval(cls, forms, package_manager):
        if isinstance(forms, Symbol):
            return package_manager.find(forms, env='VARIABLE')[forms.value]
        elif not isinstance(forms, Cons):
            return forms
        elif forms.car is Symbol('LAMBDA'):
            return Lambda(forms.cdr, package_manager)
        else:
            # Binds function object.
            # If lambda expression is given, it is bound as an anonymous function.
            # Other than that, it is bound from predefined functions involving special
            if isinstance(forms.car, Cons) and (forms.car.car is Symbol('LAMBDA')):
                func = cls.eval(forms.car, package_manager)
            else:
                func = package_manager.find(forms.car, env='FUNCTION')[forms.car.value]

            # Binds function arguments.
            # If func is instance of special operator, it dose not evaluate arguments.
            # Ohter than that, it evaluates arguments in advance.
            if isinstance(func, SpecialOperator):
                return func(forms.cdr, package_manager)
            else:
                exps = forms.cdr

                args = []
                while exps is not Null():
                    args.append(cls.eval(exps.car, package_manager))
                    exps = exps.cdr

                return func(*args)
