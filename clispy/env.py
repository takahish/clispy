# Copyright 2018 Takahiro Ishikawa. All Rights Reserved.
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

from clispy import symbol


class _Env(dict):
    """An environment is a dict of {'var': val} pairs with an outer _Env.
    """
    def __init__(self, params=(), args=(), outer=None):
        """Inits _Env with parameters, arguments and outer environment.
        """
        # Bind param list to corresponding args, or single param to list of args
        self.outer = outer
        if isinstance(params, symbol.Symbol):
            self.update({params: list(args)})
        else:
            # bind rest parameters for lambda
            params, args = _Env._parse_rest_argument(params, args)
            if len(args) != len(params):
                raise TypeError('expected %s, given %s, ' % (params, args))
            self.update(zip(params, args))

    def find(self, var):
        """Find the innermost Env where var appears.

        Args:
            var: Variable for looking up.

        Returns:
            Value of variable. If own environment don't have value,
            this method looks up variable from outer environment.
        """
        if var in self:
            return self
        elif self.outer is None:
            raise LookupError(var)
        else:
            return self.outer.find(var)

    @staticmethod
    def _parse_rest_argument(params, args):
        """Fine rest argument to parse params and args.

        Args:
            params: Parameters include &rest or &body notation.
            args: Arguments setted to params.

        Returns:
            Parameters and arguments.
        """
        if '&REST' in params or '&BODY' in params:
            params, args = list(params), list(args) # for slicing and appending
            # params=['x', '&rest', 'y'], args=[1, 2, 3, 4, 5]
            #  => params=['x', 'y'], args=[1, [2, 3, 4, 5]]
            try:
                rest_index = params.index('&REST')
            except ValueError:
                rest_index = params.index('&BODY')
            params = params[:rest_index] + params[rest_index+1:]
            args = args[:rest_index] + [args[rest_index:]]
        return params, args

class VarEnv(_Env):
    """Environment for variable.
    """
    pass

class FuncEnv(_Env):
    """Environment for function.
    """
    pass

class MacroEnv(_Env):
    """Environment for Macro.
    """
    pass
