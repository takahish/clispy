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
from clispy import util
from clispy import func


class Expander(object):
    """Provide a method to expand abstract syntax tree.
    """
    def __init__(self, evaluator, global_macro_env):
        """Inits Expander with evaluator and global macro environment.
        """
        self.evaluator = evaluator
        self.global_macro_env = global_macro_env

    def expand(self, x, macro_env=None):
        """Walk tree of x, making optimizations/fixes, and sinaling SyntaxError

        Args:
            x: Abstract syntax tree of common lisp consisted of list.
            macro_env: Macro environment.

        Returns:
            Results of expansion.
        """
        util.require(x, x != [])                   # () => Error

        if macro_env is None:
            macro_env = self.global_macro_env

        if not isinstance(x, list):                # constant => unchanged
            return x
        elif x[0] is symbol.QUOTE:                 # (quote exp)
            util.require(x, len(x) == 2)
            return x
        elif x[0] is symbol.IF:
            if len(x) == 3:
                x = x + [False]                    # (if t c) => (if t c nil)
            util.require(x, len(x) == 4)
            return [self.expand(xi, macro_env) for xi in x]
        elif x[0] is symbol.SETQ:
            if len(x) == 1:                        # (setq) => NIL
                return False
            else:
                pairs = x[1:]
                if len(pairs) % 2 == 1:            # (setq a 1 b) => (setq a 1 b nil)
                    pairs.append(False)
                x = [symbol.SETQ]
                for var, exp in zip(*[iter(pairs)]*2):
                    util.require(x, isinstance(var, symbol.Symbol), msg="can set! only a symbol")
                    x = x + [var, exp]
                return x
        elif x[0] is symbol.DEFUN or x[0] is symbol.DEFMACRO:
            if len(x) >= 4:                        # (defun name args body) => (defun name (lambda args body))
                _def, name, args, body = x[0], x[1], x[2], x[3:]
                if func._null(args):
                    args = []
                return self.expand([_def, name, [symbol.LAMBDA, args] + body], macro_env)
            else:
                util.require(x, len(x) == 3)       # (defun non-var/list exp) => Error
                _def, name, exp = x[0], x[1], x[2]
                exp = self.expand(x[2], macro_env)
                if _def is symbol.DEFMACRO:        # (defmacro v exp) => None; add {f: exp} to function env
                    proc = self.evaluator.eval(exp)
                    util.require(x, callable(proc), "macro must be a purocedure")
                    try:
                        macro_env.find(name)[name] = proc
                    except LookupError:
                        macro_env[name] = proc
                    return macro_env[name]
                return [symbol.DEFUN, name, exp]
        elif x[0] is symbol.PROGN:
            if len(x) == 1:
                return False                       # (progn) => NIL
            else:
                return [self.expand(xi, macro_env) for xi in x]
        elif x[0] is symbol.LAMBDA:                # (lambda (x) e1 e2) => (lambda (x) (progn e1 e2))
            util.require(x, len(x) >= 3)
            vars, body = x[1], x[2:]
            util.require(x, (isinstance(vars, list) and all(isinstance(v, symbol.Symbol) for v in vars))
                         or isinstance(vars, symbol.Symbol), "illegal lambda argument list")
            exp = body[0] if len(body) == 1 else [symbol.PROGN] + body
            return [symbol.LAMBDA, vars, self.expand(exp, macro_env)]
        elif x[0] is symbol.QUASIQUOTE:            # `x => __expand_quasiquote(x)
            util.require(x, len(x) == 2)
            return self.__expand_quasiquote(x[1])
        elif isinstance(x[0], symbol.Symbol) and x[0] in macro_env:
            return self.expand(macro_env.find(x[0])[x[0]](*x[1:]), macro_env)
                                                   # (m arg...) => macroexpand if m isinstance macro
        else:
            return [self.expand(xi, macro_env) for xi in x]


    ########## Helper methods ##########

    def __expand_quasiquote(self, x):
        """Expand `x => 'x; `,x => x; `(,@x y) => (append x y).

        Args:
            x: Abstract syntax tree of common lisp consisted of list.

        Returns:
            Abstract syntax tree expanded quasiquote.
        """
        if not func._consp(x):
            return [symbol.QUOTE, x]
        util.require(x, x[0] is not symbol.UNQUOTE_SPLICING, "can't splice here")
        if x[0] is symbol.UNQUOTE:
            util.require(x, len(x) == 2)
            return x[1]
        elif func._consp(x[0]) and x[0][0] is symbol.UNQUOTE_SPLICING:
            util.require(x[0], len(x[0]) == 2)
            return [symbol.APPEND, x[0][1], self.__expand_quasiquote(x[1:])]
        else:
            return [symbol.CONS, self.__expand_quasiquote(x[0]), self.__expand_quasiquote(x[1:])]
