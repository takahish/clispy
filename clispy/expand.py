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


def _expand_quasiquote(x):
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
        return [symbol.APPEND, x[0][1], _expand_quasiquote(x[1:])]
    else:
        return [symbol.CONS, _expand_quasiquote(x[0]), _expand_quasiquote(x[1:])]

def _replace_expression(x, old, new):
    """Replace expression in nested list.

    Args:
        x: Abstract syntax tree of common lisp consisted of list.

    Retruns:
        Abstract syntax tree replaced old expression to new expression.
    """
    if not isinstance(x, list) or len(x) == 0:
        if x == old:
            return new
        else:
            return x
    else:
        return [_replace_expression(x[0], old, new)] + _replace_expression(x[1:], old, new)

def closure(symbol, env, _eval):
    """Generate _expand function with global environment.

    Args:
        symbol: clispy.symbol module.
        env: clispy.symbol module.
        _eval: eval function generated clispy.eval.closure.

    Returns:
        _expand function.
    """

    # macro space environment
    global_macro_env = env.MacroEnv()

    def _expand(x, macro_env=global_macro_env):
        """Walk tree of x, making optimizations/fixes, and sinaling SyntaxError

        Args:
            x: Abstract syntax tree of common lisp consisted of list.
            macro_env: Macro environment.

        Returns:
            Results of expansion.
        """
        util.require(x, x != [])                   # () => Error
        if not isinstance(x, list):                # constant => unchanged
            return x
        elif x[0] is symbol.QUOTE:                 # (quote exp)
            util.require(x, len(x) == 2)
            return x
        elif x[0] is symbol.IF:
            if len(x) == 3:
                x = x + [False]                    # (if t c) => (if t c nil)
            util.require(x, len(x) == 4)
            return [_expand(xi) for xi in x]
        elif x[0] is symbol.SETQ:
            util.require (x, len(x) == 3)
            var = x[1]
            util.require(x, isinstance(var, symbol.Symbol), msg="can set! only a symbol")
            return [symbol.SETQ, var, _expand(x[2])]
        elif x[0] is symbol.DEFUN or x[0] is symbol.DEFMACRO:
            if len(x) >= 4:                        # (defun f (args) body)
                                                   #  => (defun f (lambda (args) body))
                _def, f, args, body = x[0], x[1], x[2], x[3:]
                if isinstance(args, list) and args:
                    return _expand([_def, f, [symbol.LAMBDA, args] + body])
            else:
                util.require(x, len(x) == 3)       # (defun non-var/list exp) => Error
                _def, f, exp = x[0], x[1], x[2]
                exp = _expand(x[2])
                if _def is symbol.DEFMACRO:        # (defmacro v exp)
                                                   #  => None; add {f: exp} to function env
                    proc = _eval(exp)
                    util.require(x, callable(proc), "macro must be a purocedure")
                    try:
                        macro_env.find(f)[f] = proc
                    except LookupError:
                        macro_env[f] = proc
                    return macro_env[f]
                return [symbol.DEFUN, f, exp]
        elif x[0] is symbol.PROGN:
            if len(x) == 1:
                return False                       # (progn) => NIL
            else:
                return [_expand(xi) for xi in x]
        elif x[0] is symbol.LAMBDA:                # (lambda (x) e1 e2)
            util.require(x, len(x) >= 3)           #  => (lambda (x) (progn e1 e2))
            vars, body = x[1], x[2:]
            util.require(x, (isinstance(vars, list) and all(isinstance(v, symbol.Symbol) for v in vars))
                         or isinstance(vars, symbol.Symbol), "illegal lambda argument list")
            exp = body[0] if len(body) == 1 else [symbol.PROGN] + body
            return [symbol.LAMBDA, vars, _expand(exp)]
        elif x[0] is symbol.QUASIQUOTE:            # `x => expand_quasiquote(x)
            util.require(x, len(x) == 2)
            return _expand_quasiquote(x[1])
        elif isinstance(x[0], symbol.Symbol) and x[0] in macro_env:
            return _expand(macro_env.find(x[0])[x[0]](*x[1:]))
                                                   # (m arg...) => macroexpand if m isinstance macro
        else:
            return [_expand(xi) for xi in x]

    # _expand function closured in global environment.
    return _expand
