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

import symbol
import env
import eval
import func
import util

def _expand_quasiquote(x):
    """Expand `x => 'x; `,x => x; `(,@x y) => (append x y).
    """
    if not func._consp(x):
        return [symbol._quote, x]
    util._require(x, x[0] is not symbol._unquote_splicing, "can't splice here")
    if x[0] is symbol._unquote:
        util._require(x, len(x)==2)
        return x[1]
    elif func._consp(x[0]) and x[0][0] is symbol._unquote_splicing:
        util._require(x[0], len(x[0])==2)
        return [symbol._append, x[0][1], _expand_quasiquote(x[1:])]
    else:
        return [symbol._cons, _expand_quasiquote(x[0]), _expand_quasiquote(x[1:])]

def _replace_expression(exp, old, new):
    """Replace expression in nested list.
    """
    if not isinstance(exp, list) or len(exp) == 0:
        if exp == old:
            return new
        else:
            return exp
    else:
        return [_replace_expression(exp[0], old, new)] + _replace_expression(exp[1:], old, new)

def _expand(x, macro_env=env._macro_env):
    """Walk tree of x, making optimizations/fixes, and sinaling SyntaxError
    """
    util._require(x, x!=[])                         # () => Error
    if not isinstance(x, list):                # constant => unchanged
        return x
    elif x[0] is symbol._quote:                # (quote exp)
        util._require(x, len(x)==2)
        return x
    elif x[0] is symbol._if:
        if len(x) == 3:
            x = x + [False]                    # (if t c) => (if t c nil)
        util._require(x, len(x)==4)
        return [_expand(xi) for xi in x]
    elif x[0] is symbol._setq:
        util._require (x, len(x)==3)
        var = x[1]
        util._require(x, isinstance(var, symbol._Symbol), msg="can set! only a symbol")
        return [symbol._setq, var, _expand(x[2])]
    elif x[0] is symbol._defun or x[0] is symbol._defmacro:
        if len(x) >= 4:                        # (defun f (args) body)
                                               #  => (defun f (lambda (args) body))
            _def, f, args, body = x[0], x[1], x[2], x[3:]
            if isinstance(args, list) and args:
                return _expand([_def, f, [symbol._lambda, args]+body])
        else:
            util._require(x, len(x)==3)             # (defun non-var/list exp) => Error
            _def, f, exp = x[0], x[1], x[2]
            exp = _expand(x[2])
            if _def is symbol._defmacro:       # (defmacro v exp)
                                               #  => None; add {f: exp} to function env
                proc = eval._eval(exp)
                util._require(x, callable(proc), "macro must be a purocedure")
                try:
                    macro_env.find(f)[f] = proc
                except LookupError:
                    macro_env[f] = proc
                return macro_env[f]
            return [symbol._defun, f, exp]
    elif x[0] is symbol._progn:
        if len(x) == 1:
            return False                       # (progn) => NIL
        else:
            return [_expand(xi) for xi in x]
    elif x[0] is symbol._lambda:               # (lambda (x) e1 e2)
        util._require(x, len(x) >= 3)               # => (lambda (x) (progn e1 e2))
        vars, body = x[1], x[2:]
        util._require(x, (isinstance(vars, list) and all(isinstance(v, symbol._Symbol) for v in vars))
                 or isinstance(vars, symbol._Symbol), "illegal lambda argument list")
        exp = body[0] if len(body) == 1 else [symbol._progn] + body
        return [symbol._lambda, vars, _expand(exp)]
    elif x[0] is symbol._quasiquote:           # `x => expand_quasiquote(x)
        util._require(x, len(x)==2)
        return _expand_quasiquote(x[1])
    elif x[0] is symbol._let:                  # (let ((var val)) body) => ((lambda (var) body) val)
        return _let(x)
    elif x[0] is symbol._flet:                 # (flet ((func var exp)) body)
        return _flet(x)                        #  => (progn body_replaced_func_to_lambda)
    elif isinstance(x[0], symbol._Symbol) and x[0] in macro_env:
        return _expand(macro_env.find(x[0])[x[0]](*x[1:]))
                                               # (m arg...) => macroexpand if m isinstance macro
    else:
        util._require(x, isinstance(x[0], symbol._Symbol)
                 or (isinstance(x[0], list) and x[0][0] is symbol._lambda),
                 "illegal function object")
        return [_expand(xi) for xi in x]

def _let(exp):
    """let special form
    """
    util._require(exp, len(exp) > 2)  # => ((lambda (var) body) val)
    bindings, body = exp[1], exp[2:]
    util._require(exp, all(isinstance(b, list) and len(b) == 2 and isinstance(b[0], symbol._Symbol)
                         for b in bindings), "illegal bindig list")
    vars, vals = zip(*bindings)
    return _expand([[symbol._lambda, list(vars)] + list(map(_expand, body))] + list(map(_expand, vals)))

def _flet(exp):
    """flet special form
    """
    util._require(exp, len(exp) == 3)
    bindings, body = exp[1], exp[2:]
    util._require(exp, all(isinstance(b, list) and len(b) == 3 and isinstance(b[0], symbol._Symbol)
                         and isinstance(b[1], list) and isinstance(b[2], list)
                         for b in bindings), "illegal binding list")
    for binding in bindings:
        body = _replace_expression(body, binding[0], [symbol._lambda, binding[1], binding[2]])
    return _expand([symbol._progn] + list(map(_expand, body)))
