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

from clispy import cons
from clispy import symbol


def _cons_cell(lst):
    """Create cons sell (or dotted pair) object.

    Args:
        lst: abstract syntax tree of common lisp consisted of list.

    Returns:
        cons cell (Cons or DottedPair).
    """
    if len(lst) > 2 and lst[-2] == symbol.DOT:
        if lst[-1] is not False:
            lst.remove(symbol.DOT)
            return cons.DottedPair(lst)
        else:
            return lst[:-2]
    else:
        return lst

def closure(symbol, env, func):
    """Generate _eval function with global environment.

    Args:
        symbol: clispy.symbol module.
        env: clispy.env module.
        func: clispy.func moudle.

    Return:
        _eval function.
    """

    # variable space environment
    global_var_env = env.VarEnv()

    # function space environment
    global_func_env = env.FuncEnv()
    global_func_env.update(func.BuiltInFunction())

    class _Procedure(object):
        """A user-defined common lisp procedure.
        And an instance localizes environment, when it is evaluated.
        """
        def __init__(self, params, exps, var_env, func_env):
            """Inits _Procedure with parameters, expression and environment.
            """
            self.params = params
            self.exps = exps

            # Environment
            self.var_env = var_env
            self.func_env = func_env

        def __call__(self, *args):
            """Make _Procedure to be callable
            """
            return _eval(
                self.exps,
                env.VarEnv(self.params, args, self.var_env),
                env.FuncEnv([], [], self.func_env)
            )

    def _eval(x, var_env=global_var_env, func_env=global_func_env):
        """Evaluate an expression in an environment.

        Args:
            x: Abstract syntax tree of common lisp consisted of list.
            var_env: Variable environment.
            func_env: Function environment.

        Returns:
            Results of evaluation.
        """
        while True:
            if isinstance(x, symbol.Symbol):          # variable reference
                return var_env.find(x)[x]
            elif not isinstance(x, list):             # constant literal
                return x
            elif x[0] is symbol.QUOTE:                # (quote exp)
                (_, exp) = x
                if isinstance(exp, list):             # list literal
                    return _cons_cell(exp)
                return exp
            elif x[0] is symbol.IF:                   # if test conseq alt
                (_, test, conseq, alt) = x
                x = (conseq if _eval(test, var_env, func_env) else alt)
            elif x[0] is symbol.DEFUN:
                (_, func, exp) = x                    # (defun func var exp)
                func_env[func] = _eval(exp, var_env, func_env)
                return func_env[func]
            elif x[0] is symbol.SETQ:                 # (setq var exp)
                (_, var, exp) = x
                var_env[var] = _eval(exp, var_env, func_env)
                return var_env[var]
            elif x[0] is symbol.LAMBDA:               # (lambda (var...) body)
                (_, params, exp) = x
                return _Procedure(params, exp, var_env, func_env)
            elif x[0] is symbol.PROGN:                # (progn exp+)
                for exp in x[1:-1]:
                    _eval(exp, var_env, func_env)
                x = x[-1]
            elif x[0] is symbol.FUNCTION:             # (function func)
                (_, func) = x
                return func_env.find(func)[func]
            elif x[0] is symbol.FUNCALL:              # (funcall func args)
                (_, func, *exps) = x
                proc = var_env.find(func)[func]
                exps = [_eval(exp, var_env, func_env) for exp in exps]
                return proc(*exps)
            elif x[0] is symbol.LET:                  # Special form: let, (let ((var val) ...) body)
                x, var_env = _let(x, var_env, func_env)
            elif x[0] is symbol.LET_ASTER:            # Special form: let*, (let* ((var val) ...) body)
                x, var_env = _let_aster(x, var_env, func_env)
            elif x[0] is symbol.FLET:                 # Special form: flet, (flet ((func var exp) ...) body)
                x, func_env = _flet(x, var_env, func_env)
            elif x[0] is symbol.LABELS:               # Special form: labels, (labels ((func var exp) ...) body)
                x, func_env = _labels(x, var_env, func_env)
            else:
                if isinstance(x[0], symbol.Symbol):
                    proc = func_env.find(x[0])[x[0]]
                elif isinstance(x[0], list) and x[0][0] is symbol.LAMBDA:
                    proc = _eval(x[0], var_env, func_env)
                exps = [_eval(exp, var_env, func_env) for exp in x[1:]]
                if isinstance(proc, _Procedure):
                    x = proc.exps
                    var_env = env.VarEnv(proc.params, exps, proc.var_env)
                    func_env = env.FuncEnv([], [], proc.func_env)
                else:
                    return proc(*exps)


    ########## Special forms ##########

    def _let(x, var_env, func_env):
        """let create new variable bindings and execute a series of forms that
        use these bindings. let performs the bindings in parallel.

        Args:
            x: Abstract syntax tree of common lisp consisted of list.
            var_env: Variable environment.
            func_env: Function environment.

        Return:
            A series of forms and new variable bindings.
        """
        bindings, body = x[1], x[2:]
        vars, exps = zip(*bindings)

        vals = []
        for exp in exps:
            vals.append(_eval(exp, var_env, func_env))

        # The bindings in parallel.
        var_env = env.VarEnv(vars, vals, var_env)

        x = [symbol.PROGN] + body
        return x, var_env

    def _let_aster(x, var_env, func_env):
        """let* create new variable bindings and execute a series of forms that
        use these bindings. let* performs the bindings in sequential.

        Args:
            x: Abstract syntax tree of common lisp consisted of list.
            var_env: Variable environment.
            func_env: Function environment.

        Return:
            A series of forms and new variable bindings.
        """
        bindings, body = x[1], x[2:]

        for var, exp in bindings:
            val = _eval(exp, var_env, func_env)
            # The bindings in sequential.
            var_env = env.VarEnv([var], [val], var_env)

        x = [symbol.PROGN] + body
        return x, var_env

    def _flet(x, var_env, func_env):
        """flet defines locally named functions and execute a series of forms with
        these definition bindings. Any number of such local function can be defined.
        The scope of the name bindings encompasses only the body.

        Args:
            x: Abstract syntax tree of common lisp consisted of list.
            var_env: Variable environment.
            func_env: Function environment.

        Return:
            A series of forms and new function bindings.
        """
        bindings, body = x[1], x[2:]

        local_func_env = env.FuncEnv([], [], func_env)
        for binding in bindings:
            func, exp = binding[0], binding[1:]
            exp = [symbol.LAMBDA] + exp
            # The scope of the name bindings encompasses only the body.
            local_func_env[func] = _eval(exp, var_env, func_env)

        x = [symbol.PROGN] + body
        return x, local_func_env

    def _labels(x, var_env, func_env):
        """labels is equivalent to flet except that the scope the defined function
        names for labels encompasses the function definitions themselves as well as
        the body.

        Args:
            x: Abstract syntax tree of common lisp consisted of list.
            var_env: Variable environment.
            func_env: Function environment.

        Returns:
            A series of forms and new function bindings.
        """
        bindings, body = x[1], x[2:]

        local_func_env = env.FuncEnv([], [], func_env)
        for binding in bindings:
            func, exp = binding[0], binding[1:]
            exp = [symbol.LAMBDA] + exp
            # The scope of the name bindings encompasses the function definitions
            # themselves as well as the body.
            local_func_env[func] = _eval(exp, var_env, local_func_env)

        x = [symbol.PROGN] + body
        return x, local_func_env


    # _eval function closured in global environment.
    return _eval
