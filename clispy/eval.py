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
from clispy import env
from clispy import symbol


class Procedure(object):
    """A user-defined common lisp procedure.
    And an instance localizes environment, when it is evaluated.
    """
    def __init__(self, evaluator, params, exps, var_env, func_env):
        """Inits _Procedure with parameters, expression and environment.
        """
        # Evaluator.
        self.evaluator = evaluator

        # Parameters and Expressions.
        self.params = params
        self.exps = exps

        # Environment.
        self.var_env = var_env
        self.func_env = func_env

    def __call__(self, *args):
        """Make _Procedure to be callable
        """
        return self.evaluator.eval(
            self.exps,
            env.VarEnv(self.params, args, self.var_env),
            env.FuncEnv([], [], self.func_env)
        )


class Evaluator(object):
    """Provide a method to evaluate abstract syntax tree.
    """
    def __init__(self, global_var_env, global_func_env):
        """Inits Evaluator with global variable and function environment.
        """
        self.global_var_env = global_var_env
        self.global_func_env = global_func_env

    def eval(self, x, var_env=None, func_env=None):
        """Evaluate an expression in an environment.

        Args:
            x: Abstract syntax tree of common lisp consisted of list.
            var_env: Variable environment.
            func_env: Function environment.

        Returns:
            Results of evaluation.
        """
        if var_env is None:
            var_env = self.global_var_env

        if func_env is None:
            func_env = self.global_func_env

        while True:
            if isinstance(x, symbol.Symbol):          # variable reference
                return var_env.find(x)[x]
            elif not isinstance(x, list):             # constant literal
                return x
            elif x[0] is symbol.QUOTE:                # (quote exp)
                (_, exp) = x
                if isinstance(exp, list):             # list literal
                    return self.__cons_cell(exp)
                return exp
            elif x[0] is symbol.IF:                   # if test conseq alt
                (_, test, conseq, alt) = x
                x = (conseq if self.eval(test, var_env, func_env) else alt)
            elif x[0] is symbol.DEFUN:
                (_, func, exp) = x                    # (defun func var exp)
                func_env[func] = self.eval(exp, var_env, func_env)
                return func
            elif x[0] is symbol.SETQ:                 # Special form: setq, (setq var exp ...)
                x, var_env = self.__setq(x, var_env, func_env)
                return x
            elif x[0] is symbol.LAMBDA:               # (lambda (var...) body)
                (_, params, exp) = x
                return Procedure(self, params, exp, var_env, func_env)
            elif x[0] is symbol.PROGN:                # (progn exp+)
                for exp in x[1:-1]:
                    self.eval(exp, var_env, func_env)
                x = x[-1]
            elif x[0] is symbol.FUNCTION:             # (function func)
                (_, func) = x
                return func_env.find(func)[func]
            elif x[0] is symbol.FUNCALL:              # (funcall func args)
                (_, func, *exps) = x
                proc = var_env.find(func)[func]
                exps = [self.eval(exp, var_env, func_env) for exp in exps]
                return proc(*exps)
            elif x[0] is symbol.LET:                  # Special form: let, (let ((var val) ...) body)
                x, var_env = self.__let(x, var_env, func_env)
            elif x[0] is symbol.LET_ASTER:            # Special form: let*, (let* ((var val) ...) body)
                x, var_env = self.__let_aster(x, var_env, func_env)
            elif x[0] is symbol.FLET:                 # Special form: flet, (flet ((func var exp) ...) body)
                x, func_env = self.__flet(x, var_env, func_env)
            elif x[0] is symbol.LABELS:               # Special form: labels, (labels ((func var exp) ...) body)
                x, func_env = self.__labels(x, var_env, func_env)
            else:
                if isinstance(x[0], symbol.Symbol):
                    proc = func_env.find(x[0])[x[0]]
                elif isinstance(x[0], list) and x[0][0] is symbol.LAMBDA:
                    proc = self.eval(x[0], var_env, func_env)
                exps = [self.eval(exp, var_env, func_env) for exp in x[1:]]
                return proc(*exps)


    ########## Helper methods ##########

    def __cons_cell(self, lst):
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


    ########## Special forms ##########

    def __setq(self, x, var_env, func_env):
        """setq is simple variable assignment of common lisp.

        Args:
            x: Abstract syntax tree of common lisp consisted of list.

        Returns:
            Last assigned value.
        """
        for var, exp in zip(*[iter(x[1:])]*2):
            val = self.eval(exp, var_env, func_env)
            try:
                var_env.find(var)[var] = val
            except LookupError:
                var_env[var] = val
        return val, var_env

    def __let(self, x, var_env, func_env):
        """let create new variable bindings and execute a series of forms that
        use these bindings. let performs the bindings in parallel.

        Args:
            x: Abstract syntax tree of common lisp consisted of list.
            var_env: Variable environment.
            func_env: Function environment.

        Returns:
            A series of forms and new variable bindings.
        """
        bindings, body = x[1], x[2:]
        vars, exps = zip(*bindings)

        vals = []
        for exp in exps:
            vals.append(self.eval(exp, var_env, func_env))

        # The bindings are in parallel.
        var_env = env.VarEnv(vars, vals, var_env)

        x = [symbol.PROGN] + body
        return x, var_env

    def __let_aster(self, x, var_env, func_env):
        """let* create new variable bindings and execute a series of forms that
        use these bindings. let* performs the bindings in sequential.

        Args:
            x: Abstract syntax tree of common lisp consisted of list.
            var_env: Variable environment.
            func_env: Function environment.

        Returns:
            A series of forms and new variable bindings.
        """
        bindings, body = x[1], x[2:]

        for var, exp in bindings:
            val = self.eval(exp, var_env, func_env)
            # The bindings are in sequential.
            var_env = env.VarEnv([var], [val], var_env)

        x = [symbol.PROGN] + body
        return x, var_env

    def __flet(self, x, var_env, func_env):
        """flet defines locally named functions and execute a series of forms with
        these definition bindings. Any number of such local function can be defined.
        The scope of the name bindings encompasses only the body.

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
            # The scope of the name bindings encompasses only the body.
            local_func_env[func] = self.eval(exp, var_env, func_env)

        x = [symbol.PROGN] + body
        return x, local_func_env

    def __labels(self, x, var_env, func_env):
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
            local_func_env[func] = self.eval(exp, var_env, local_func_env)

        x = [symbol.PROGN] + body
        return x, local_func_env
