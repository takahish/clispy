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

from clispy import env
from clispy import symbol
from clispy.utils import require
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
            x: Abstract syntax tree of lisp, consisted of python list.
            macro_env: Macro environment.

        Returns:
            Results of expansion.
        """
        require(x, x != [])                        # () => Error

        if macro_env is None:
            macro_env = self.global_macro_env

        if not isinstance(x, list):                # constant => unchanged
            return x
        elif x[0] is symbol.DEFUN:                 # (defun sym (lambda args body))
            return self.__defun(x, macro_env)
        elif x[0] is symbol.DEFMACRO:              # (defmacro sym (lambda args body))
            return self.__defmacro(x, macro_env)
        elif x[0] is symbol.LAMBDA:                # (lambda (var) body)
            return self.__lambda(x, macro_env)
        elif x[0] is symbol.QUASIQUOTE:            # `x => (quasiquote x)
            return self.__quasiquote(x)
        elif x[0] is symbol.QUOTE:                 # Special form: quote, (quote exp)
            return self.__quote(x)
        elif x[0] is symbol.IF:                    # Special form: if, (if test-form then-form else-form)
            return self.__if(x, macro_env)
        elif x[0] is symbol.SETQ:                  # Special form: setq, (setq var val)
            return self.__setq(x)
        elif x[0] is symbol.PROGN:                 # Special form: progn, (progn exp+)
            return self.__progn(x, macro_env)
        elif x[0] is symbol.FUNCTION:              # Special form: function, (function func)
            return self.__function(x, macro_env)
        elif x[0] is symbol.MACROLET:              # Special form: macrolet, (macrolet ((macro var exp)) body)
            return self.__macrolet(x, macro_env)
        elif x[0] is symbol.BLOCK:                 # Special form: block, (block name)
            return self.__block(x, macro_env)
        elif x[0] is symbol.RETURN_FROM:           # Special form: return-from, (block name (return-from name))
            return self.__return_from(x, macro_env)
        elif x[0] is symbol.CATCH:                 # Special form: catch, (catch 'tag)
            return self.__catch(x, macro_env)
        elif x[0] is symbol.THROW:                 # Special form: throw, (catch 'tag (throw 'tag retval))
            return self.__throw(x, macro_env)
        elif isinstance(x[0], symbol.Symbol) and x[0] in macro_env:
                                                   # (m arg...) => macroexpand if m isinstance macro
            return self.__expand_macro(x, macro_env)
        else:
            return self.__expand_recur(x, macro_env)


    ########## Special forms ##########

    def __quote(self, x):
        """quote just returns object.

        Args:
            x: Abstract syntax tree of lisp, consisted of python list.

        Returns:
            An expression.
        """
        require(x, len(x) == 2)
        return x

    def __if(self, x, macro_env):
        """if allows the execution of a form to be dependent on a single test-form.

        Args:
            x: Abstract syntax tree of lisp, consisted of python list.
            macro_env: Macro environment.

        Returns:
            A series of forms
        """
        if len(x) == 3: # (if t c) => (if t c nil)
            x = x + [False]
        require(x, len(x) == 4)
        return [self.expand(xi, macro_env) for xi in x]

    def __setq(self, x):
        """setq is simple variable assignment of common lisp.

        Args:
            x: Abstract syntax tree of lisp, consisted of python list.

        Returns:
            A series of forms.
        """
        if len(x) == 1:  # (setq) => NIL
            return False
        else:
            pairs = x[1:]
            if len(pairs) % 2 == 1: # (setq a 1 b) => (setq a 1 b nil)
                pairs.append(False)
            x = [symbol.SETQ]
            for var, exp in zip(*[iter(pairs)] * 2):
                require(x, isinstance(var, symbol.Symbol), msg="can set! only a symbol")
                x = x + [var, exp]
            return x

    def __progn(self, x, macro_env):
        """progn evaluates forms, in the order in which they are given.

        Args:
            x: Abstract syntax tree of lisp, consisted of python list.
            macro_env: Macro environment.

        Returns:
            A series of forms.
        """
        if len(x) == 1: # (progn) => NIL
            return False
        else:
            return self.__expand_recur(x, macro_env)

    def __function(self, x, macro_env):
        """The value of function is the functional value name in the current lexical
        environment.

        Args:
            x: Abstract syntax tree of lisp, consisted of python list.
            macro_env: Variable environment.

        Returns:
            A series of forms.
        """
        require(x, len(x) == 2)
        require(x, isinstance(x[1], symbol.Symbol), "an argument must be symbol")
        return x

    def __macrolet(self, x, macro_env):
        """macrolet establishes local macro definitions, using the same format used by defmacro.

        Args:
            x: Abstract syntax tree of lisp, consisted of python list.

        Returns:
            A series of forms and new macro bindings.
        """
        require(x, len(x) >= 2)

        bindings, body = x[1], x[2:]

        local_macro_env = env.MacroEnv([], [], macro_env)

        for binding in bindings:
            name, exp = binding[0], binding[1:]

            exp = [symbol.LAMBDA] + exp
            exp = self.expand(exp, macro_env)

            proc = self.evaluator.eval(exp)
            require(x, callable(proc), "macro must be a purocedure")

            try:
                local_macro_env.find(name)[name] = proc
            except LookupError:
                local_macro_env[name] = proc

        x = [symbol.PROGN] + body
        return self.expand(x, local_macro_env)

    def __block(self, x, macro_env):
        """block establishes a block named name and then evaluates forms as an implicit
        progn.

        Args:
            x: Abstract syntax tree of lisp, consisted of python list.

        Returns:
            A series of forms and new macro bindings.
        """
        require(x, len(x) >= 2 and isinstance(x[1], symbol.Symbol), "block name must be symbol")
        if len(x) == 2: # (block empty) => nil
            return False
        else:
            return self.__expand_recur(x, macro_env)

    def __return_from(self, x, macro_env):
        """Return control from a lexically enclosing block.

        Args:
            x: Abstract syntax tree of lisp, consisted of python list.

        Returns:
            A series of forms and new macro bindings.
        """
        require(x, len(x) >= 2 and isinstance(x[1], symbol.Symbol), "block name must be symbol")

        # if there is no exp, nil appended.
        if len(x) == 2:
            x.append(False)

        require(x, len(x) == 3)
        return self.__expand_recur(x, macro_env)

    def __catch(self, x, macro_env):
        """catch is used as the destination of a non-local control transfer by throw.

        Args:
            x: Abstract syntax tree of lisp, consisted of python list.

        Returns:
            A series of forms and new macro bindings.
        """
        require(x, len(x) >= 2)
        if len(x) == 2:  # (catch 'empty) => nil
            return False
        else:
            return self.__expand_recur(x, macro_env)

    def __throw(self, x, macro_env):
        """throw causes a non-local control transfer to a catch whose tag is eq to tag.

        Args:
            x: Abstract syntax tree of lisp, consisted of python list.

        Returns:
            A series of forms and new macro bindings.
        """
        require(x, len(x) >= 2)

        # if there is no exp, nil appended.
        if len(x) == 2:
            x.append(False)

        require(x, len(x) == 3)
        return self.__expand_recur(x, macro_env)


    ########## Helper methods ##########

    def __quasiquote(self, x):
        """Expand `x => 'x; `,x => x; `(,@x y) => (append x y).

        Args:
            x: Abstract syntax tree of common lisp consisted of list.

        Returns:
            Abstract syntax tree expanded quasiquote.
        """
        require(x, len(x) == 2)
        return self.__quasiquote_recur(x[1])

    def __quasiquote_recur(self, x):
        """Expand `x => 'x; `,x => x; `(,@x y) => (append x y).

        Args:
            x: Abstract syntax tree of common lisp consisted of list.

        Returns:
            Abstract syntax tree expanded quasiquote.
        """
        if not func._consp(x):
            return [symbol.QUOTE, x]

        require(x, x[0] is not symbol.UNQUOTE_SPLICING, "can't splice here")

        if x[0] is symbol.UNQUOTE:
            require(x, len(x) == 2)
            return x[1]
        elif func._consp(x[0]) and x[0][0] is symbol.UNQUOTE_SPLICING:
            require(x[0], len(x[0]) == 2)
            return [symbol.APPEND, x[0][1], self.__quasiquote_recur(x[1:])]
        else:
            return [symbol.CONS, self.__quasiquote_recur(x[0]), self.__quasiquote_recur(x[1:])]

    def __defun(self, x, macro_env):
        """Expand a series of forms for defun.

        Args:
            x: Abstract syntax tree of lisp, consisted of python list.
            macro_env: Macro environment.

        Returns:
            A series of forms.
        """
        if len(x) >= 4: # (defun name args body) => (defun name (lambda args body))
            defun, name, args, body = x[0], x[1], x[2], x[3:]
            if func._null(args): # (def name nil body) => (def name [] body)
                args = []
            # Implicit BLOCK.
            body = [[symbol.BLOCK, name] + body]
            return self.expand([defun, name, [symbol.LAMBDA, args] + body], macro_env)
        else:
            require(x, len(x) == 3) # (defun non-var/list exp) => Error
            defun, name, exp = x[0], x[1], x[2]
            exp = self.expand(exp, macro_env)
            return [defun, name, exp]

    def __defmacro(self, x, macro_env):
        """Expand a series of forms for defmacro.

        Args:
            x: Abstract syntax tree of lisp, consisted of python list.
            macro_env: Macro environment.

        Returns:
            A series of forms or defined macro symbol.
        """
        if len(x) >= 4: # (defun name args body) => (defun name (lambda args body))
            defun, name, args, body = x[0], x[1], x[2], x[3:]
            if func._null(args): # (def name nil body) => (def name [] body)
                args = []
            return self.__defmacro([defun, name, [symbol.LAMBDA, args] + body], macro_env)
        else:
            require(x, len(x) == 3) # (defun non-var/list exp) => Error
            _def, name, exp = x[0], x[1], x[2]
            exp = self.expand(x[2], macro_env)

            proc = self.evaluator.eval(exp)
            require(x, callable(proc), "macro must be a purocedure")
            try:
                macro_env.find(name)[name] = proc
            except LookupError:
                macro_env[name] = proc
            return [symbol.QUOTE, name] # quote is needed for eval and print

    def __lambda(self, x, macro_env):
        """Make instance of Procedure that is lambda expression.

        Args:
            x: Abstract syntax tree of lisp, consisted of python list.
            macro_env: Macro environment.

        Returns:
            A procedure.
        """
        require(x, len(x) >= 3)
        vars, body = x[1], x[2:]
        require(x, (isinstance(vars, list) and all(isinstance(v, symbol.Symbol) for v in vars))
                      or isinstance(vars, symbol.Symbol), "illegal lambda argument list")

        # Set expression in lambda.
        # There is BLOCK in body, this lambda is defined by defun,
        # or there is one expression in body.
        if len(body) == 1 or body[0][0] is symbol.BLOCK:
            exp = body[0]
        else:
            # Implicit progn for some body.
            exp = [symbol.PROGN] + body

        return [symbol.LAMBDA, vars, self.expand(exp, macro_env)]

    def __expand_macro(self, x, macro_env):
        """Expand macro.

        Args:
            x: Abstract syntax tree of lisp, consisted of python list.
            macro_env: Macro environment.

        Returns:
            A series of forms expanded macro.
        """
        return self.expand(macro_env.find(x[0])[x[0]](*x[1:]), macro_env)

    def __expand_recur(self, x, macro_env):
        """Expand recursion.

        Args:
            x: Abstract syntax tree of lisp, consisted of python list.
            macro_env: Macro environment.

        Returns:
            A series of forms
        """
        return [self.expand(xi, macro_env) for xi in x]
