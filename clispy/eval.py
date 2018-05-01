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

from clispy.symbol import *
from clispy.cons import DottedPair
from clispy.env import VarEnv, FuncEnv
from clispy.utils import callcc


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
            x: Abstract syntax tree of lisp, consisted of python list.
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
            if isinstance(x, Symbol):          # variable reference
                return self.__refer(x, var_env)
            elif not isinstance(x, list):      # constant literal
                return x
            elif x[0] is DEFUN:                # (defun sym (lambda args body))
                return self.__defun(x, var_env, func_env)
            elif x[0] is LAMBDA:               # (lambda (var) body)
                return self.__lambda(x, var_env, func_env)
            elif x[0] is QUOTE:                # Special form: quote, (quote exp)
                return self.__quote(x)
            elif x[0] is IF:                   # Special form: if, (if test-form then-form else-form)
                x = self.__if(x, var_env, func_env)
            elif x[0] is SETQ:                 # Special form: setq, (setq var exp)
                return self.__setq(x, var_env, func_env)
            elif x[0] is PROGN:                # Special form: progn, (progn exp+)
                x = self.__progn(x, var_env, func_env)
            elif x[0] is FUNCTION:             # Special form: function, (function func)
                x = self.__function(x, var_env, func_env)
            elif x[0] is LET:                  # Special form: let, (let ((var val)) body)
                x, var_env = self.__let(x, var_env, func_env)
            elif x[0] is LET_ASTER:            # Special form: let*, (let* ((var val)) body)
                x, var_env = self.__let_aster(x, var_env, func_env)
            elif x[0] is FLET:                 # Special form: flet, (flet ((func var exp)) body)
                x, func_env = self.__flet(x, var_env, func_env)
            elif x[0] is LABELS:               # Special form: labels, (labels ((func var exp)) body)
                x, func_env = self.__labels(x, var_env, func_env)
            elif x[0] is BLOCK:                # Special form: block, (block name)
                return self.__block(x, var_env, func_env)
            elif x[0] is RETURN_FROM:          # Special form: return-from, (block name (return-from name))
                return self.__return_from(x, var_env, func_env)
            elif x[0] is TAGBODY:              # Special form: tagbody, (tagbody tag)
                return self.__tagbody(x, var_env, func_env)
            elif x[0] is GO:                   # Special form: go, (tagbody tag (go tag))
                return self.__go(x, var_env)
            elif x[0] is CATCH:                # Special form: catch, (catch 'tag)
                x = self.__catch(x, var_env, func_env)
            elif x[0] is THROW:                # Special form: throw, (catch 'tag (throw 'tag retval))
                return self.__throw(x, var_env, func_env)
            else:
                return self.__execute(x, var_env, func_env)


    ########## Special forms ##########

    def __quote(self, x):
        """quote just returns object.

        Args:
            x: Abstract syntax tree of lisp, consisted of python list.

        Returns:
            An expression.
        """
        _, exp = x
        if isinstance(exp, list): # List literal is converted to cons cell.
            return self.__cons(exp)
        return exp

    def __if(self, x, var_env, func_env):
        """if allows the execution of a form to be dependent on a single test-form.
        First test-form is evaluated. If the result is true, then then-form is selected;
        otherwise else-form is selected. Which form is selected is then evaluated.

        Args:
            x: Abstract syntax tree of lisp, consisted of python list.
            var_env: Variable environment.
            func_env: Function environment.

        Returns:
            A series of forms
        """
        (_, test_form, then_form, else_form) = x
        x = (then_form if self.eval(test_form, var_env, func_env) else else_form)
        return x

    def __setq(self, x, var_env, func_env):
        """setq is simple variable assignment of common lisp.

        Args:
            x: Abstract syntax tree of lisp, consisted of python list.

        Returns:
            Last assigned symbol.
        """
        for var, exp in zip(*[iter(x[1:])]*2):
            val = self.eval(exp, var_env, func_env)
            try:
                var_env.find(var)[var] = val
            except LookupError:
                var_env[var] = val
        return val

    def __progn(self, x, var_env, func_env):
        """progn evaluates forms, in the order in which they are given. The values
        of each form but the last are discarded. If progn appears as a top level
        form, then all forms within that progn are considered by the compiler to be
        top level form.

        Args:
            x: Abstract syntax tree of lisp, consisted of python list.
            var_env: Variable environment.
            func_env: Function environment.

        Returns:
            A series of forms.
        """
        for exp in x[1:-1]:
            self.eval(exp, var_env, func_env)
        x = x[-1]
        return x

    def __function(self, x, var_env, func_env):
        """The value of function is the functional value name in the current lexical
        environment.

        Args:
            x: Abstract syntax tree of lisp, consisted of python list.
            var_env: Variable environment.
            func_env: Function environment.

        Returns:
            A callable object.
        """
        (_, exp) = x
        if isinstance(exp, list) and exp[0] is LAMBDA:
            x = self.__lambda(exp, var_env, func_env)
        else:
            x = func_env.find(exp)[exp]
        return x

    def __let(self, x, var_env, func_env):
        """let create new variable bindings and execute a series of forms that
        use these bindings. let performs the bindings in parallel.

        Args:
            x: Abstract syntax tree of lisp, consisted of python list.
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
        var_env = VarEnv(vars, vals, var_env)

        x = [PROGN] + body
        return x, var_env

    def __let_aster(self, x, var_env, func_env):
        """let* create new variable bindings and execute a series of forms that
        use these bindings. let* performs the bindings in sequential.

        Args:
            x: Abstract syntax tree of lisp, consisted of python list.
            var_env: Variable environment.
            func_env: Function environment.

        Returns:
            A series of forms and new variable bindings.
        """
        bindings, body = x[1], x[2:]

        for var, exp in bindings:
            val = self.eval(exp, var_env, func_env)
            # The bindings are in sequential.
            var_env = VarEnv([var], [val], var_env)

        x = [PROGN] + body
        return x, var_env

    def __flet(self, x, var_env, func_env):
        """flet defines locally named functions and execute a series of forms with
        these definition bindings. Any number of such local function can be defined.
        The scope of the name bindings encompasses only the body.

        Args:
            x: Abstract syntax tree of lisp, consisted of python list.
            var_env: Variable environment.
            func_env: Function environment.

        Returns:
            A series of forms and new function bindings.
        """
        bindings, body = x[1], x[2:]

        local_func_env = FuncEnv([], [], func_env)
        for binding in bindings:
            func, exp = binding[0], binding[1:]
            exp = [LAMBDA] + exp

            # The scope of the name bindings encompasses only the body.
            local_func_env[func] = self.eval(exp, var_env, func_env)

        x = [PROGN] + body
        return x, local_func_env

    def __labels(self, x, var_env, func_env):
        """labels is equivalent to flet except that the scope the defined function
        names for labels encompasses the function definitions themselves as well as
        the body.

        Args:
            x: Abstract syntax tree of lisp, consisted of python list.
            var_env: Variable environment.
            func_env: Function environment.

        Returns:
            A series of forms and new function bindings.
        """
        bindings, body = x[1], x[2:]

        local_func_env = FuncEnv([], [], func_env)
        for binding in bindings:
            func, exp = binding[0], binding[1:]
            exp = [LAMBDA] + exp
            # The scope of the name bindings encompasses the function definitions
            # themselves as well as the body.
            local_func_env[func] = self.eval(exp, var_env, local_func_env)

        x = [PROGN] + body
        return x, local_func_env

    def __block(self, x, var_env, func_env):
        """block establishes a block named name and then evaluates forms as an implicit
        progn. The special operators block and return-from work together to probide a
        structured, lexical, non-local exit facility.

        Args:
            x: Abstract syntax tree of lisp, consisted of python list.
            var_env: Variable environment.
            func_env: Function environment.

        Returns:
            A series of forms and new function bindings.
        """
        (_, name, exp) = x[0], x[1], x[2:]

        # throw is param of lambda in call/cc.
        throw = [name]
        # An implicit progn.
        exp = [PROGN] + exp

        # call/cc is used to control.
        x = callcc(Procedure(self, throw, exp, var_env, func_env))
        return x

    def __return_from(self, x, var_env, func_env):
        """Return control from a lexically enclosing block. A block form named must
        lexically enclose the occurrence of return-from; any vlaue yield by the
        evaluation of result are immediately returned from the innermost such lexically
        enclosing block.

        Args:
            x: Abstract syntax tree of lisp, consisted of python list.
            var_env: Variable environment.
            func_env: Function environment.

        Returns:
            A series of forms and new function bindings.
        """
        (_, name, exp) = x[0], x[1], x[2]

        # name is param of lambda and have throw function as value in call/cc.
        x = var_env.find(name)[name](exp)
        return x

    def __tagbody(self, x, var_env, func_env):
        """Execute zero or more statements in a lexical environment that provides for
        control transfers labels indicated by the tag. The statements in a tagbody
        are evaluated in order from left to right, and their values are discarded. If
        at any time there are no remaining statements, tagbody returns nil. However if
        (go tag) is evaluated, control jumps to the part of the body labeled with tag.

        Args:
            x: Abstract syntax tree of lisp, consisted of python list.
            var_env: Variable environment.
            func_env: Function environment.

        Returns:
            False (NIL).
        """
        (_, register) = x[0], x[1:]

        pointer = 0
        while True:
            # there is no expression in register.
            if pointer >= len(register) - 1:
                break

            exp = register[pointer]

            # exp is tag.
            if isinstance(exp, Symbol):
                pointer = pointer + 1
                exp = register[pointer]

            # NIL is param of lambda in call/cc.
            # NIL is converted to False implicitly in parse, so it never collide.
            x = callcc(Procedure(self, [NIL], exp, var_env, func_env))

            # exp is (go tag), then pointer is jumped to tag.
            if isinstance(x, Symbol) and x in register:
                pointer = register.index(x)
                continue

            pointer = pointer + 1

        # tagbody returns nil.
        x = False
        return x

    def __go(self, x, var_env):
        """go transfers control to the point in the body an enclosing tagbody from
        labeled by a tag. If there is no such tag in the body, the bodies of lexically
        containing tagbody forms (if any) are examined as well.

        Args:
            x: Abstract syntax tree of lisp, consisted of python list.
            var_env: Variable environment.

        Returns:
            A tag (symbol).
        """
        (_, tag) = x

        # NIL is param of lambda and have throw function as value in call/cc.
        # NIL is converted to False implicitly in parse, so it never collide.
        x = var_env.find(NIL)[NIL](tag)
        return x

    def __catch(self, x, var_env, func_env):
        """catch is used as the destination of a non-local control transfer by throw.
        Tags are used to find the catch to which a throw is transferring control.
        (catch 'foo form) catches a (throw 'tag form) but not a (throw 'bar form).

        Args:
            x: Abstract syntax tree of lisp consisted of python list.
            var_env: Variable environment.
            func_env: Function environment.

        Returns:
             A series of forms.
        """
        (_, tag, exp) = x[0], x[1], x[2:]

        tag = self.eval(tag, var_env, func_env)
        # implicit progn.
        exp = [PROGN] + exp

        # catch differs from block in that catch tags have dynamic scope.
        try:
            x = self.eval(exp, var_env, func_env)
        except ControlError as e:
            if e.tag == tag:
                x = e.retval
            else:
                raise e

        return x

    def __throw(self, x, var_env, func_env):
        """throw causes a non-local control transfer to a catch whose tag is eq to tag.

        Args:
            x: Abstract syntax tree of lisp consisted of python list.
            var_env: Variable environment.
            func_env: Function environment.

        Returns:
             A series of forms.
        """
        (_, tag, exp) = x[0], x[1], x[2]

        tag = self.eval(tag, var_env, func_env)
        x = self.eval(exp, var_env, func_env)

        raise ControlError("attempt to throw to the nonexistent tag " + str(tag), tag, x)


    ########## Helper methods ##########

    def __cons(self, lst):
        """Create cons sell (or dotted pair) object.

        Args:
            lst: Abstract syntax tree of lisp, consisted of python list.

        Returns:
            cons cell (Cons or DottedPair).
        """
        if len(lst) > 2 and lst[-2] == DOT:
            if lst[-1] is not False:
                lst.remove(DOT)
                return DottedPair(lst)
            else:
                return lst[:-2]
        else:
            return lst

    def __refer(self, x, var_env):
        """Reference value bound variable:

        Args:
            x: Abstract syntax tree of lisp, consisted of python list.
            var_env: Variable environment.

        Returns:
             An value.
        """
        return var_env.find(x)[x]

    def __defun(self, x, var_env, func_env):
        """Set procedure to func_env. Procedure is lambda expression.

        Args:
            x: Abstract syntax tree of lisp, consisted of python list.

        Returns:
            Assigned symbol
        """
        (_, sym, exp) = x # x is (defun sym (lambda args body))
        func_env[sym] = self.eval(exp, var_env, func_env)
        return sym

    def __lambda(self, x, var_env, func_env):
        """Make instance of Procedure that is lambda expression.

        Args:
            x: Abstract syntax tree of lisp, consisted of python list.

        Returns:
            A procedure.
        """
        (_, params, exp) = x
        return Procedure(self, params, exp, var_env, func_env)

    def __execute(self, x, var_env, func_env):
        """Execute expression.

        Args:
            x: Abstract syntax tree of lisp, consisted of python list.
            var_env: Variable environment.
            func_env: Function environment.

        Returns:
            An executed value
        """
        if isinstance(x[0], Symbol):
            proc = func_env.find(x[0])[x[0]]
        elif isinstance(x[0], list) and x[0][0] is LAMBDA:
            proc = self.eval(x[0], var_env, func_env)
        exps = [self.eval(exp, var_env, func_env) for exp in x[1:]]
        return proc(*exps)


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
        """Make _Procedure to be callable.
        """
        return self.evaluator.eval(
            self.exps,
            VarEnv(self.params, args, self.var_env),
            FuncEnv([], [], self.func_env)
        )


class ControlError(Exception):
    """Control error of eval is raised by special form throw.
    """
    def __init__(self, message, tag, retval):
        """Inits ControlError with message, tag and return value.
        """
        super().__init__(message)
        self.tag = tag
        self.retval = retval
