# Copyright 2025 Takahiro Ishikawa. All Rights Reserved.
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

from clispy.callcc import CallCC
from clispy.function import Function, Lambda
from clispy.package import Environment, PackageManager, assign_helper, use_package_helper
from clispy.type import BuiltInClass, Symbol, Null, Cons, String


# ==============================================================================
# Defines base classes.
#
#     SpecialOperator
# ==============================================================================

class SpecialOperator(Function):
    """A special forms is a form with special syntax, special evaluation rules
    or both, possibly manipulating the evaluation environment, control flow,
    or both.

    The set of special operator names is fixed in common lisp; no way is provided
    for the user to define a special operator. The next figure lists all of the
    common lisp symbols that have definitions as special operators.

    # Common lisp special operators
    #
    #     block      let*                  return-from
    #     catch      load-time-value       setq
    #     eval-when  locally               symbol-macrolet
    #     flet       macrolet              tagbody
    #     function   multiple-value-call   the
    #     go         multiple-value-prog1  throw
    #     if         progn                 unwind-protect
    #     labels     progv
    #     let        quote
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates SpecialOperator.
        """
        cls.__name__ = 'SPECIAL-OPERATOR'
        return object.__new__(cls)

    def __repr__(self):
        """The official string representation.
        """
        return "#<SPECIAL-OPERATOR {0} {{{1:X}}}>".format(self.__class__.__name__, id(self))


class _GoSignal(RuntimeWarning):
    """Internal exception used to implement ``tagbody``/``go`` control flow.
    """
    def __init__(self, tag):
        super().__init__(tag)
        self.tag = tag


class _ThrowSignal(RuntimeWarning):
    """Internal exception used to implement ``catch``/``throw`` control flow.
    """
    def __init__(self, tag, value):
        super().__init__(tag)
        self.tag = tag
        self.value = value


# ==============================================================================
# Defines special operator classes.
#
#     Block       Let*                ReturnFrom
#     Catch       LoadTimeValue       Setq
#     EvalWhen    Locally             SymbolMacrolet
#     Flet        Macrolet            Tagbody
#     Function_   MultipleValueCall   The
#     Go          MultipleValueProg1  Throw
#     If          Progn               UnwindProtect
#     Labels      Progv
#     Let         Quote
# ==============================================================================


class BlockSpecialOperator(SpecialOperator):
    """block establishes a block and then evaluates forms as an implicit progn.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates BlockSpecialOperator.
        """
        cls.__name__ = 'BLOCK'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of BlockSpecialOperator.
        """
        from clispy.evaluator import Evaluator

        # throw is a param of lambda in call/cc.
        lambda_forms = Cons(Cons(forms.car, Null()), forms.cdr)

        # call/cc is used to control.
        callcc = CallCC(Lambda(lambda_forms, var_env, func_env, macro_env))

        return callcc(var_env, func_env, macro_env)


class CatchSpecialOperator(SpecialOperator):
    """catch is used as the destination of a non-local control transfer by throw.
    Tags are used to find the catch to which a throw is transferring control.
    (catch 'foo forms) catches a (throw 'foo forms) but not a (throw 'bar forms).
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates CatchSpecialOperator.
        """
        cls.__name__ = 'CATCH'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of CatchSpecialOperator.
        """
        from clispy.evaluator import Evaluator

        tag = Evaluator.eval(forms.car, var_env, func_env, macro_env)
        body = forms.cdr

        try:
            retval = Null()
            while body is not Null():
                retval = Evaluator.eval(body.car, var_env, func_env, macro_env)
                body = body.cdr
            return retval
        except _ThrowSignal as signal:
            if signal.tag == tag:
                return signal.value
            raise


class EvalWhenSpecialOperator(SpecialOperator):
    """The body of an eval-when forms is processed as an implicit progn, but only
    in the situations listed.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates EvalWhenSpecialOperator.
        """
        cls.__name__ = 'EVAL-WHEN'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of EvalWhenSpecialOperator.
        """
        return forms  # TODO: To implement the behavior.


class FletSpecialOperator(SpecialOperator):
    """flet, labels, and macrolet define local functions and macros, and execute
    forms using the local definitions. forms are executed in order of occurrence.

    The body forms (but not the lambda list) of each function created by flet
    and labels and each macro created by macrolet are enclosed in an implicit
    block whose name is the function block name of the function-name or name,
    as appropriate.

    flet defines locally named functions and executes a series of forms with
    these definition bindings. Any number of such local functions can be defined.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates FletSpecialOperator.
        """
        cls.__name__ = 'FLET'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of FletSpecialOperator.
        """
        from clispy.evaluator import Evaluator

        bindings, body = forms.car, forms.cdr.car

        funcs = []
        exps = []
        while bindings is not Null():
            func, exp = bindings.car.car, bindings.car.cdr

            # Interns symbol that represents function name into current package.
            PackageManager.intern(String(func.value))

            funcs.append(func.value)
            exp = Cons(Symbol('LAMBDA'), exp)

            # The scope of the name bindings does not encompass the function definitions.
            exps.append(Evaluator.eval(exp, var_env, func_env, macro_env))

            bindings = bindings.cdr

        # The bindings are in parallel.
        func_env = func_env.extend(params=funcs, args=exps)

        return Evaluator.eval(body, var_env, func_env, macro_env)


class FunctionSpecialOperator(SpecialOperator):
    """The value of function is the functional value of name in the current
    lexical environment.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates FunctionSpecialOperator.
        """
        cls.__name__ = 'FUNCTION'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of FunctionSpecialOperator.
        """
        if isinstance(forms.car, Cons) and forms.car.car is Symbol('LAMBDA'):
            return Lambda(forms.car.cdr, var_env, func_env, macro_env)
        else:
            # Gets symbol_name, package_name, and status_check.
            func_name, package_name, status_check = PackageManager.split_symbol_name(forms.car.value)

            # Gets the function binded by the symbol.
            try:
                # First, tries to get the value from lexical environment.
                return func_env.find(func_name)[func_name]
            except LookupError:
                # If LookupError is raised, tries to get from another package.
                return PackageManager.find(func_name, package_name, status_check, env='FUNCTION')[func_name]


class GoSpecialOperator(SpecialOperator):
    """go transfers control to the point in the body of an enclosing tagbody
    forms labeled by a tag eql to tag.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates GoSpecialOperator.
        """
        cls.__name__ = 'GO'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of GoSpecialOperator.

        A ``go`` form causes a non-local transfer of control within the
        dynamically enclosing :class:`TagbodySpecialOperator`.  The tag name is
        passed to the continuation supplied by ``tagbody`` which then resumes
        execution starting at the referenced label.
        """

        tag = forms.car
        if isinstance(tag, Symbol):
            label = tag.value
        else:
            label = str(getattr(tag, "value", tag))

        raise _GoSignal(label)


class IfSpecialOperator(SpecialOperator):
    """if allows the execution of a form to be dependent on a single test-form.

    First test-form is evaluated. If the result is true, then then-form is selected;
    otherwise else-form is selected. Whichever form is selected is then evaluated.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates IfSpecialOperator.
        """
        cls.__name__ = 'IF'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of IfSpecialOperator.
        """
        from clispy.evaluator import Evaluator

        # Sets test-form, then-form and else-form.
        test_form, then_form, else_form = forms.car, forms.cdr.car, forms.cdr.cdr.car

        # If-then-else
        if Evaluator.eval(test_form, var_env, func_env, macro_env) is Null():
            return Evaluator.eval(else_form, var_env, func_env, macro_env)
        else:
            return Evaluator.eval(then_form, var_env, func_env, macro_env)


class LabelsSpecialOperator(SpecialOperator):
    """flet, labels, and macrolet define local functions and macros, and execute
    forms using the local definitions. forms are executed in order of occurrence.

    The body forms (but not the lambda list) of each function created by flet
    and labels and each macro created by macrolet are enclosed in an implicit
    block whose name is the function block name of the function-name or name,
    as appropriate.

    labels is equivalent to flet except that the scope of the defined function
    names for labels encompasses the function definitions themselves as well
    as the body.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates LabelsSpecialOperator.
        """
        cls.__name__ = 'LABELS'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of FletSpecialOperator.
        """
        from clispy.evaluator import Evaluator

        bindings, body = forms.car, forms.cdr.car

        funcs = []
        exps = []

        # For encompassing the function definitions themselves,
        # a function environment is extended in advance.
        local_func_env = func_env.extend()

        while bindings is not Null():
            func, exp = bindings.car.car, bindings.car.cdr

            # Interns symbol that represents function name into current package.
            PackageManager.intern(String(func.value))

            funcs.append(func.value)
            exp = Cons(Symbol('LAMBDA'), exp)

            # The scope of the name bindings encompasses the function definitions
            # themselves as well as the body.
            exps.append(Evaluator.eval(exp, var_env, local_func_env, macro_env))

            bindings = bindings.cdr

        # The bindings are in parallel.
        local_func_env.update(zip(funcs, exps))

        return Evaluator.eval(body, var_env, local_func_env, macro_env)


class LetSpecialOperator(SpecialOperator):
    """let and let* create new variable bindings and execute a series of forms
    that use these bindings. let performs the bindings in parallel and let* does
    them sequentially.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates LetSpecialOperator.
        """
        cls.__name__ = 'LET'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of LetSpecialOperator.
        """
        from clispy.evaluator import Evaluator

        bindings, body = forms.car, forms.cdr.car

        vars, vals = [], []
        while bindings is not Null():
            var, val = bindings.car.car, bindings.car.cdr.car

            # Interns symbol that represents function name into current package.
            PackageManager.intern(String(var.value))

            vars.append(var.value)
            vals.append(Evaluator.eval(val, var_env, func_env, macro_env))

            bindings = bindings.cdr

        # The bindings are in parallel.
        var_env = var_env.extend(params=vars, args=vals)

        return Evaluator.eval(body, var_env, func_env, macro_env)


class LetAsterSpecialOperator(SpecialOperator):
    """let and let* create new variable bindings and execute a series of forms
    that use these bindings. let performs the bindings in parallel and let* does
    them sequentially.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates LetAsterSpecialOperator.
        """
        cls.__name__ = 'LET*'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of LetAsterSpecialOperator.
        """
        from clispy.evaluator import Evaluator

        bindings, body = forms.car, forms.cdr.car

        while bindings is not Null():
            var, val = bindings.car.car, bindings.car.cdr.car

            # Interns symbol that represents function name into current package.
            PackageManager.intern(String(var.value))

            var = var.value
            val = Evaluator.eval(val, var_env, func_env, macro_env)

            # The bindings are in sequence.
            var_env = var_env.extend(params=[var], args=[val])

            bindings = bindings.cdr

        return Evaluator.eval(body, var_env, func_env, macro_env)


class LoadTimeValueSpecialOperator(SpecialOperator):
    """Load-time-value provides a mechanism for delaying evaluation of forms until
    the expression is in the run-time environment
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates LoadTimeValueSpecialOperator.
        """
        cls.__name__ = 'LOAD-TIME-VALUE'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of LoadTimeValueSpecialOperator.
        """
        return forms  # TODO: To implement the behavior.


class LocallySpecialOperator(SpecialOperator):
    """Sequentially evaluates a body of formss in a lexical environment where the
    given declarations have effect.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates LocallySpecialOperator.
        """
        cls.__name__ = 'LOCALLY'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of LocallySpecialOperator.
        """
        return forms  # TODO: To implement the behavior.


class MacroletSpecialOperator(SpecialOperator):
    """flet, labels, and macrolet define local functions and macros, and execute
    forms using the local definitions. forms are executed in order of occurrence.

    The body forms (but not the lambda list) of each function created by flet
    and labels and each macro created by macrolet are enclosed in an implicit
    block whose name is the function block name of the function-name or name,
    as appropriate.

    macrolet establishes local macro definitions, using the same format used
    by defmacro.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates MacroletSpecialOperator.
        """
        cls.__name__ = 'MACROLET'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of MacroletSpecialOperator.
        """
        return forms  # TODO: To implement the behavior.


class MultipleValueCallSpecialOperator(SpecialOperator):
    """Applies function to a list of the objects collected from groups of
    multiple values
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates MultipleValueCallSpecialOperator.
        """
        cls.__name__ = 'MULTIPLE-VALUE-CALL'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of MutipleValueCallSpecialOperator.
        """
        return forms  # TODO: To implement the behavior.


class MultipleValueProg1SpecialOperator(SpecialOperator):
    """multiple-value-prog1 evaluates first-form and saves all the values
    produced by that form. It then evaluates each form from left to right,
    discarding their values.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates MultipleValueProg1SpecialOperator.
        """
        cls.__name__ = 'MULTIPLE-VALUE-PROG1'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of MultipleValueProg1SpecialOperator.
        """
        return forms  # TODO: To implement the behavior.


class PrognSpecialOperator(SpecialOperator):
    """progn evaluates forms, in the order in which they are given.
    The values of each form but the last are discarded.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates PrognSpecialOperator.
        """
        cls.__name__ = 'PROGN'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of PrognSpecialOperator.
        """
        from clispy.evaluator import Evaluator

        # The values of eatch form but the last are discarded.
        while forms is not Null():
            last = Evaluator.eval(forms.car, var_env, func_env, macro_env)
            forms = forms.cdr

        return last


class ProgvSpecialOperator(SpecialOperator):
    """progv creates new dynamic variable bindings and executes each form
    using those bindings. Each form is evaluated in order.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates ProgvSpecialOperator.
        """
        cls.__name__ = 'PROGV'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of ProgvSpecialOperator.
        """
        return forms  # TODO: To implement the behavior.


class QuoteSpecialOperator(SpecialOperator):
    """The quote special operator just returns object.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates QuoteSpecialOperator.
        """
        cls.__name__ = 'QUOTE'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of QuoteSpecialOperator.
        """
        return forms.car


class ReturnFromSpecialOperator(SpecialOperator):
    """Returns control and multiple values[2] from a lexically enclosing block.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates ReturnFromSpecialOperator.
        """
        cls.__name__ = 'RETURN-FROM'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of ReturnFromSpecialOperator.
        """
        from clispy.evaluator import Evaluator

        block_name, body = forms.car.value, forms.cdr.car

        # Sets return value.
        retval = Evaluator.eval(body, var_env, func_env, macro_env)

        # During BlockSpecialOperator.__call__, the block is executed with a continuation created by CallCC.
        # This continuation is passed to the block as a PyObject wrapper in call/cc:
        #     self.args = Cons(PyObject(Invoke(self)), Null())
        # See BlockSpecialOperator.__call__ and clispy.callcc.CallCC for more details.
        return var_env.find(block_name)[block_name].value(retval)  # Unwrap PyObject and execute Invoke object.


class SetqSpecialOperator(SpecialOperator):
    """Assigns values to variables.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates SetqSpecialOperator.
        """
        cls.__name__ = 'SETQ'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of SetqSpecialOperator.
        """
        from clispy.evaluator import Evaluator

        while forms is not Null():
            symbol, value, forms = forms.car, forms.cdr.car, forms.cdr.cdr

            symbol_name = symbol.value
            value = Evaluator.eval(value, var_env, func_env, macro_env)

            # Interns symbol that represents function name into current package.
            PackageManager.intern(String(symbol_name))

            # setq may be used for assignment of both lexical and dynamic variables.
            try:
                var_env.find(symbol_name)[symbol_name] = value
            except LookupError:
                try:
                    # package_name=None means finding an environment from current package.
                    PackageManager.find(symbol_name, package_name=None, status_check=False)[symbol_name] = value
                except LookupError:
                    PackageManager.current_package.env['VARIABLE'][symbol_name] = value

        # the primary value of the last form
        return value


class SymbolMacroletSpecialOperator(SpecialOperator):
    """symbol-macrolet provides a mechanism for affecting the macro expansion
    environment for symbols.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates SymbolMacroletSpecialOperator.
        """
        cls.__name__ = 'SYMBOL-MACROLET'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of SymbolMacroletSpecialOperator.
        """
        return forms  # TODO: To implement the behavior.


class TagbodySpecialOperator(SpecialOperator):
    """Executes zero or more statements in a lexical environment that provides
    for control transfers to labels indicated by the tags."""
    def __new__(cls, *args, **kwargs):
        """Instantiates TagbodySpecialOperator."""
        cls.__name__ = 'TAGBODY'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of TagbodySpecialOperator.

        The body is executed sequentially while permitting non-local jumps
        signaled by :class:`GoSpecialOperator`. Each tag is paired with a
        continuation constructed from ``CallCC`` and ``Lambda`` so that
        evaluation can resume from the referenced label. Once execution
        completes without further jumps, ``tagbody`` returns ``nil``.
        """

        local_var_env = var_env.extend()
        label_map = {}

        TagbodySpecialOperator.__build_map(
            forms, label_map, local_var_env, func_env, macro_env
        )

        current_body = TagbodySpecialOperator.__strip_tags(forms)
        current = CallCC(
            Lambda(
                Cons(Cons(Symbol("__GO__"), Null()), current_body),
                local_var_env,
                func_env,
                macro_env,
            )
        )

        while True:
            try:
                current(local_var_env, func_env, macro_env)
                return Null()
            except _GoSignal as signal:
                target = signal.tag
                cont = label_map.get(target)
                if cont is None:
                    raise LookupError(target)
                current = cont

    @staticmethod
    def __strip_tags(seq):
        """__strip_tags separates tags and forms. The function is recursive, that is,
        1) if the form is empty, return Nil(). 2) If a car of the form is  Symbol,
        return a cdr applied by the function. 3) If a car of the form is Cons, return
        Cons of the car and a cdr applied by the function.
        """
        if seq is Null():
            return Null()
        form = seq.car
        rest = seq.cdr
        if isinstance(form, Symbol) or (hasattr(form, "value") and not isinstance(form, Cons)):
            return TagbodySpecialOperator.__strip_tags(rest)
        return Cons(form, TagbodySpecialOperator.__strip_tags(rest))

    @staticmethod
    def __build_map(seq, label_map, local_var_env, func_env, macro_env):
        """__build_map assigns the map of tags and forms. When it sets the map object,
        forms are wrapped by a CallCC and a Lambda class.
        """
        if seq is Null():
            return
        form = seq.car
        rest = seq.cdr
        if isinstance(form, Symbol) or (hasattr(form, "value") and not isinstance(form, Cons)):
            if isinstance(form, Symbol):
                label = form.value
            else:
                label = str(form.value)
            body_forms = TagbodySpecialOperator.__strip_tags(rest)
            lambda_forms = Cons(
                Cons(Symbol("__GO__"), Null()),
                body_forms,
            )
            label_map[label] = CallCC(
                Lambda(lambda_forms, local_var_env, func_env, macro_env)
            )
        TagbodySpecialOperator.__build_map(rest, label_map, local_var_env, func_env, macro_env)

class TheSpecialOperator(SpecialOperator):
    """the specifies that the values[1a] returned by form are of the types
    specified by value-type. The consequences are undefined if any result is
    not of the declared type.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates TheSpecialOperator.
        """
        cls.__name__ = 'THE'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of TheSpecialOperator.
        """
        return forms  # TODO: To implement the behavior.


class ThrowSpecialOperator(SpecialOperator):
    """throw causes a non-local control transfer to a catch whose tag is eq to tag.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates ThrowSpecialOperator.
        """
        cls.__name__ = 'THROW'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of ThrowSpecialOperator.
        """
        from clispy.evaluator import Evaluator

        tag = Evaluator.eval(forms.car, var_env, func_env, macro_env)
        result_form = forms.cdr.car if forms.cdr is not Null() else Null()
        value = Evaluator.eval(result_form, var_env, func_env, macro_env)
        raise _ThrowSignal(tag, value)


class UnwindProtectSpecialOperator(SpecialOperator):
    """unwind-protect evaluates protected-form and guarantees that cleanup-forms are
    executed before unwind-protect exits, whether it terminates normally or is
    aborted by a control transfer of some kind. unwind-protect is intended to be
    used to make sure that certain side effects take place after the evaluation
    of protected-form.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates UnwindProtectSpecialOperator.
        """
        cls.__name__ = 'UNWIND-PROTECT'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of UnwindProtectSpecialOperator.
        """
        return forms  # TODO: To implement the behavior.


# ==============================================================================
# Set functions related on special operators
# ==============================================================================

assign_helper(symbol_name='BLOCK', value=BlockSpecialOperator(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='CATCH', value=CatchSpecialOperator(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='EVAL-WHEN', value=EvalWhenSpecialOperator(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='FLET', value=FletSpecialOperator(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='FUNCTION', value=FunctionSpecialOperator(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='GO', value=GoSpecialOperator(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='IF', value=IfSpecialOperator(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='LABELS', value=LabelsSpecialOperator(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='LET', value=LetSpecialOperator(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='LET*', value=LetAsterSpecialOperator(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='LOAD-TIME-VALUE', value=LoadTimeValueSpecialOperator(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='LOCALLY', value=LocallySpecialOperator(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='MACROLET', value=LoadTimeValueSpecialOperator(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='MULTIPLE-VALUE-CALL', value=MultipleValueCallSpecialOperator(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='MULTIPLE-VALUE-PROG1', value=MultipleValueProg1SpecialOperator(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='PROGN', value=PrognSpecialOperator(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='PROGV', value=ProgvSpecialOperator(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='QUOTE', value=QuoteSpecialOperator(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='RETURN-FROM', value=ReturnFromSpecialOperator(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='SETQ', value=SetqSpecialOperator(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='SYMBOL-MACROLET', value=SymbolMacroletSpecialOperator(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='TAGBODY', value=TagbodySpecialOperator(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='THE', value=TheSpecialOperator(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='THROW', value=ThrowSpecialOperator(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='UNWIND-PROTECT', value=UnwindProtectSpecialOperator(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')

# COMMON-LISP-USER package
use_package_helper(package_name_to_use='COMMON-LISP', package_name='COMMON-LISP-USER')