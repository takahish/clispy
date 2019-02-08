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

import copy
from clispy.function_ import Function, Lambda
from clispy.package import Environment, assign_helper
from clispy.type import BuiltInClass, Symbol, Null, Cons, String


# ==============================================================================
# Defines base classes.
#
#     SpecialOperator
# ==============================================================================

class SpecialOperator(Function):
    """A special forms is a form with special syntax, special evaluation rules
    or both, possibley manipulating the evaluation environment, control flow,
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


# ==============================================================================
# Defines special operator classes.
#
#     Block       Let*                ReturnFrom
#     Catch       LoadIimeValue       Setq
#     EvalWhen    Locally             SymbolMcrolet
#     Flet        Macrolet            Tagbody
#     Function_   MultipleValueCall   The
#     Go          MultipleValueProg1  Throw
#     If          Progn               UnwindProtect
#     Labels      Progv
#     Let         Quote
# ==============================================================================


class Block(SpecialOperator):
    """block establishes a block and then evaluates forms as an implicit progn.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates Block.
        """
        cls.__name__ = 'BLOCK'
        return object.__new__(cls)

    def __call__(self, forms, package_manager):
        """Behavior of Block.
        """
        return forms  # TODO: To implement the behavior.


class Catch(SpecialOperator):
    """catch is used as the destination of a non-local control transfer by throw.
    Tags are used to find the catch to which a throw is transferring control.
    (catch 'foo forms) catches a (throw 'foo forms) but not a (throw 'bar forms).
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates Catch.
        """
        cls.__name__ = 'CATCH'
        return object.__new__(cls)

    def __call__(self, forms, package_manager):
        """Behavior of Catch.
        """
        return forms  # TODO: To implement the behavior.


class EvalWhen(SpecialOperator):
    """The body of an eval-when forms is processed as an implicit progn, but only
    in the situations listed.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates EvalWhen.
        """
        cls.__name__ = 'EVAL-WHEN'
        return object.__new__(cls)

    def __call__(self, forms, package_manager):
        """Behavior of EvalWhen.
        """
        return forms  # TODO: To implement the behavior.


class Flet(SpecialOperator):
    """flet, labels, and macrolet define local functions and macros, and execute
    forms using the local definitions. forms are executed in order of occurence.

    The body forms (but not the lambda list) of each function created by flet
    and labels and each macro created by macrolet are enclosed in an implicit
    block whose name is the function block name of the function-name or name,
    as appropriate.

    flet defines locally named functions and executes a series of forms with
    these definition bindings. Any number of such local functions can be defined.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates Flet.
        """
        cls.__name__ = 'FLET'
        return object.__new__(cls)

    def __call__(self, forms, package_manager):
        """Behavior of Flet.
        """
        from clispy.evaluator_ import Evaluator

        # Deepcopy current scope to escape from local scope later.
        lexical_package_manager = copy.deepcopy(package_manager)

        bindings, body = forms.car, forms.cdr

        # The body of a let* is an implicit progn.
        body = Cons(Symbol('PROGN'), body)

        funcs = []
        exps = []
        while bindings is not Null():
            func, exp = bindings.car.car, bindings.car.cdr

            lexical_package_manager.intern(String(func.value))

            funcs.append(func.value)
            exp = Cons(Symbol('LAMBDA'), exp)

            # The scope of the name bindings dose not encompasse the function definitions.
            exps.append(Evaluator.eval(exp, package_manager))

            bindings = bindings.cdr

        # The bindings are in parallel.
        local_scope = lexical_package_manager.current_package.space['FUNCTION'].extend(params=funcs, args=exps)
        lexical_package_manager.current_package.space['FUNCTION'] = local_scope

        return Evaluator.eval(body, lexical_package_manager)


class Function_(SpecialOperator):
    """The value of function is the functional value of name in the current
    lexical environment.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates Function_.
        """
        cls.__name__ = 'FUNCTION'
        return object.__new__(cls)

    def __call__(self, forms, package_manager):
        """Behavior of Function_.
        """
        if isinstance(forms.car, Cons) and forms.car.car is Symbol('LAMBDA'):
            return Lambda(forms.car.cdr, package_manager)
        else:
            return package_manager.find(forms.car, env='FUNCTION')[forms.car.value]


class Go(SpecialOperator):
    """go transfers control to the point in the body of an enclosing tagbody
    forms labeled by a tag eql to tag.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates Go.
        """
        cls.__name__ = 'GO'
        return object.__new__(cls)

    def __call__(self, forms, package_manager):
        """Behavior of Go.
        """
        return forms  # TODO: To implement the behavior.


class If(SpecialOperator):
    """if allows the execution of a form to be dependent on a single test-form.

    First test-form is evaluated. If the result is true, then then-form is selected;
    otherwise else-form is selected. Whichever form is selected is then evaluated.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates If.
        """
        cls.__name__ = 'IF'
        return object.__new__(cls)

    def __call__(self, forms, package_manager):
        """Behavior of If.
        """
        from clispy.evaluator_ import Evaluator

        # Sets test-forms, then-forms and else-forms.
        test_forms, then_forms, else_forms = forms.car, forms.cdr.car, forms.cdr.cdr.car

        # If-then-else
        if Evaluator.eval(test_forms, package_manager) is Null():
            return Evaluator.eval(else_forms, package_manager)
        else:
            return Evaluator.eval(then_forms, package_manager)


class Labels(SpecialOperator):
    """flet, labels, and macrolet define local functions and macros, and execute
    forms using the local definitions. forms are executed in order of occurence.

    The body forms (but not the lambda list) of each function created by flet
    and labels and each macro created by macrolet are enclosed in an implicit
    block whose name is the function block name of the function-name or name,
    as appropriate.

    labels is equivalent to flet except that the scope of the defined function
    names for labels encompasses the function definitions themselves as well
    as the body.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates Labels.
        """
        cls.__name__ = 'LABELS'
        return object.__new__(cls)

    def __call__(self, forms, package_manager):
        """Behavior of Flet.
        """
        from clispy.evaluator_ import Evaluator

        # Deepcopy current scope to escape from local scope later.
        lexical_package_manager = copy.deepcopy(package_manager)

        bindings, body = forms.car, forms.cdr

        # The body of a let* is an implicit progn.
        body = Cons(Symbol('PROGN'), body)

        funcs = []
        exps = []
        while bindings is not Null():
            func, exp = bindings.car.car, bindings.car.cdr

            lexical_package_manager.intern(String(func.value))

            funcs.append(func.value)

            exp = Cons(Symbol('LAMBDA'), exp)

            # The scope of the name bindings encompasses the function definitions
            # themselves as well as the body.
            exps.append(Evaluator.eval(exp, lexical_package_manager))

            bindings = bindings.cdr

        # The bindings are in parallel.
        local_scope = lexical_package_manager.current_package.space['FUNCTION'].extend(params=funcs, args=exps)
        lexical_package_manager.current_package.space['FUNCTION'] = local_scope

        return Evaluator.eval(body, lexical_package_manager)


class Let(SpecialOperator):
    """let and let* create new variable bindings and execute a series of forms
    that use these bindings. let performs the bindings in parallel and let* does
    them sequentially.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates Let.
        """
        cls.__name__ = 'LET'
        return object.__new__(cls)

    def __call__(self, forms, package_manager):
        """Behavior of Let.
        """
        from clispy.evaluator_ import Evaluator

        # Deepcopy current scope to escape from local scope later.
        lexical_package_manager = copy.deepcopy(package_manager)

        bindings, body = forms.car, forms.cdr

        # The body of a let* is an implicit progn.
        body = Cons(Symbol('PROGN'), body)

        vars, vals = [], []
        while bindings is not Null():
            var, val = bindings.car.car, bindings.car.cdr.car

            lexical_package_manager.intern(String(var.value))

            vars.append(var.value)
            vals.append(Evaluator.eval(val, lexical_package_manager))

            bindings = bindings.cdr

        # The bindings are in parallel.
        local_scope = lexical_package_manager.current_package.space['VARIABLE'].extend(params=vars, args=vals)
        lexical_package_manager.current_package.space['VARIABLE'] = local_scope

        return Evaluator.eval(body, lexical_package_manager)


class LetAster(SpecialOperator):
    """let and let* create new variable bindings and execute a series of forms
    that use these bindings. let performs the bindings in parallel and let* does
    them sequentially.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates LetAster.
        """
        cls.__name__ = 'LET*'
        return object.__new__(cls)

    def __call__(self, forms, package_manager):
        """Behavior of LetAster.
        """
        from clispy.evaluator_ import Evaluator

        # Deepcopy current scope to escape from local scope later.
        lexical_package_manager = copy.deepcopy(package_manager)

        bindings, body = forms.car, forms.cdr

        # The body of a let* is an implicit progn.
        body = Cons(Symbol('PROGN'), body)

        while bindings is not Null():
            var, val = bindings.car.car, bindings.car.cdr.car

            lexical_package_manager.intern(String(var.value))

            var = var.value
            val = Evaluator.eval(val, lexical_package_manager)

            # The bindings are in sequence.
            nested_scope = lexical_package_manager.current_package.space['VARIABLE'].extend(params=[var], args=[val])
            lexical_package_manager.current_package.space['VARIABLE'] = nested_scope

            bindings = bindings.cdr

        return Evaluator.eval(body, lexical_package_manager)


class LoadTimeValue(SpecialOperator):
    """Load-time-value provides a mechanism for delaying evaluation of forms until
    the expression is in the run-time environment
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates LoadTimeValue.
        """
        cls.__name__ = 'LOAD-TIME-VALUE'
        return object.__new__(cls)

    def __call__(self, forms, package_manager):
        """Behavior of LoadTimeValue.
        """
        return forms  # TODO: To implement the behavior.


class Locally(SpecialOperator):
    """Sequentially evaluates a body of formss in a lexical environment where the
    given declarations have effect.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates Locally.
        """
        cls.__name__ = 'LOCALLY'
        return object.__new__(cls)

    def __call__(self, forms, package_manager):
        """Behavior of Locally.
        """
        return forms  # TODO: To implement the behavior.


class Macrolet(SpecialOperator):
    """flet, labels, and macrolet define local functions and macros, and execute
    forms using the local definitions. forms are executed in order of occurence.

    The body forms (but not the lambda list) of each function created by flet
    and labels and each macro created by macrolet are enclosed in an implicit
    block whose name is the function block name of the function-name or name,
    as appropriate.

    macrolet establishes local macro definitions, using the same format used
    by defmacro.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates Macrolet.
        """
        cls.__name__ = 'MACROLET'
        return object.__new__(cls)

    def __call__(self, forms, package_manager):
        """Behavior of Macrolet.
        """
        return forms  # TODO: To implement the behavior.


class MultipleValueCall(SpecialOperator):
    """Applies function to a list of the objects collected from groups of
    multiple values
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates MultipleValueCall.
        """
        cls.__name__ = 'MULTIPLE-VALUE-CALL'
        return object.__new__(cls)

    def __call__(self, forms, package_manager):
        """Behavior of MutipleValueCall.
        """
        return forms  # TODO: To implement the behavior.


class MultipleValueProg1(SpecialOperator):
    """multiple-value-prog1 evaluates first-form and saves all the values
    produced by that form. It then evaluates each form from left to right,
    discarding their values.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates MultipleValueProg1.
        """
        cls.__name__ = 'MULTIPLE-VALUE-PROG1'
        return object.__new__(cls)

    def __call__(self, forms, package_manager):
        """Behavior of MultipleValueProg1.
        """
        return forms  # TODO: To implement the behavior.


class Progn(SpecialOperator):
    """progn evaluates forms, in the order in which they are given.
    The values of each form but the last are discarded.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates Progn.
        """
        cls.__name__ = 'PROGN'
        return object.__new__(cls)

    def __call__(self, forms, package_manager):
        """Behavior of Progn.
        """
        from clispy.evaluator_ import Evaluator

        # The values of eatch form but the last are discarded.
        while forms is not Null():
            last = Evaluator.eval(forms.car, package_manager)
            forms = forms.cdr

        return last


class Progv(SpecialOperator):
    """progv creates new dynamic variable bindings and executes each form
    using those bindings. Each form is evaluated in order.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates Progv.
        """
        cls.__name__ = 'PROGV'
        return object.__new__(cls)

    def __call__(self, forms, package_manager):
        """Behavior of Progv.
        """
        return forms  # TODO: To implement the behavior.


class Quote(SpecialOperator):
    """The quote special operator just returns object.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates Quote.
        """
        cls.__name__ = 'QUOTE'
        return object.__new__(cls)

    def __call__(self, forms, package_manager):
        """Behavior of Quote.
        """
        return forms.car


class ReturnFrom(SpecialOperator):
    """Returns control and multiple values[2] from a lexically enclosing block.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates ReturnFrom.
        """
        cls.__name__ = 'RETURN-FROM'
        return object.__new__(cls)

    def __call__(self, forms, package_manager):
        """Behavior of ReturnFrom.
        """
        return forms  # TODO: To implement the behavior.


class Setq(SpecialOperator):
    """Assigns values to variables.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates Setq.
        """
        cls.__name__ = 'SETQ'
        return object.__new__(cls)

    def __call__(self, forms, package_manager):
        """Behavior of Setq.
        """
        from clispy.evaluator_ import Evaluator

        while forms is not Null():
            symbol, value, forms = forms.car, forms.cdr.car, forms.cdr.cdr

            symbol_name = symbol.value
            value = Evaluator.eval(value, package_manager)

            package_manager.intern(String(symbol_name))
            try:
                package_manager.find(symbol, env='VARIABLE')[symbol_name] = value
            except LookupError:
                package_manager.current_package.space['VARIABLE'][symbol_name] = value

        # the primary value of the last form
        return value


class SymbolMacrolet(SpecialOperator):
    """symbol-macrolet provides a mechanism for affecting the macro expansion
    environment for symbols.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates SymbolMacrolet.
        """
        cls.__name__ = 'SYMBOL-MACROLET'
        return object.__new__(cls)

    def __call__(self, forms, package_manager):
        """Behavior of SymbolMacrolet.
        """
        return forms  # TODO: To implement the behavior.


class Tagbody(SpecialOperator):
    """Executes zero or more statements in a lexical environment that provides
    for control transfers to labels indicated by the tags.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates Tagbody.
        """
        cls.__name__ = 'TAGBODY'
        return object.__new__(cls)

    def __call__(self, forms, package_manager):
        """Behavior of Tagbody.
        """
        return forms  # TODO: To implement the behavior.


class The(SpecialOperator):
    """the specifies that the values[1a] returned by form are of the types
    specified by value-type. The consequences are undefined if any result is
    not of the declared type.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates The.
        """
        cls.__name__ = 'THE'
        return object.__new__(cls)

    def __call__(self, forms, package_manager):
        """Behavior of The.
        """
        return forms  # TODO: To implement the behavior.


class Throw(SpecialOperator):
    """throw causes a non-local control transfer to a catch whose tag is eq to tag.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates Throw.
        """
        cls.__name__ = 'THROW'
        return object.__new__(cls)

    def __call__(self, forms, package_manager):
        """Behavior of Throw.
        """
        return forms  # TODO: To implement the behavior.


class UnwindProtect(SpecialOperator):
    """unwind-protect evaluates protected-form and guarantees that cleanup-forms are
    executed before unwind-protect exits, whether it terminates normally or is
    aborted by a control transfer of some kind. unwind-protect is intended to be
    used to make sure that certain side effects take place after the evaluation
    of protected-form.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates UnwindProtect.
        """
        cls.__name__ = 'UNWIND-PROTECT'
        return object.__new__(cls)

    def __call__(self, forms, package_manager):
        """Behavior of UnwindProtect.
        """
        return forms  # TODO: To implement the behavior.


# ==============================================================================
# Set functions related on special operators
# ==============================================================================

assign_helper(symbol_name='BLOCK', value=Block(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='CATCH', value=Catch(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='EVAL-WHEN', value=EvalWhen(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='FLET', value=Flet(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='FUNCTION', value=Function_(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='GO', value=Go(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='IF', value=If(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='LABELS', value=Labels(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='LET', value=Let(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='LET*', value=LetAster(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='LOAD-TIME-VALUE', value=LoadTimeValue(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='LOCALLY', value=Locally(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='MACROLET', value=LoadTimeValue(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='MULTIPLE-VALUE-CALL', value=MultipleValueCall(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='MULTIPLE-VALUE-PROG1', value=MultipleValueProg1(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='PROGN', value=Progn(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='PROGV', value=Progv(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='QUOTE', value=Quote(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='RETURN-FROM', value=ReturnFrom(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='SETQ', value=Setq(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='SYMBOL-MACROLET', value=SymbolMacrolet(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='TAGBODY', value=Tagbody(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='THE', value=The(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='THROW', value=Throw(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='UNWIND-PROTECT', value=UnwindProtect(), package_name='COMMON-LISP', env='FUNCTION', status=':EXTERNAL')
