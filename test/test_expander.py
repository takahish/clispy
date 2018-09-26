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

import unittest
from clispy.symbol import *
from clispy.environment import VariableEnvironment, FunctionEnvironment, MacroEnvironment
from clispy.function import BuiltInFunction
from clispy.evaluator import Evaluator, Procedure
from clispy.expander import Expander


class UnitTestCase(unittest.TestCase):
    def setUp(self):
        # Inits global variable environment.
        self.global_var_env = VariableEnvironment()

        # Inits global function environment.
        self.global_func_env = FunctionEnvironment()
        self.global_func_env.update(BuiltInFunction())

        # Make instance of Evaluator.
        self.evaluator = Evaluator(self.global_var_env, self.global_func_env)

        # Inits global macro environment.
        self.global_macro_env = MacroEnvironment()

        # Make instance of Expander.
        self.expander = Expander(self.evaluator, self.global_macro_env)

    def test_expand(self):
        expand = lambda x: self.expander.expand(x)

        FUNC = Symbol('FUNC')
        X = Symbol('X')
        Y = Symbol('Y')
        ADD = Symbol('+')
        MUL = Symbol('*')
        MACRO = Symbol('MACRO')

        # constant => unchanged
        self.assertEqual(expand(3), 3)

        # (quote exp)
        self.assertEqual(expand([QUOTE, [1, 2]]), [QUOTE, [1, 2]])

        # (if t c) => (if t c nil)
        self.assertEqual(expand([IF, True, 2]), [IF, True, 2, False])

        # (defun f (args) body) => (defun f (lambda (args) body))
        self.assertEqual(expand([DEFUN, FUNC, [X], [MUL, X, X]]),
                         [DEFUN, FUNC, [LAMBDA, [X], [BLOCK, FUNC, [MUL, X, X]]]])

        # (defmacro v proc) => None; add {v: proc} to macro_table
        expand([DEFMACRO, MACRO, [X, Y],
                [QUASIQUOTE, [ADD, [UNQUOTE, X], [UNQUOTE, Y]]]])
        self.assertTrue(callable(self.global_macro_env[MACRO]))

        # (progn) => NIL
        self.assertEqual(expand([PROGN]), False)

        # (lambda (x) e1 e2) => (lambda (x) (begin e1 e2))
        self.assertEqual(expand([LAMBDA, [X], [MUL, X, X], [ADD, X, X]]),
                         [LAMBDA, [X], [PROGN, [MUL, X, X], [ADD, X, X]]])

        # `x => expand_quasiquote(x)
        self.assertEqual(expand([QUASIQUOTE, 3]), [QUOTE, 3])

        # (m arg...) => macroexpand if m isinstance macro
        self.assertEqual(expand([MACRO, 1, 2]), [ADD, 1, 2])


    ########## Special forms ##########

    def test__quote(self):
        A = Symbol('A')
        B = Symbol('B')

        x = [QUOTE, A]
        exp = self.expander._Expander__quote(x)
        self.assertEqual(exp, x)

        x = [QUOTE, A, B]
        self.assertRaisesRegex(SyntaxError, "wrong length", self.expander._Expander__quote, x)

    def test__if(self):
        A = Symbol('A')
        B = Symbol('B')

        x = [IF, True, A, B]
        exp = self.expander._Expander__if(x, self.global_macro_env)
        self.assertEqual(exp, [IF, True, A, B])

        x = [IF, True, A]
        exp = self.expander._Expander__if(x, self.global_macro_env)
        self.assertEqual(exp, [IF, True, A, False])

        x = [IF, True]
        self.assertRaisesRegex(SyntaxError, "wrong length", self.expander._Expander__if, x, self.global_macro_env)

        x = [IF, True, A, B, False]
        self.assertRaisesRegex(SyntaxError, "wrong length", self.expander._Expander__if, x, self.global_macro_env)

    def test__setq(self):
        A = Symbol('A')
        B = Symbol('B')
        ADD = Symbol('+')

        x = [SETQ]
        exp = self.expander._Expander__setq(x)
        self.assertFalse(exp)

        x = [SETQ, A, 10, B]
        exp = self.expander._Expander__setq(x)
        self.assertEqual(exp, [SETQ, A, 10, B, False])

        x = [SETQ, [ADD, A, B], 10]
        self.assertRaisesRegex(SyntaxError, "can set! only a symbol", self.expander._Expander__setq, x)

    def test__progn(self):
        x = [PROGN]
        exp = self.expander._Expander__progn(x, self.global_macro_env)
        self.assertFalse(exp)

        x = [PROGN, [SETQ]]
        exp = self.expander._Expander__progn(x, self.global_macro_env)
        self.assertEqual(exp, [PROGN, False])

    def test__function(self):
        X = Symbol('X')
        MUL = Symbol('*')

        x = [FUNCTION, MUL]
        exp = self.expander._Expander__function(x, self.global_macro_env)
        self.assertEqual(exp, [FUNCTION, MUL])

        x = [FUNCTION, [LAMBDA, [X], [MUL, X, X]]]
        exp = self.expander._Expander__function(x, self.global_macro_env)
        self.assertEqual(exp, [FUNCTION, [LAMBDA, [X], [MUL, X, X]]])

        x = [FUNCTION, [MUL, X, X]]
        self.assertRaisesRegex(SyntaxError, "an argument must be symbol", self.expander._Expander__function, x, self.global_macro_env)

        x = [FUNCTION, 10]
        self.assertRaisesRegex(SyntaxError, "an argument must be symbol", self.expander._Expander__function, x, self.global_macro_env)

    def test__flet(self):
        FUNC = Symbol('FUNC')
        X = Symbol('X')
        MUL = Symbol('*')

        x = [FLET, [FUNC], [FUNC, 3]]
        self.assertRaisesRegex(SyntaxError, "wrong length", self.expander._Expander__flet, x, self.global_macro_env)

        x = [FLET, [[FUNC, [X]]], [FUNC, 3]]
        self.assertRaisesRegex(SyntaxError, "wrong length", self.expander._Expander__flet, x, self.global_macro_env)

        x = [FLET, [["FUNC", [X], [MUL, X, X]]], [FUNC, 3]]
        self.assertRaisesRegex(SyntaxError, "illegal function name", self.expander._Expander__flet, x, self.global_macro_env)

        x = [FLET, [[FUNC, False, False]], [FUNC]]
        x = self.expander._Expander__flet(x, self.global_macro_env)
        self.assertEqual(x, [FLET, [[FUNC, [], False]], [FUNC]])

    def test__lables(self):
        FUNC = Symbol('FUNC')
        X = Symbol('X')
        MUL = Symbol('*')

        x = [LABELS, [FUNC], [FUNC, 3]]
        self.assertRaisesRegex(SyntaxError, "wrong length", self.expander._Expander__labels, x, self.global_macro_env)

        x = [LABELS, [[FUNC, [X]]], [FUNC, 3]]
        self.assertRaisesRegex(SyntaxError, "wrong length", self.expander._Expander__labels, x, self.global_macro_env)

        x = [LABELS, [["FUNC", [X], [MUL, X, X]]], [FUNC, 3]]
        self.assertRaisesRegex(SyntaxError, "illegal function name", self.expander._Expander__labels, x, self.global_macro_env)

        x = [LABELS, [[FUNC, False, False]], [FUNC]]
        x = self.expander._Expander__labels(x, self.global_macro_env)
        self.assertEqual(x, [LABELS, [[FUNC, [], False]], [FUNC]])

    def test__macrolet(self):
        MACRO = Symbol('MACRO')
        X = Symbol('X')
        Y = Symbol('Y')
        ADD = Symbol('ADD')

        x = [MACROLET, [[MACRO, [X, Y], [QUASIQUOTE, [ADD, [UNQUOTE, X], [UNQUOTE, Y]]]]], [MACRO, 10, 20]]
        exp = self.expander._Expander__macrolet(x, self.global_macro_env)

        self.assertEqual(exp, [PROGN, [ADD, 10, 20]])
        self.assertFalse(MACRO in self.global_func_env)

    def test__blocl(self):
        NAME = Symbol('NAME')

        x = [BLOCK]
        self.assertRaisesRegex(SyntaxError, "block name must be symbol", self.expander._Expander__block, x, self.global_macro_env)

        x = [BLOCK, 10]
        self.assertRaisesRegex(SyntaxError, "block name must be symbol", self.expander._Expander__block, x, self.global_macro_env)

        x = [BLOCK, NAME]
        self.assertEqual(self.expander._Expander__block(x, self.global_macro_env), False)

    def test__return_from(self):
        NAME = Symbol('NAME')
        ADD = Symbol('+')

        x = [RETURN_FROM]
        self.assertRaisesRegex(SyntaxError, "block name must be symbol", self.expander._Expander__return_from, x,self.global_macro_env)

        x = [RETURN_FROM, 10]
        self.assertRaisesRegex(SyntaxError, "block name must be symbol", self.expander._Expander__return_from, x, self.global_macro_env)

        x = [RETURN_FROM, NAME, [ADD, 10, 20], [ADD, 30, 40]]
        self.assertRaisesRegex(SyntaxError, "wrong length", self.expander._Expander__return_from, x, self.global_macro_env)

        x = [RETURN_FROM, NAME]
        x = self.expander._Expander__return_from(x, self.global_macro_env)
        self.assertEqual(x, [RETURN_FROM, NAME, False])

    def test__catch(self):
        TAG = Symbol('TAG')

        x = [CATCH]
        self.assertRaisesRegex(SyntaxError, "wrong length", self.expander._Expander__catch, x, self.global_macro_env)

        x = [CATCH, [QUOTE, TAG]]
        self.assertEqual(self.expander._Expander__catch(x, self.global_macro_env), False)

    def test__throw(self):
        TAG = Symbol('TAG')

        x = [THROW]
        self.assertRaisesRegex(SyntaxError, "wrong length", self.expander._Expander__throw, x, self.global_macro_env)

        x = [THROW, [QUOTE, TAG]]
        x = self.expander._Expander__throw(x, self.global_var_env)
        self.assertEqual(x, [THROW, [QUOTE, TAG], False])

        x = [THROW, [QUOTE, TAG], 10, 20, 30]
        self.assertRaisesRegex(SyntaxError, "wrong length", self.expander._Expander__throw, x, self.global_macro_env)


    ########## Helper methods ##########

    def test__quasiquote(self):
        S = Symbol('S')
        ADD = Symbol('+')

        x = [QUASIQUOTE, S]
        self.assertEqual(self.expander._Expander__quasiquote(x), [QUOTE, S])

        x = [QUASIQUOTE, [UNQUOTE, [ADD, 1, 2]]]
        self.assertEqual(self.expander._Expander__quasiquote(x), [ADD, 1, 2])

        x = [QUASIQUOTE, [[UNQUOTE_SPLICING, [1, 2]], 3]]
        self.assertEqual(self.expander._Expander__quasiquote(x),
                         [APPEND, [1, 2], [CONS, [QUOTE, 3], [QUOTE, []]]])

    def test__quasiquote_recur(self):
        S = Symbol('S')
        ADD = Symbol('+')

        x = S
        self.assertEqual(self.expander._Expander__quasiquote_recur(x), [QUOTE, S])

        x = [UNQUOTE, [ADD, 1, 2]]
        self.assertEqual(self.expander._Expander__quasiquote_recur(x), [ADD, 1, 2])

        x = [[UNQUOTE_SPLICING, [1, 2]], 3]
        self.assertEqual(self.expander._Expander__quasiquote_recur(x),
                         [APPEND, [1, 2], [CONS, [QUOTE, 3], [QUOTE, []]]])

    def test__defun(self):
        FUNC = Symbol('FUNC')
        X = Symbol('X')
        MUL = Symbol('*')

        x = [DEFUN, FUNC, [X], [MUL, X, X]]
        val = self.expander._Expander__defun(x, self.global_macro_env)
        self.assertEqual(val, [DEFUN, FUNC, [LAMBDA, [X], [BLOCK, FUNC, [MUL, X, X]]]])

    def test__defmacro(self):
        MACRO = Symbol('MACRO')
        X = Symbol('X')
        Y = Symbol('Y')
        ADD = Symbol('+')

        x = [DEFMACRO, MACRO, [X, Y], [QUASIQUOTE, [ADD, [UNQUOTE, X], [UNQUOTE, Y]]]]
        exp = self.expander._Expander__defmacro(x, self.global_macro_env)
        sym = exp[1] # exp is (quote macro)

        self.assertTrue(sym in self.global_macro_env)
        self.assertIsInstance(self.global_macro_env.find(sym)[sym], Procedure)
        self.assertTrue(callable(self.global_macro_env.find(sym)[sym]))

    def test__lambda(self):
        X = Symbol('X')
        MUL = Symbol('*')
        ADD = Symbol('+')

        x = [LAMBDA, [X], [MUL, X, X]]
        val = self.expander._Expander__lambda(x, self.global_macro_env)
        self.assertEqual(val, [LAMBDA, [X], [MUL, X, X]])

        x = [LAMBDA, [X], [MUL, X, X], [ADD, X, X]]
        val = self.expander._Expander__lambda(x, self.global_macro_env)
        self.assertEqual(val, [LAMBDA, [X], [PROGN, [MUL, X, X], [ADD, X, X]]])

    def test__expand_macro(self):
        MACRO = Symbol('MACRO')
        X = Symbol('X')
        Y = Symbol('Y')
        ADD = Symbol('+')

        x = [DEFMACRO, MACRO, [X, Y], [QUASIQUOTE, [ADD, [UNQUOTE, X], [UNQUOTE, Y]]]]
        self.expander._Expander__defmacro(x, self.global_macro_env)

        x = [MACRO, 10, 20]
        exp = self.expander._Expander__expand_macro(x, self.global_macro_env)
        self.assertEqual(exp, ['+', 10, 20]) # (macro 10 20) => (+ 10 20)

    def test__expand_recur(self):
        MACRO = Symbol('MACRO')
        X = Symbol('X')
        Y = Symbol('Y')
        ADD = Symbol('+')
        MUL = Symbol('*')

        x = [DEFMACRO, MACRO, [X, Y], [QUASIQUOTE, [ADD, [UNQUOTE, X], [UNQUOTE, Y]]]]
        self.expander._Expander__defmacro(x, self.global_macro_env)

        x = [[LAMBDA, [X, Y], [ADD, X, Y], [MUL, X, Y]], [MACRO, 10, 20], 30]
        exp = self.expander._Expander__expand_recur(x, self.global_macro_env)
        self.assertEqual(exp, [[LAMBDA, [X, Y], [PROGN, [ADD, X, Y], [MUL, X, Y]]], [ADD, 10, 20], 30])
