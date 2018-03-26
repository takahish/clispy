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
from clispy import symbol
from clispy import env
from clispy import func
from clispy import eval
from clispy import expand


class UnitTestCase(unittest.TestCase):
    def setUp(self):
        # Inits global variable environment.
        self.global_var_env = env.VarEnv()

        # Inits global function environment.
        self.global_func_env = env.FuncEnv()
        self.global_func_env.update(func.BuiltInFunction())

        # Make instance of Evaluator.
        self.evaluator = eval.Evaluator(self.global_var_env, self.global_func_env)

        # Inits global macro environment.
        self.global_macro_env = env.MacroEnv()

        # Make instance of Expander.
        self.expander = expand.Expander(self.evaluator, self.global_macro_env)

    def test_expand(self):
        expand = lambda x: self.expander.expand(x)

        QUOTE = symbol.QUOTE
        IF = symbol.IF
        DEFUN = symbol.DEFUN
        PROGN = symbol.PROGN
        LAMBDA = symbol.LAMBDA
        DEFMACRO = symbol.DEFMACRO
        QUASIQUOTE = symbol.QUASIQUOTE
        UNQUOTE = symbol.UNQUOTE

        FUNC = symbol.Symbol('FUNC')
        X = symbol.Symbol('X')
        Y = symbol.Symbol('Y')
        ADD = symbol.Symbol('+')
        MUL = symbol.Symbol('*')
        MACRO = symbol.Symbol('MACRO')

        # constant => unchanged
        self.assertEqual(expand(3), 3)

        # (quote exp)
        self.assertEqual(expand([QUOTE, [1, 2]]), [QUOTE, [1, 2]])

        # (if t c) => (if t c nil)
        self.assertEqual(expand([IF, True, 2]), [IF, True, 2, False])

        # (defun f (args) body) => (defun f (lambda (args) body))
        self.assertEqual(expand([DEFUN, FUNC, [X], [MUL, X, X]]),
                         [DEFUN, FUNC, [LAMBDA, [X], [MUL, X, X]]])

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
        QUOTE = symbol.QUOTE
        A = symbol.Symbol('A')
        B = symbol.Symbol('B')

        x = [QUOTE, A]
        exp = self.expander._Expander__quote(x)
        self.assertEqual(exp, x)

        x = [QUOTE, A, B]
        self.assertRaisesRegex(SyntaxError, "wrong length", self.expander._Expander__quote, x)

    def test__if(self):
        IF = symbol.IF
        A = symbol.Symbol('A')
        B = symbol.Symbol('B')

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
        SETQ = symbol.SETQ
        A = symbol.Symbol('A')
        B = symbol.Symbol('B')
        ADD = symbol.Symbol('+')

        x = [SETQ]
        exp = self.expander._Expander__setq(x)
        self.assertFalse(exp)

        x = [SETQ, A, 10, B]
        exp = self.expander._Expander__setq(x)
        self.assertEqual(exp, [SETQ, A, 10, B, False])

        x = [SETQ, [ADD, A, B], 10]
        self.assertRaisesRegex(SyntaxError, "can set! only a symbol", self.expander._Expander__setq, x)

    def test__progn(self):
        PROGN = symbol.PROGN
        SETQ = symbol.SETQ

        x = [PROGN]
        exp = self.expander._Expander__progn(x, self.global_macro_env)
        self.assertFalse(exp)

        x = [PROGN, [SETQ]]
        exp = self.expander._Expander__progn(x, self.global_macro_env)
        self.assertEqual(exp, [PROGN, False])

    def test__function(self):
        FUNCTION = symbol.FUNCTION
        FUNC = symbol.Symbol('FUNC')

        x = [FUNCTION, FUNC]
        exp = self.expander._Expander__function(x, self.global_macro_env)
        self.assertEqual(exp, [FUNCTION, FUNC])

        x = [FUNCTION, 10]
        self.assertRaisesRegex(SyntaxError, "an argument must be symbol", self.expander._Expander__function, x, self.global_macro_env)

    def test__macrolet(self):
        MACROLET = symbol.MACROLET
        QUASIQUOTE = symbol.QUASIQUOTE
        UNQUOTE = symbol.UNQUOTE
        PROGN = symbol.PROGN
        MACRO = symbol.Symbol('MACRO')
        X = symbol.Symbol('X')
        Y = symbol.Symbol('Y')
        ADD = symbol.Symbol('ADD')

        x = [MACROLET, [[MACRO, [X, Y], [QUASIQUOTE, [ADD, [UNQUOTE, X], [UNQUOTE, Y]]]]], [MACRO, 10, 20]]
        exp = self.expander._Expander__macrolet(x, self.global_macro_env)

        self.assertEqual(exp, [PROGN, [ADD, 10, 20]])
        self.assertFalse(MACRO in self.global_func_env)

    def test__blocl(self):
        BLOCK = symbol.BLOCK
        NAME = symbol.Symbol('NAME')

        x = [BLOCK]
        self.assertRaisesRegex(SyntaxError, "block name must be symbol", self.expander._Expander__block, x, self.global_macro_env)

        x = [BLOCK, 10]
        self.assertRaisesRegex(SyntaxError, "block name must be symbol", self.expander._Expander__block, x, self.global_macro_env)

        x = [BLOCK, NAME]
        self.assertEqual(self.expander._Expander__block(x, self.global_macro_env), False)

    def test__return_from(self):
        RETURN_FROM = symbol.RETURN_FROM
        NAME = symbol.Symbol('NAME')
        ADD = symbol.Symbol('+')

        x = [RETURN_FROM]
        self.assertRaisesRegex(SyntaxError, "block name must be symbol", self.expander._Expander__return_from, x,self.global_macro_env)

        x = [RETURN_FROM, 10]
        self.assertRaisesRegex(SyntaxError, "block name must be symbol", self.expander._Expander__return_from, x, self.global_macro_env)

        x = [RETURN_FROM, NAME, [ADD, 10, 20], [ADD, 30, 40]]
        self.assertRaisesRegex(SyntaxError, "wrong length", self.expander._Expander__return_from, x, self.global_macro_env)

        x = [RETURN_FROM, NAME]
        x = self.expander._Expander__return_from(x, self.global_macro_env)
        self.assertEqual(x, [RETURN_FROM, NAME, False])


    ########## Helper methods ##########

    def test__quasiquote(self):
        QUASIQUOTE = symbol.QUASIQUOTE
        QUOTE = symbol.QUOTE
        UNQUOTE = symbol.UNQUOTE
        UNQUOTE_SPLICING = symbol.UNQUOTE_SPLICING
        CONS = symbol.CONS
        APPEND = symbol.APPEND
        S = symbol.Symbol('S')
        ADD = symbol.Symbol('+')

        x = [QUASIQUOTE, S]
        self.assertEqual(self.expander._Expander__quasiquote(x), [QUOTE, S])

        x = [QUASIQUOTE, [UNQUOTE, [ADD, 1, 2]]]
        self.assertEqual(self.expander._Expander__quasiquote(x), [ADD, 1, 2])

        x = [QUASIQUOTE, [[UNQUOTE_SPLICING, [1, 2]], 3]]
        self.assertEqual(self.expander._Expander__quasiquote(x),
                         [APPEND, [1, 2], [CONS, [QUOTE, 3], [QUOTE, []]]])

    def test__quasiquote_recur(self):
        QUOTE = symbol.QUOTE
        UNQUOTE = symbol.UNQUOTE
        UNQUOTE_SPLICING = symbol.UNQUOTE_SPLICING
        CONS = symbol.CONS
        APPEND = symbol.APPEND
        S = symbol.Symbol('S')
        ADD = symbol.Symbol('+')

        x = S
        self.assertEqual(self.expander._Expander__quasiquote_recur(x), [QUOTE, S])

        x = [UNQUOTE, [ADD, 1, 2]]
        self.assertEqual(self.expander._Expander__quasiquote_recur(x), [ADD, 1, 2])

        x = [[UNQUOTE_SPLICING, [1, 2]], 3]
        self.assertEqual(self.expander._Expander__quasiquote_recur(x),
                         [APPEND, [1, 2], [CONS, [QUOTE, 3], [QUOTE, []]]])

    def test__defun(self):
        DEFUN = symbol.DEFUN
        LAMBDA = symbol.LAMBDA
        FUNC = symbol.Symbol('FUNC')
        X = symbol.Symbol('X')
        MUL = symbol.Symbol('*')

        x = [DEFUN, FUNC, [X], [MUL, X, X]]
        val = self.expander._Expander__defun(x, self.global_macro_env)
        self.assertEqual(val, [DEFUN, FUNC, [LAMBDA, [X], [MUL, X, X]]])

    def test__defmacro(self):
        QUASIQUOTE = symbol.QUASIQUOTE
        UNQUOTE = symbol.UNQUOTE
        DEFMACRO = symbol.DEFMACRO
        MACRO = symbol.Symbol('MACRO')
        X = symbol.Symbol('X')
        Y = symbol.Symbol('Y')
        ADD = symbol.Symbol('+')

        x = [DEFMACRO, MACRO, [X, Y], [QUASIQUOTE, [ADD, [UNQUOTE, X], [UNQUOTE, Y]]]]
        exp = self.expander._Expander__defmacro(x, self.global_macro_env)
        sym = exp[1] # exp is (quote macro)

        self.assertTrue(sym in self.global_macro_env)
        self.assertIsInstance(self.global_macro_env.find(sym)[sym], eval.Procedure)
        self.assertTrue(callable(self.global_macro_env.find(sym)[sym]))

    def test__lambda(self):
        LAMBDA = symbol.LAMBDA
        X = symbol.Symbol('X')
        MUL = symbol.Symbol('*')
        ADD = symbol.Symbol('+')
        PROGN = symbol.PROGN

        x = [LAMBDA, [X], [MUL, X, X]]
        val = self.expander._Expander__lambda(x, self.global_macro_env)
        self.assertEqual(val, [LAMBDA, [X], [MUL, X, X]])

        x = [LAMBDA, [X], [MUL, X, X], [ADD, X, X]]
        val = self.expander._Expander__lambda(x, self.global_macro_env)
        self.assertEqual(val, [LAMBDA, [X], [PROGN, [MUL, X, X], [ADD, X, X]]])

    def test__expand_macro(self):
        QUASIQUOTE = symbol.QUASIQUOTE
        UNQUOTE = symbol.UNQUOTE
        DEFMACRO = symbol.DEFMACRO
        MACRO = symbol.Symbol('MACRO')
        X = symbol.Symbol('X')
        Y = symbol.Symbol('Y')
        ADD = symbol.Symbol('+')

        x = [DEFMACRO, MACRO, [X, Y], [QUASIQUOTE, [ADD, [UNQUOTE, X], [UNQUOTE, Y]]]]
        self.expander._Expander__defmacro(x, self.global_macro_env)

        x = [MACRO, 10, 20]
        exp = self.expander._Expander__expand_macro(x, self.global_macro_env)
        self.assertEqual(exp, ['+', 10, 20]) # (macro 10 20) => (+ 10 20)

    def test__expand_recur(self):
        QUASIQUOTE = symbol.QUASIQUOTE
        UNQUOTE = symbol.UNQUOTE
        DEFMACRO = symbol.DEFMACRO
        LAMBDA = symbol.LAMBDA
        PROGN = symbol.PROGN
        MACRO = symbol.Symbol('MACRO')
        X = symbol.Symbol('X')
        Y = symbol.Symbol('Y')
        ADD = symbol.Symbol('+')
        MUL = symbol.Symbol('*')

        x = [DEFMACRO, MACRO, [X, Y], [QUASIQUOTE, [ADD, [UNQUOTE, X], [UNQUOTE, Y]]]]
        self.expander._Expander__defmacro(x, self.global_macro_env)

        x = [[LAMBDA, [X, Y], [ADD, X, Y], [MUL, X, Y]], [MACRO, 10, 20], 30]
        exp = self.expander._Expander__expand_recur(x, self.global_macro_env)
        self.assertEqual(exp, [[LAMBDA, [X, Y], [PROGN, [ADD, X, Y], [MUL, X, Y]]], [ADD, 10, 20], 30])
