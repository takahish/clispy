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

    def test__expand_quasiquote(self):
        QUOTE = symbol.QUOTE
        UNQUOTE = symbol.UNQUOTE
        UNQUOTE_SPLICING = symbol.UNQUOTE_SPLICING
        CONS = symbol.CONS
        APPEND = symbol.APPEND
        S = symbol.Symbol('S')
        ADD = symbol.Symbol('+')

        self.assertEqual(self.expander._Expander__expand_quasiquote(S), [QUOTE, S])
        self.assertEqual(self.expander._Expander__expand_quasiquote([UNQUOTE, [ADD, 1, 2]]), [ADD, 1, 2])
        self.assertEqual(self.expander._Expander__expand_quasiquote([[UNQUOTE_SPLICING, [1, 2]], 3]),
                         [APPEND, [1, 2], [CONS, [QUOTE, 3], [QUOTE, []]]])

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
