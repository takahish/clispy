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
from clispy import cons
from clispy import env
from clispy import eval as eval_module
from clispy import func


class UnitTestCase(unittest.TestCase):
    def setUp(self):
        # Inits global variable environment.
        self.global_var_env = env.VarEnv()

        # Inits global function einvironment.
        self.global_func_env = env.FuncEnv()
        self.global_func_env.update(func.BuiltInFunction())

        # Make instance of Evaluator.
        self.evaluator = eval_module.Evaluator(self.global_var_env, self.global_func_env)

    def test_eval(self):
        eval_ = lambda x: self.evaluator.eval(x)

        QUOTE = symbol.QUOTE
        SETQ = symbol.SETQ
        IF = symbol.IF
        DEFUN = symbol.DEFUN
        LAMBDA = symbol.LAMBDA
        PROGN = symbol.PROGN
        FUNCTION = symbol.FUNCTION

        # Special forms

        # variable reference
        VAR = symbol.Symbol('VAR')
        self.assertRaisesRegex(LookupError, "VAR", eval_, VAR)

        # (setq var 1)
        eval_([SETQ, VAR, 1])
        self.assertEqual(eval_(VAR), 1)

        # constant literal
        self.assertEqual(eval_(1), 1)

        # (quote exp)
        self.assertEqual(eval_([QUOTE, [1, 2, 3]]), [1, 2, 3])

        # (if test conseq alt)
        GT = symbol.Symbol('>')
        self.assertEqual(eval_([IF, [GT, 2, 1], 3, 4]), 3)
        self.assertEqual(eval_([IF, [GT, 1, 2], 3, 4]), 4)

        # (defun func (lambda (x) (* x x)))
        FUNC = symbol.Symbol('FUNC')
        MUL = symbol.Symbol('*')
        X = symbol.Symbol('X')
        eval_([DEFUN, FUNC, [LAMBDA, [X], [MUL, X, X]]])
        self.assertRaisesRegex(LookupError, "FUNC", eval_, FUNC)
        self.assertIsInstance(self.global_func_env['FUNC'], eval_module.Procedure)

        # (progn exp+)
        A = symbol.Symbol('A')
        B = symbol.Symbol('B')
        ADD = symbol.Symbol('+')
        result = eval_([PROGN,
                       [SETQ, A, 2],
                       [SETQ, B, 3],
                       [ADD, A, B]])
        self.assertEqual(result, 5)

        # (function func)
        self.assertIsInstance(eval_([FUNCTION, FUNC]), eval_module.Procedure)

        # others
        # Procedure defined by defun.
        self.assertEqual(eval_([FUNC, 3]), 9)
        # Built-In function.
        self.assertEqual(eval_([ADD, 2, 3]), 5)


    ########## Special forms ##########

    def test__quote(self):
        QUOTE = symbol.QUOTE
        A = symbol.Symbol('A')

        # List literal is converted to cons cell.
        exp = self.evaluator._Evaluator__quote([QUOTE, [1, 2, 3]])
        self.assertEqual(exp, [1, 2, 3])
        self.assertIsInstance(exp, cons.Cons)

        # Constant literal
        exp = self.evaluator._Evaluator__quote([QUOTE, A])
        self.assertEqual(exp, A)

    def test__if(self):
        IF = symbol.IF

        x = [IF, True, 3, 4]
        self.assertEqual(self.evaluator._Evaluator__if(x, self.global_var_env, self.global_func_env), 3)

        x = [IF, False, 3, 4]
        self.assertEqual(self.evaluator._Evaluator__if(x, self.global_var_env, self.global_func_env), 4)

    def test__setq(self):
        var_env = env.VarEnv([], [], self.global_var_env)

        SETQ = symbol.SETQ
        A = symbol.Symbol('A')
        B = symbol.Symbol('B')
        C = symbol.Symbol('C')

        x = [SETQ, A, 10, B, 20]
        self.evaluator._Evaluator__setq(x, var_env, self.global_func_env)

        self.assertTrue(A in var_env)
        self.assertEqual(self.evaluator._Evaluator__refer(A, var_env), 10)
        self.assertTrue(B in var_env)
        self.assertEqual(self.evaluator._Evaluator__refer(B, var_env), 20)
        self.assertRaisesRegex(LookupError, "C", self.evaluator._Evaluator__refer, C, var_env)

    def test__progn(self):
        var_env = env.VarEnv([], [], self.global_var_env)

        PROGN = symbol.PROGN
        SETQ = symbol.SETQ
        A = symbol.Symbol('A')
        B = symbol.Symbol('B')
        ADD = symbol.Symbol('+')

        x = [PROGN, [SETQ, A, 10], [SETQ, B, 20], [ADD, A, B]]
        exp = self.evaluator._Evaluator__progn(x, var_env, self.global_var_env)

        # Side effects.
        self.assertTrue(A in var_env)
        self.assertEqual(var_env[A], 10)
        self.assertTrue(B in var_env)
        self.assertEqual(var_env[B], 20)

        # Return expression.
        self.assertEqual(exp, [ADD, A, B])

    def test__function(self):
        FUNCTION = symbol.FUNCTION
        LAMBDA = symbol.LAMBDA
        MUL = symbol.Symbol('*')
        X = symbol.Symbol('X')
        ADD = symbol.Symbol('+')
        NO_DEFIED_FUNCTION = symbol.Symbol('NO_DEFIED_FUNCTION')

        x = [FUNCTION, [LAMBDA, [X], [MUL, X, X]]]
        p = self.evaluator._Evaluator__function(x, self.global_var_env, self.global_func_env)
        self.assertIsInstance(p, eval_module.Procedure)

        x = [FUNCTION, ADD]
        f = self.evaluator._Evaluator__function(x, self.global_var_env, self.global_func_env)
        self.assertTrue(callable(f))

        x = [FUNCTION, NO_DEFIED_FUNCTION]
        self.assertRaisesRegex(LookupError, "NO_DEFIED_FUNCTION", self.evaluator._Evaluator__function, x,
                               self.global_var_env, self.global_func_env)

    def test__let(self):
        LET = symbol.LET
        PROGN = symbol.PROGN
        A = symbol.Symbol('A')
        B = symbol.Symbol('B')
        ADD = symbol.Symbol('+')

        # __let is private method in Evaluator.
        x = [LET, [[A, 1], [B, 2]], [ADD, A, B]]
        x, var_env = self.evaluator._Evaluator__let(x, self.global_var_env, self.global_func_env)

        self.assertEqual(x, [PROGN, [ADD, A, B]])
        self.assertIsInstance(var_env, env.VarEnv)

        # The bindings are in parallel.
        self.assertEqual(var_env, {'A': 1, 'B': 2})
        self.assertEqual(var_env.outer, self.global_var_env)

    def test__let_aster(self):
        LET_ASTER = symbol.LET_ASTER
        PROGN = symbol.PROGN
        A = symbol.Symbol('A')
        B = symbol.Symbol('B')
        ADD = symbol.Symbol('+')

        # __let_aster is private method in Evaluator.
        x = [LET_ASTER, [[A, 1], [B, A]], [ADD, A, B]]
        x, var_env = self.evaluator._Evaluator__let_aster(x, self.global_var_env, self.global_func_env)

        self.assertEqual(x, [PROGN, [ADD, A, B]])
        self.assertIsInstance(var_env, env.VarEnv)

        # The bindings are in sequential.
        self.assertEqual(var_env, {'B': 1})
        self.assertEqual(var_env.outer, {'A': 1})
        self.assertEqual(var_env.outer.outer, self.global_var_env)

    def test__flet(self):
        FLET = symbol.FLET
        PROGN = symbol.PROGN
        FUNC = symbol.Symbol('FUNC')
        X = symbol.Symbol('X')
        ADD = symbol.Symbol('+')
        MUL = symbol.Symbol('*')

        # __flet is private method in Evaluator.
        x = [FLET, [[FUNC, [X], [MUL, X, X]]], [ADD, [FUNC, 2], [FUNC, 2]]]
        x, func_env = self.evaluator._Evaluator__flet(x, self.global_var_env, self.global_func_env)

        self.assertEqual(x, [PROGN, [ADD, [FUNC, 2], [FUNC, 2]]])
        self.assertIsInstance(func_env, env.FuncEnv)

        # The scope of the name bindings encompasses only the body.
        self.assertEqual(func_env[FUNC].func_env, self.global_func_env)
        self.assertIsNone(func_env[FUNC].func_env.outer)

    def test__lables(self):
        LABLES = symbol.LABELS
        PROGN = symbol.PROGN
        FUNC = symbol.Symbol('FUNC')
        X = symbol.Symbol('X')
        ADD = symbol.Symbol('+')
        MUL = symbol.Symbol('*')

        # __flet is private method in Evaluator.
        x = [LABLES, [[FUNC, [X], [MUL, X, X]]], [ADD, [FUNC, 2], [FUNC, 2]]]
        x, func_env = self.evaluator._Evaluator__labels(x, self.global_var_env, self.global_func_env)

        self.assertEqual(x, [PROGN, [ADD, [FUNC, 2], [FUNC, 2]]])
        self.assertIsInstance(func_env, env.FuncEnv)

        # The scope of the name bindings encompasses the function definitions
        # themselves as well as the body.
        self.assertEqual(func_env[FUNC].func_env, func_env[FUNC].func_env)
        self.assertEqual(func_env[FUNC].func_env.outer, self.global_func_env)


    ########## Helper methods ##########

    def test__cons(self):
        DOT = symbol.DOT
        A = symbol.Symbol('A')
        B = symbol.Symbol('B')
        C = symbol.Symbol('C')

        # __cons is private method in Evaluator.

        # cons
        self.assertIsInstance(self.evaluator._Evaluator__cons([A, B, C]), cons.Cons)
        self.assertEqual(self.evaluator._Evaluator__cons([A, B, C]), ['A', 'B', 'C'])

        # dotted pair
        self.assertIsInstance(self.evaluator._Evaluator__cons([A, DOT, B]), cons.DottedPair)
        self.assertEqual(self.evaluator._Evaluator__cons([A, DOT, B]), ['A', 'B'])

        # dotted pair with nil
        self.assertIsInstance(self.evaluator._Evaluator__cons([A, DOT, False]), cons.Cons)
        self.assertEqual(self.evaluator._Evaluator__cons([A, DOT, False]), ['A'])

    def test__refer(self):
        A = symbol.Symbol('A')
        B = symbol.Symbol('B')
        C = symbol.Symbol('C')

        # Set local values.
        var_env = env.VarEnv([A, B], [10, 20], self.global_func_env)

        self.assertEqual(self.evaluator._Evaluator__refer(A, var_env), 10)
        self.assertEqual(self.evaluator._Evaluator__refer(B, var_env), 20)
        self.assertRaisesRegex(LookupError, "C", self.evaluator._Evaluator__refer, C, var_env)

    def test__defun(self):
        DEFUN = symbol.DEFUN
        LAMBDA = symbol.LAMBDA
        FUNC = symbol.Symbol('FUNC')
        MUL = symbol.Symbol('*')
        X = symbol.Symbol('X')

        # (defun func (lambda (x) (* x x)))
        x = [DEFUN, FUNC, [LAMBDA, [X], [MUL, X, X]]]
        self.evaluator._Evaluator__defun(x, self.global_var_env, self.global_func_env)

        self.assertTrue(FUNC in self.global_func_env)
        self.assertIsInstance(self.global_func_env[FUNC], eval_module.Procedure)

    def test__lambda(self):
        LAMBDA = symbol.LAMBDA
        MUL = symbol.Symbol('*')
        X = symbol.Symbol('X')

        x = [LAMBDA, [X], [MUL, X, X]]
        f = self.evaluator._Evaluator__lambda(x, self.global_var_env, self.global_func_env)
        self.assertIsInstance(f, eval_module.Procedure)

    def test__execute(self):
        ADD = symbol.Symbol('+')
        LAMBDA = symbol.LAMBDA
        MUL = symbol.Symbol('*')
        X = symbol.Symbol('X')

        x = [ADD, 1, 2, 3]
        val = self.evaluator._Evaluator__execute(x, self.global_var_env, self.global_func_env)
        self.assertEqual(val, 6)

        x = [[LAMBDA, [X], [MUL, X, X]], 10]
        val = self.evaluator._Evaluator__execute(x, self.global_var_env, self.global_func_env)
        self.assertEqual(val, 100)
