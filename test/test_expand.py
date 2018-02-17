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
from clispy import symbol, env, expand


class UnitTestCase(unittest.TestCase):
    def test_expand_quasiquote(self):
        _symbol = symbol._Symbol('symbol')
        _quote = symbol._quote
        _unquote = symbol._unquote
        _unquote_splicing = symbol._unquote_splicing
        _cons = symbol._cons
        _append = symbol._append
        _add = symbol._Symbol('+')

        self.assertEqual(expand._expand_quasiquote(_symbol), [symbol._quote, _symbol])
        self.assertEqual(expand._expand_quasiquote([_unquote, [_add, 1, 2]]), [_add, 1, 2])
        self.assertEqual(expand._expand_quasiquote([[_unquote_splicing, [1, 2]], 3]),
                         [_append, [1, 2], [_cons, [_quote, 3], [_quote, []]]])

    def test_replace_expression(self):
        self.assertEqual(expand._replace_expression(['func', 1, 2], 'func', 'add'), ['add', 1, 2])
        self.assertEqual(expand._replace_expression([['func', 1, 2], 3], 'func', 'add'),
                         [['add', 1, 2], 3])
        self.assertEqual(expand._replace_expression(['add', ['func', 1, 2], 3], 'func', 'sub'),
                         ['add', ['sub', 1, 2], 3])

    def test_expand(self):
        _quote = symbol._quote
        _if = symbol._if
        _defun = symbol._defun
        _progn = symbol._progn
        _lambda = symbol._lambda
        _defmacro = symbol._defmacro
        _quasiquote = symbol._quasiquote
        _unquote = symbol._unquote
        _func = symbol._Symbol('func')
        _x = symbol._Symbol('x')
        _y = symbol._Symbol('y')
        _add = symbol._Symbol('+')
        _mul = symbol._Symbol('*')
        _test = symbol._Symbol('test')

        # constant => unchanged
        self.assertEqual(expand._expand(3), 3)

        # (quote exp)
        self.assertEqual(expand._expand([_quote, [1, 2]]), [_quote, [1, 2]])

        # (if t c) => (if t c False)
        self.assertEqual(expand._expand([_if, True, 2]), [_if, True, 2, False])

        # (defun f (args) body) => (defun f (lambda (args) body))
        self.assertEqual(expand._expand([_defun, _func, [_x], [_mul, _x, _x]]),
                         [_defun, _func, [_lambda, [_x], [_mul, _x, _x]]])

        # (defmacro v proc) => None; add {v: proc} to macro_table
        expand._expand([_defmacro, _test, [_x, _y],
                        [_quasiquote, [_add, [_unquote, _x], [_unquote, _y]]]])
        self.assertTrue(callable(env.macro_env[_test]))

        # (progn) => NIL
        self.assertEqual(expand._expand([_progn]), False)

        # (lambda (x) e1 e2) => (lambda (x) (begin e1 e2))
        self.assertEqual(expand._expand([_lambda, [_x], [_mul, _x, _x], [_add, _x, _x]]),
                         [_lambda, [_x], [_progn, [_mul, _x, _x], [_add, _x, _x]]])

        # `x => expand_quasiquote(x)
        self.assertEqual(expand._expand([_quasiquote, 3]), [_quote, 3])

        # (m arg...) => macroexpand if m isinstance macro
        self.assertEqual(expand._expand([_test, 1, 2]), [_add, 1, 2])

    def test_let(self):
        _let = symbol._let
        _lambda = symbol._lambda
        _x = symbol._Symbol('x')
        _y = symbol._Symbol('y')
        _mul = symbol._Symbol('*')

        # (let ((var val)) body) => ((lambda (var) body) val)
        self.assertEqual(expand._expand([_let, [[_x, 10], [_y, 20]], [_mul, _x, _y]]),
                         [[_lambda, [_x, _y], [_mul, _x, _y]], 10, 20])

    def test_flet(self):
        _flet = symbol._flet
        _progn = symbol._progn
        _lambda = symbol._lambda
        _func = symbol._Symbol('func')
        _x = symbol._Symbol('x')
        _mul = symbol._Symbol('*')

        # (flet ((func var exp)) body) => (progn body_replaced_func_to_lambda)
        self.assertEqual(expand._expand([_flet, [[_func, [_x], [_mul, _x, _x]]], [_func, 3]]),
                         [_progn, [[_lambda, [_x], [_mul, _x, _x]], 3]])
