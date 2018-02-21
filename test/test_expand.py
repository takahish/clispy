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
        self._eval = eval.closure(symbol, env, func)
        self._expand = expand.closure(symbol, env, self._eval)

    def test_expand_quasiquote(self):
        _symbol = symbol.Symbol('symbol')
        _quote = symbol.QUOTE
        _unquote = symbol.UNQUOTE
        _unquote_splicing = symbol.UNQUOTE_SPLICING
        _cons = symbol.CONS
        _append = symbol.APPEND
        _add = symbol.Symbol('+')

        self.assertEqual(expand._expand_quasiquote(_symbol), [symbol.QUOTE, _symbol])
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
        _expand = self._expand

        _quote = symbol.QUOTE
        _if = symbol.IF
        _defun = symbol.DEFUN
        _progn = symbol.PROGN
        _lambda = symbol.LAMBDA
        _defmacro = symbol.DEFMACRO
        _quasiquote = symbol.QUASIQUOTE
        _unquote = symbol.UNQUOTE
        _let = symbol.LET
        _flet = symbol.FLET
        _func = symbol.Symbol('func')
        _x = symbol.Symbol('x')
        _y = symbol.Symbol('y')
        _add = symbol.Symbol('+')
        _mul = symbol.Symbol('*')
        _test = symbol.Symbol('test')

        # constant => unchanged
        self.assertEqual(_expand(3), 3)

        # (quote exp)
        self.assertEqual(_expand([_quote, [1, 2]]), [_quote, [1, 2]])

        # (if t c) => (if t c False)
        self.assertEqual(_expand([_if, True, 2]), [_if, True, 2, False])

        # (defun f (args) body) => (defun f (lambda (args) body))
        self.assertEqual(_expand([_defun, _func, [_x], [_mul, _x, _x]]),
                         [_defun, _func, [_lambda, [_x], [_mul, _x, _x]]])

        # (defmacro v proc) => None; add {v: proc} to macro_table
        _expand([_defmacro, _test, [_x, _y],
                 [_quasiquote, [_add, [_unquote, _x], [_unquote, _y]]]])
        #self.assertTrue(callable(env.macro_env[_test]))

        # (progn) => NIL
        self.assertEqual(_expand([_progn]), False)

        # (lambda (x) e1 e2) => (lambda (x) (begin e1 e2))
        self.assertEqual(_expand([_lambda, [_x], [_mul, _x, _x], [_add, _x, _x]]),
                         [_lambda, [_x], [_progn, [_mul, _x, _x], [_add, _x, _x]]])

        # `x => expand_quasiquote(x)
        self.assertEqual(_expand([_quasiquote, 3]), [_quote, 3])

        # (let ((var val)) body) => ((lambda (var) body) val)
        self.assertEqual(_expand([_let, [[_x, 10], [_y, 20]], [_mul, _x, _y]]),
                         [[_lambda, [_x, _y], [_mul, _x, _y]], 10, 20])

        # (flet ((func var exp)) body) => (progn body_replaced_func_to_lambda)
        self.assertEqual(_expand([_flet, [[_func, [_x], [_mul, _x, _x]]], [_func, 3]]),
                         [_progn, [[_lambda, [_x], [_mul, _x, _x]], 3]])

        # (m arg...) => macroexpand if m isinstance macro
        self.assertEqual(_expand([_test, 1, 2]), [_add, 1, 2])
