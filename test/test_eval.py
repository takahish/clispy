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
from clispy import eval
from clispy import func


class UnitTestCase(unittest.TestCase):
    def setUp(self):
        self._eval = eval.closure(symbol, env, func)

    def test_cons_cell(self):
        _a = symbol.Symbol('a')
        _b = symbol.Symbol('b')
        _c = symbol.Symbol('c')

        _dot = symbol.DOT

        # cons
        self.assertIsInstance(eval._cons_cell([_a, _b, _c]), cons.Cons)
        self.assertEqual(eval._cons_cell([_a, _b, _c]), ['a', 'b', 'c'])

        # dotted pair
        self.assertIsInstance(eval._cons_cell([_a, _dot, _b]), cons.DottedPair)
        self.assertEqual(eval._cons_cell([_a, _dot, _b]), ['a', 'b'])

        # dotted pair with nil
        self.assertIsInstance(eval._cons_cell([_a, _dot, False]), cons.Cons)
        self.assertEqual(eval._cons_cell([_a, _dot, False]), ['a'])

    def test_eval(self):
        _eval = self._eval

        _quote    = symbol.QUOTE
        _setq     = symbol.SETQ
        _if       = symbol.IF
        _defun    = symbol.DEFUN
        _lambda   = symbol.LAMBDA
        _progn    = symbol.PROGN
        _function = symbol.FUNCTION
        _funcall  = symbol.FUNCALL

        # variable reference
        var1 = symbol.Symbol('var1')
        self.assertRaisesRegex(LookupError, "var1", _eval, var1)

        # (setq var 1)
        _eval([_setq, var1, 1])
        self.assertEqual(_eval(var1), 1)

        # constant literal
        self.assertEqual(_eval(1), 1)

        # (quote exp)
        self.assertEqual(_eval([_quote, [1, 2, 3]]), [1, 2, 3])

        # (if test conseq alt)
        _gt = symbol.Symbol('>')
        self.assertEqual(_eval([_if, [_gt, 2, 1], 3, 4]), 3)
        self.assertEqual(_eval([_if, [_gt, 1, 2], 3, 4]), 4)

        # (defun func (lambda (x) (* x x)))
        _func1 = symbol.Symbol('func1')
        _mult  = symbol.Symbol('*')
        _x     = symbol.Symbol('x')
        _eval([_defun, _func1, [_lambda, [_x], [_mult, _x, _x]]])
        self.assertRaisesRegex(LookupError, "func1", _eval, _func1)
        #self.assertIsInstance(env.func_env['func1'], eval._Procedure)

        # (progn exp+)
        _var2 = symbol.Symbol('var2')
        _var3 = symbol.Symbol('var3')
        _add  = symbol.Symbol('+')
        result = _eval([_progn,
                             [_setq, _var2, 2],
                             [_setq, _var3, 3],
                             [_add, _var2, _var3]])
        self.assertEqual(result, 5)

        # (function func)
        #self.assertIsInstance(_eval([_function, _func1]), eval._Procedure)

        # (funcall func args)
        _func2 = symbol.Symbol('func2')
        _add   = symbol.Symbol('+')
        _x     = symbol.Symbol('x')
        _y     = symbol.Symbol('y')
        _eval([_setq, _func2, [_lambda, [_x, _y], [_add, _x, _y]]])
        self.assertEqual(_eval([_funcall, _func2, 1, 2]), 3)

        # others
        self.assertEqual(_eval([_func1, 3]), 9)
        self.assertEqual(_eval([_add, 2, 3]), 5)
