from unittest import TestCase
from clispy.symbol import _Symbol, _quote, _if, _set, _define, _lambda, _begin
from clispy.env import _global_env
from clispy.eval import _Procedure, _eval

class UnitTestCase(TestCase):
    def testProcedure(self):
        # (lambda (x) (* x x))
        params = ['x']
        exp = ['*', 'x', 'x']
        proc = _Procedure(params, exp, _global_env)
        self.assertIsInstance(proc, _Procedure)

    def test_eval(self):
        # variable reference
        self.assertEqual(_eval(_Symbol('round')), round)
        # constant literal
        self.assertEqual(_eval(1), 1)
        # (quote exp)
        self.assertEqual(_eval([_quote, [1, 2, 3]]), [1, 2, 3])
        # (if test conseq alt)
        self.assertEqual(_eval([_if, [_Symbol('>'), 1, 2], 3, 4]), 4)
        # (define var 3)
        result = _eval([_define, _Symbol('var'), 3])
        self.assertEqual(_eval(_Symbol('var')), 3)
        # (set! var 4)
        result = _eval([_set, _Symbol('var'), 4])
        self.assertEqual(_eval(_Symbol('var')), 4)
        # (define func (lambda (x) (* x x)))
        _eval([_define, _Symbol('func'),
               [_lambda, [_Symbol('x')], [_Symbol('*'), _Symbol('x'), _Symbol('x')]]])
        self.assertIsInstance(_eval(_Symbol('func')), _Procedure)
        # (begin exp+)
        result = _eval([_begin,
                        [_define, _Symbol('a'), 2],
                        [_define, _Symbol('b'), 3],
                        [_Symbol('+'), _Symbol('a'), _Symbol('b')]])
        self.assertEqual(result, 5)
        # others
        self.assertEqual(_eval([_Symbol('func'), 3]), 9)
        self.assertEqual(_eval([_Symbol('+'), 2, 3]), 5)
