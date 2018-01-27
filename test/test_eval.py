import unittest
import symbol
import env
import eval

class UnitTestCase(unittest.TestCase):
    def testProcedure(self):
        # (lambda (x) (* x x))
        params = ['x']
        exp = ['*', 'x', 'x']
        proc = eval._Procedure(params, exp, env._global_env)
        self.assertIsInstance(proc, eval._Procedure)

    def test_eval(self):
        # variable reference
        self.assertEqual(eval._eval(symbol._Symbol('round')), round)
        # constant literal
        self.assertEqual(eval._eval(1), 1)
        # (quote exp)
        self.assertEqual(eval._eval([symbol._quote, [1, 2, 3]]), [1, 2, 3])
        # (if test conseq alt)
        self.assertEqual(eval._eval([symbol._if, [symbol._Symbol('>'), 1, 2], 3, 4]), 4)
        # (define var 3)
        result = eval._eval([symbol._define, symbol._Symbol('var'), 3])
        self.assertEqual(eval._eval(symbol._Symbol('var')), 3)
        # (set! var 4)
        result = eval._eval([symbol._set, symbol._Symbol('var'), 4])
        self.assertEqual(eval._eval(symbol._Symbol('var')), 4)
        # (define func (lambda (x) (* x x)))
        eval._eval([symbol._define, symbol._Symbol('func'),
               [symbol._lambda, [symbol._Symbol('x')], [symbol._Symbol('*'), symbol._Symbol('x'), symbol._Symbol('x')]]])
        self.assertIsInstance(eval._eval(symbol._Symbol('func')), eval._Procedure)
        # (begin exp+)
        result = eval._eval([symbol._begin,
                        [symbol._define, symbol._Symbol('a'), 2],
                        [symbol._define, symbol._Symbol('b'), 3],
                        [symbol._Symbol('+'), symbol._Symbol('a'), symbol._Symbol('b')]])
        self.assertEqual(result, 5)
        # others
        self.assertEqual(eval._eval([symbol._Symbol('func'), 3]), 9)
        self.assertEqual(eval._eval([symbol._Symbol('+'), 2, 3]), 5)
