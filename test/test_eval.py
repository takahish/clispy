import unittest
import symbol
import env
import eval

class UnitTestCase(unittest.TestCase):
    def testProcedure(self):
        # (lambda (x) (* x x))
        params = ['x']
        exp = ['*', 'x', 'x']
        proc = eval._Procedure(params, exp, env._var_env, env._func_env)
        self.assertIsInstance(proc, eval._Procedure)

    def test_eval(self):
        _eval = eval._eval

        _quote    = symbol._quote
        _setq     = symbol._setq
        _if       = symbol._if
        _defun    = symbol._defun
        _lambda   = symbol._lambda
        _progn    = symbol._progn
        _function = symbol._function
        _funcall  = symbol._funcall

        # variable reference
        var1 = symbol._Symbol('var1')
        self.assertRaisesRegex(LookupError, "var1", _eval, var1)

        # (setq var 1)
        eval._eval([_setq, var1, 1])
        self.assertEqual(_eval(var1), 1)

        # constant literal
        self.assertEqual(_eval(1), 1)

        # (quote exp)
        self.assertEqual(_eval([_quote, [1, 2, 3]]), [1, 2, 3])

        # (if test conseq alt)
        _gt = symbol._Symbol('>')
        self.assertEqual(_eval([_if, [_gt, 2, 1], 3, 4]), 3)
        self.assertEqual(_eval([_if, [_gt, 1, 2], 3, 4]), 4)

        # (defun func (lambda (x) (* x x)))
        _func1 = symbol._Symbol('func1')
        _mult  = symbol._Symbol('*')
        _x     = symbol._Symbol('x')
        eval._eval([_defun, _func1, [_lambda, [_x], [_mult, _x, _x]]])
        self.assertRaisesRegex(LookupError, "func1", _eval, _func1)
        self.assertIsInstance(env._func_env['func1'], eval._Procedure)

        # (progn exp+)
        _var2 = symbol._Symbol('var2')
        _var3 = symbol._Symbol('var3')
        _add  = symbol._Symbol('+')
        result = eval._eval([_progn,
                        [_setq, _var2, 2],
                        [_setq, _var3, 3],
                        [_add, _var2, _var3]])
        self.assertEqual(result, 5)

        # (function func)
        self.assertIsInstance(eval._eval([_function, _func1]), eval._Procedure)

        # (funcall func args)
        _func2 = symbol._Symbol('func2')
        _add   = symbol._Symbol('+')
        _x     = symbol._Symbol('x')
        _y     = symbol._Symbol('y')
        eval._eval([_setq, _func2, [_lambda, [_x, _y], [_add, _x, _y]]])
        self.assertEqual(eval._eval([_funcall, _func2, 1, 2]), 3)

        # others
        self.assertEqual(eval._eval([_func1, 3]), 9)
        self.assertEqual(eval._eval([_add, 2, 3]), 5)
