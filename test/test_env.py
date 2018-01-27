import unittest
import symbol
import env
import operator as op

class UnitTestCase(unittest.TestCase):
    def testEnv(self):
        param = symbol._Symbol('a')
        args = [2, 3, 4]
        test_env = env._Env(param, args)
        self.assertEqual(test_env[param], args)

        param = ['x', 'y', 'z']
        args = [2, 3, 4]
        test_env = env._Env(param, args)
        self.assertEqual(test_env['x'], 2)
        self.assertEqual(test_env['y'], 3)
        self.assertEqual(test_env['z'], 4)

        param = ['x', 'y']
        args = [2]
        self.assertRaisesRegex(TypeError, "expected .*, given .*", env._Env, param, args)

        param = ['x', 'y', 'z']
        args = [2, 3, 4]
        test_env = env._Env(param, args)
        self.assertEqual(test_env.find('x'), test_env)
        self.assertRaisesRegex(LookupError, "a", test_env.find, 'a')

        param = ['a']
        args = [1]
        sub_test_env = env._Env(param, args, outer=test_env)
        self.assertEqual(sub_test_env.find('a'), sub_test_env)
        self.assertEqual(sub_test_env.find('x'), test_env)

    def test_standard_env(self):
        self.assertIsInstance(env._global_env, env._Env)

    def test_standard_procedures(self):
        test_env = env._global_env

        # +, -, *, /
        self.assertEqual(test_env.find('+')['+'](2, 3, 4), 9)
        self.assertEqual(test_env.find('-')['-'](2, 3, 4), -5)
        self.assertEqual(test_env.find('*')['*'](2, 3, 4), 24)
        self.assertEqual(test_env.find('/')['/'](4, 2, 2), 1)

        # >, <, >=, <=, =
        self.assertTrue(test_env.find('>')['>'](4, 3, 2))
        self.assertFalse(test_env.find('>')['>'](3, 2, 4))
        self.assertTrue(test_env.find('<')['<'](2, 3, 4))
        self.assertFalse(test_env.find('<')['<'](3, 4, 2))
        self.assertTrue(test_env.find('>=')['>='](3, 3, 2))
        self.assertFalse(test_env.find('>=')['>='](3, 2, 3))
        self.assertTrue(test_env.find('<=')['<='](3, 3, 4))
        self.assertFalse(test_env.find('<=')['<='](3, 4, 3))
        self.assertTrue(test_env.find('=')['='](2, 2, 2))
        self.assertFalse(test_env.find('=')['='](2, 2, 3))

        # abs, round
        self.assertEqual(test_env.find('abs')['abs'](-3), 3)
        self.assertEqual(test_env.find('round')['round'](3.4), 3)
        self.assertEqual(test_env.find('round')['round'](3.5), 4)

        # append
        self.assertEqual(test_env.find('append')['append']([2, 3], [4], [5, 6]), [2, 3, 4, 5, 6])

        # apply
        self.assertEqual(test_env.find('apply')['apply'](op.add, [2, 3]), 5)

        # begin
        self.assertEqual(test_env.find('begin')['begin'](2, 3, 4), 4)

        # car, cdr, cons
        self.assertEqual(test_env.find('car')['car']([2, 3, 4]), 2)
        self.assertEqual(test_env.find('cdr')['cdr']([2, 3, 4]), [3, 4])
        self.assertEqual(test_env.find('cons')['cons'](2, [3, 4]), [2, 3, 4])

        # eq?, equal?
        self.assertTrue(test_env.find('eq?')['eq?'](2, 2))
        self.assertFalse(test_env.find('eq?')['eq?']([2, 3], [2, 3]))
        self.assertTrue(test_env.find('equal?')['equal?']([2, 3], [2, 3]))

        # length, list, list?
        self.assertEqual(test_env.find('length')['length']([2, 3, 4]), 3)
        self.assertEqual(test_env.find('list')['list'](2, 3, 4), [2, 3, 4])
        self.assertTrue(test_env.find('list?')['list?']([2, 3, 4]))

        # map
        self.assertEqual(test_env.find('map')['map'](abs, [-2, -3, -4]), [2, 3, 4])

        # max, min
        self.assertEqual(test_env.find('max')['max'](2, 4, 3), 4)
        self.assertEqual(test_env.find('min')['min'](4, 2, 3), 2)

        # not, null?
        self.assertTrue(test_env.find('not')['not'](op.gt(2, 3)))
        self.assertTrue(test_env.find('null?')['null?']([]))

        # number?, procedure?, symbol?
        self.assertTrue(test_env.find('number?')['number?'](3))
        self.assertTrue(test_env.find('procedure?')['procedure?'](op.add))
        self.assertTrue(test_env.find('symbol?')['symbol?'](symbol._quote))
