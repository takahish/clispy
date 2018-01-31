import unittest
import symbol
import env
import operator as op

class UnitTestCase(unittest.TestCase):
    def testEnv_parse_rest_argument(self):
        params = ['x', '.', 'y']
        args = [1, 2, 3, 4, 5]
        params, args = env._Env._parse_rest_argument(params, args)
        self.assertEqual(params, ['x', 'y'])
        self.assertEqual(args, [1, [2, 3, 4, 5]])

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

        params = ['x', '.', 'y']
        args = [1, 2, 3, 4, 5]
        test_env = env._Env(params, args)
        self.assertEqual(test_env['x'], 1)
        self.assertEqual(test_env['y'], [2, 3, 4, 5])

    def test_standard_env(self):
        self.assertIsInstance(env._global_env, env._Env)

