import unittest
import symbol
import env
import operator as op

class UnitTestCase(unittest.TestCase):
    def testEnv_parse_rest_argument(self):
        params = ['x', '&REST', 'y']
        args = [1, 2, 3, 4, 5]
        params, args = env._Environment._parse_rest_argument(params, args)
        self.assertEqual(params, ['x', 'y'])
        self.assertEqual(args, [1, [2, 3, 4, 5]])

        params = ['x', '&BODY', 'y']
        args = [1, 2, 3, 4, 5]
        params, args = env._Environment._parse_rest_argument(params, args)
        self.assertEqual(params, ['x', 'y'])
        self.assertEqual(args, [1, [2, 3, 4, 5]])

    def testEnvironment(self):
        param = symbol._Symbol('a')
        args = [2, 3, 4]
        test_env = env._Environment(param, args)
        self.assertEqual(test_env[param], args)

        param = ['x', 'y', 'z']
        args = [2, 3, 4]
        test_env = env._Environment(param, args)
        self.assertEqual(test_env['x'], 2)
        self.assertEqual(test_env['y'], 3)
        self.assertEqual(test_env['z'], 4)

        param = ['x', 'y']
        args = [2]
        self.assertRaisesRegex(TypeError, "expected .*, given .*", env._Environment, param, args)

        param = ['x', 'y', 'z']
        args = [2, 3, 4]
        test_env = env._Environment(param, args)
        self.assertEqual(test_env.find('x'), test_env)
        self.assertRaisesRegex(LookupError, "a", test_env.find, 'a')

        param = ['a']
        args = [1]
        sub_test_env = env._Environment(param, args, outer=test_env)
        self.assertEqual(sub_test_env.find('a'), sub_test_env)
        self.assertEqual(sub_test_env.find('x'), test_env)

        params = ['x', '&REST', 'y']
        args = [1, 2, 3, 4, 5]
        test_env = env._Environment(params, args)
        self.assertEqual(test_env['x'], 1)
        self.assertEqual(test_env['y'], [2, 3, 4, 5])

    def test_variable_env(self):
        self.assertIsInstance(env._var_env, env._Environment)
        self.assertIsInstance(env._var_env, env._VariableEnvironment)

    def test_function_env(self):
        self.assertIsInstance(env._func_env, env._Environment)
        self.assertIsInstance(env._func_env, env._FunctionEnvironment)

    def test_macro_env(self):
        self.assertIsInstance(env._macro_env, env._Environment)
        self.assertIsInstance(env._macro_env, env._MacroEnvironment)
