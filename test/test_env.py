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

