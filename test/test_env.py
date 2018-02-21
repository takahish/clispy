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


class UnitTestCase(unittest.TestCase):
    def testEnv_parse_rest_argument(self):
        params = ['x', '&REST', 'y']
        args = [1, 2, 3, 4, 5]
        params, args = env._Env._parse_rest_argument(params, args)
        self.assertEqual(params, ['x', 'y'])
        self.assertEqual(args, [1, [2, 3, 4, 5]])

        params = ['x', '&BODY', 'y']
        args = [1, 2, 3, 4, 5]
        params, args = env._Env._parse_rest_argument(params, args)
        self.assertEqual(params, ['x', 'y'])
        self.assertEqual(args, [1, [2, 3, 4, 5]])

    def testEnvironment(self):
        param = symbol.Symbol('a')
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

        params = ['x', '&REST', 'y']
        args = [1, 2, 3, 4, 5]
        test_env = env._Env(params, args)
        self.assertEqual(test_env['x'], 1)
        self.assertEqual(test_env['y'], [2, 3, 4, 5])

