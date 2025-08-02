# Copyright 2025 Takahiro Ishikawa. All Rights Reserved.
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
from clispy.evaluator import Evaluator
from clispy.expander import Expander
from clispy.function.lambda_expression import Lambda
from clispy.package import PackageManager
from clispy.parser import Parser
from clispy.type import Integer


class LambdaUnitTestCase(unittest.TestCase):
    """This test is to check clispy.function_.lambda_expression.Lambda.
    Lambda is base of user defined function and macro.
    """
    def testLambda(self):
        """Checks an instance of Lambda and object official representation.
        """
        # Makes an instance of Lambda.
        forms = Parser.parse('((x) (* x x x))')
        lambda_func = Lambda(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks lambda function.
        self.assertTrue(callable(lambda_func))

        # Checks official representation.
        self.assertRegex(str(lambda_func), r"<FUNCTION LAMBDA \{[0-9A-Z]+\}")

    def testLambda_properties(self):
        """Checks object properties. Properties are as follow,

            self.params: Parameters.
            self.forms: Body (forms).
            self.var_env: Lexical variable environment.
            self.func_env: Lexical function environment.
            self.macro_env: Lexical macro environment.
        """
        # makes an instance of Lmabda.
        forms = Parser.parse('((x) (* x x x))')
        lambda_func = Lambda(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks properties.

        # Checks lambda_func.params.
        self.assertEqual(lambda_func.params, ['X'])

        # Checks lambda_func.forms.
        self.assertEqual(str(lambda_func.forms), '((* X X X))')

        # Checks lambda_func lexical scope.
        self.assertTrue(lambda_func.var_env is PackageManager.current_package.env['VARIABLE'])
        self.assertTrue(lambda_func.func_env is PackageManager.current_package.env['FUNCTION'])
        self.assertTrue(lambda_func.macro_env is PackageManager.current_package.env['MACRO'])

    def testLambda_call(self):
        """Checks call method of Lambda. The body of Lmabda is expanded
        and executed when the method is called.
        """
        # Makes an instance of Lambda.
        forms = Parser.parse('((x) (* x x x))')
        lambda_func = Lambda(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks call.
        retval = lambda_func(
            Parser.parse('(2)'),
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks return value.
        self.assertTrue(retval, Integer(8))

    def testLambda_implicit_progn(self):
        """Multiple body forms are evaluated sequentially, returning the last."""

        lambda_forms = Parser.parse('(() (setq x 1) (setq x 3))')
        lambda_func = Lambda(
            lambda_forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO'],
        )

        retval = lambda_func(
            Parser.parse('()'),
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO'],
        )

        self.assertEqual(
            PackageManager.current_package.env['VARIABLE'].find('X')['X'],
            Integer(3),
        )
        self.assertTrue(retval is Integer(3))

    def testLambda_call_evaluate_argument(self):
        """Arguments are evaluated before the call method is executed.
        """
        # Makes an instance of Lambda.
        forms = Parser.parse('((x) (* x x x))')
        lambda_func = Lambda(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks call.
        retval = lambda_func(
            Parser.parse('((* 2 2 2))'), # an argument is expression.
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks return value.
        self.assertTrue(retval, Integer(512))

    def testLambda_call_expand_argument(self):
        """Arguments are expanded before the call method is executed.
        """
        # Makes an instance of Lambda.
        forms = Parser.parse('((x) (* x x x))')
        lambda_func = Lambda(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Define macro.
        forms = Parser.parse('(defmacro cube (x) `(* ,x ,x ,x))')
        exp = Expander.expand(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )
        Evaluator.eval(
            exp,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks call.
        retval = lambda_func(
            Parser.parse('((cube 2))'), # an argument is expression.
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks return value.
        self.assertTrue(retval, Integer(512))

    def testLambda_properties_optional_accessor(self):
        """Checks a propertie of optional accessor for arguments.
        """
        # Makes an instance of Lmabda.
        forms = Parser.parse('((x &optional y) (* x x x))')
        lambda_func = Lambda(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks properties.

        # Checks lambda_func.params.
        self.assertEqual(lambda_func.params, ['X', 'Y'])

        # Checks lambda_func.accessor_index.
        self.assertEqual(lambda_func.accessor_index['&OPTIONAL'], 1)

    def testLambda_properties_rest_accessor(self):
        """Checks a propertie of rest accessor for arguments.
        """
        # Makes an instance of Lmabda.
        forms = Parser.parse('((x &rest y) (* x x x))')
        lambda_func = Lambda(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks properties.

        # Checks lambda_func.params.
        self.assertEqual(lambda_func.params, ['X', 'Y'])

        # Checks lambda_func.accessor_index.
        self.assertEqual(lambda_func.accessor_index['&REST'], 1)

    def testLambda_properties_keyword_accessor(self):
        # Makes an instance of Lambda.
        forms = Parser.parse('((x &key y) (* x x x))')

        lambda_func = Lambda(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks properties.

        # Checks lambda_func.params.

        self.assertEqual(lambda_func.params, ['X', 'Y'])

        # Checks lambda_func.accessor_index.
        self.assertEqual(lambda_func.accessor_index['&KEY'], 1)

    def testLambda_call_optional_argument(self):
        """Checks assigning optinal arguments.
        """
        # Makes an instance of Lmabda.
        forms = Parser.parse('((x &optional y) (if y (* x x) (* x x x))))')
        lambda_func = Lambda(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks call.

        # When an optional argumet is not given, Null() is set to an argument.
        retval = lambda_func(
            Parser.parse('(2)'),
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # retval is result of (* x x x).
        self.assertTrue(retval is Integer(8))

        # When an optional argumet is given, this is set to an argument.
        retval = lambda_func(
            Parser.parse('(2 t)'),
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # retval is result of (* x x).
        self.assertTrue(retval is Integer(4))

    def testLambda_call_optional_argument_with_default_value(self):
        """Checks assigning optinal arguments.
        """
        # Makes an instance of Lmabda.
        forms = Parser.parse('((x &optional (y t)) (if y (* x x) (* x x x))))')
        lambda_func = Lambda(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks call.

        # When an optional argumet is not given, Null() is set to an argument.
        retval = lambda_func(
            Parser.parse('(2)'),
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # retval is result of (* x x x).
        self.assertTrue(retval is Integer(4))

        # When an optional argumet is given, this is set to an argument.
        retval = lambda_func(
            Parser.parse('(2 nil)'),
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # retval is result of (* x x).
        self.assertTrue(retval is Integer(8))

    def testLambda_call_rest_argument(self):
        """Checks assigning rest arguments.
        """
        # Makes an instance of Lmabda.
        forms = Parser.parse('((x &rest y) (cons x y))')
        lambda_func = Lambda(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks call.

        # When an optional argumet is not given, Null() is set to an argument.
        retval = lambda_func(
            Parser.parse('(1 2 3 4 5)'),
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # retval is result of being given &rest parameter.
        self.assertEqual(str(retval), '(1 2 3 4 5)')

    def testLambda_call_keyword_argument(self):
        """Checks assigning keyword arguments.
        """
        # Makes an instance of Lmabda.
        forms = Parser.parse('((x &key y) (if y (* x x) (* x x x))))')
        lambda_func = Lambda(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks call.

        # When a keyword argumet is not given, Null() is set to an argument.
        retval = lambda_func(
            Parser.parse('(2)'),
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # retval is result of (* x x x).
        self.assertTrue(retval is Integer(8))

        # When an keyword argumet is given, this is set to an argument.
        retval = lambda_func(
            Parser.parse('(2 :y t)'),
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # retval is result of (* x x).
        self.assertTrue(retval is Integer(4))

    def testLambda_call_keyword_argument_with_default_value(self):
        """Checks assigning keyword arguments.
        """
        # Makes an instance of Lmabda.
        forms = Parser.parse('((x &key (y t)) (if y (* x x) (* x x x))))')
        lambda_func = Lambda(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks call.

        # When a keyword argumet is not given, Null() is set to an argument.
        retval = lambda_func(
            Parser.parse('(2)'),
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # retval is result of (* x x x).
        self.assertTrue(retval is Integer(4))

        # When an keyword argumet is given, this is set to an argument.
        retval = lambda_func(
            Parser.parse('(2 :y nil)'),
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # retval is result of (* x x).
        self.assertTrue(retval is Integer(8))
