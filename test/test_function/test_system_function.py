# Copyright 2019 Takahiro Ishikawa. All Rights Reserved.
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
from clispy.type import Cons, Integer, Null, Ratio, String, Symbol
from clispy.function.system_function import *
from clispy.package import PackageManager
from clispy.parser import Parser


class SystemFunctionUnitTestCase(unittest.TestCase):
    def testSystemFunction(self):
        # Makes an instance of SystemFunction.
        func = SystemFunction()

        # Checks official representation.
        self.assertRegex(str(func), r"#<SYSTEM-FUNCTION SYSTEM-FUNCTION \{[0-9A-Z]+\}>")

    def test_eval_forms(self):
        forms = Parser.parse("(1 'hoge \"fuga\")")

        # Evaluates forms
        args = SystemFunction.eval_forms(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO'],
        )

        # Checks type of args.
        self.assertIsInstance(args, Cons)

        # Checks args
        self.assertTrue(args.car is Integer(1))
        self.assertTrue(args.cdr.car is Symbol('HOGE'))
        self.assertTrue(args.cdr.cdr.car is String('fuga'))
        self.assertTrue(args.cdr.cdr.cdr is Null())


class LambdaSystemFunctionUnitTestCase(unittest.TestCase):
    def testLambdaSystemFunction(self):
        # Makes an instance of LambdaSytemFunction.
        forms = Parser.parse('((x) (* x x x))')
        lambda_func = Lambda(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks official representation.
        self.assertRegex(str(lambda_func), r"#<FUNCTION LAMBDA \{[0-9A-Z]+\}>")

    def testLambdaSystemFunction_call(self):
        # Makes an instance of LambdaSytemFunction.
        forms = Parser.parse('((x) (* x x x))')
        lambda_func = Lambda(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Executes lambda function.
        retval = lambda_func(
            Parser.parse("(2)"),
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks return value.
        # exp is (* x x x) and arg is 2, so retval must be Integer(8).
        self.assertTrue(retval is Integer(8))


class DefunSystemFunctionUnitTestCase(unittest.TestCase):
    def testDefunSytemFunction(self):
        # Makes an instance of DefuncSystemFunction.
        defun = DefunSystemFunction()

        # Checks official representation.
        self.assertRegex(str(defun), r"#<SYSTEM-FUNCTION DEFUN \{[0-9A-Z]+\}>")

    def testDefunSytemFunction_call(self):
        # Makes an instance of DefunSystemFunction.
        defun = DefunSystemFunction()

        # Calls defun.
        forms = Parser.parse('(func (x) (* x x x))')
        retval = defun(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks retval.
        self.assertTrue(retval, Symbol('FUNC'))

        # Gets function object.
        func = PackageManager.find('FUNC', package_name=None, status_check=False, env='FUNCTION')['FUNC']

        # Checks type.
        self.assertIsInstance(func, Lambda)


class DefmacroSystemFunctionUnitTestCase(unittest.TestCase):
    def testDefmacroSystemFunction(self):
        # Makes an instance of DefmacroSystemFunction.
        defmacro = DefmacroSystemFunction()

        # Checks official representation.
        self.assertRegex(str(defmacro), r"#<SYSTEM-FUNCTION DEFMACRO \{[0-9A-Z]+\}>")

    def testDefmacroSystemFunction_call(self):
        # Makes an instance of DefmacroSystemFunction.
        defmacro = DefmacroSystemFunction()

        # Calls defmacro.
        forms = Parser.parse('(mac (x) `(* ,x ,x ,x))')
        retval = defmacro(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks retval.
        self.assertTrue(retval, Symbol('MACRO'))

        # Gets macro object.
        mac = PackageManager.find('MAC', package_name=None, status_check=False, env='MACRO')['MAC']

        # Checks type.
        self.assertIsInstance(mac, Lambda)


class ConsSystemFunctionUnitTestCase(unittest.TestCase):
    def testConsSystemFunction(self):
        # Makes an instance of ConsSystemFunction.
        cons = ConsSystemFunction()

        # Checks official representation.
        self.assertRegex(str(cons), r"#<SYSTEM-FUNCTION CONS \{[0-9A-Z]+\}>")

    def testConsSystemFunction_call(self):
        # Makes an instance of ConsSystemFunction.
        cons = ConsSystemFunction()

        # Calls cons.
        forms = Parser.parse('(1 2)')
        retval = cons(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks type of return value.
        self.assertIsInstance(retval, Cons)

        # Chesk return value.
        self.assertTrue(retval, Cons(Integer(1), Integer(2)))


class CarSystemFunctionUnitTestCase(unittest.TestCase):
    def testCarSystemFunction(self):
        # Makes an instance of CarSystemFunction.
        car = CarSystemFunction()

        # Checks official representation.
        self.assertRegex(str(car), r"#<SYSTEM-FUNCTION CAR \{[0-9A-Z]+\}>")

    def testConsSystemFunction_call(self):
        # Makes an instance of CarSystemFunction.
        car = CarSystemFunction()

        # Calls cons.
        forms = Parser.parse("('(1 2))")
        retval = car(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Chesk return value.
        self.assertTrue(retval, Integer(1))


class CdrSystemFunctionUnitTestCase(unittest.TestCase):
    def testCdrSystemFunction(self):
        # Makes an instance of CdrSystemFunction.
        cdr = CdrSystemFunction()

        # Checks official representation.
        self.assertRegex(str(cdr), r"#<SYSTEM-FUNCTION CDR \{[0-9A-Z]+\}>")

    def testConsSystemFunction_call(self):
        # Makes an instance of CdrSystemFunction.
        cdr = CdrSystemFunction()

        # Calls cons.
        forms = Parser.parse("('(1 2))")
        retval = cdr(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Chesk return value.
        self.assertTrue(retval, Integer(2))


class AppendSystemFunctionUniteTestCase(unittest.TestCase):
    def testAppendSystemFunction(self):
        # Makes an instance of AppendSystemFunction.
        append = AppendSystemFunction()

        # Checks official representation.
        self.assertRegex(str(append), r"#<SYSTEM-FUNCTION APPEND \{[0-9A-Z]+\}>")

    def testAppendSystemFunction_call(self):
        # Makes an instance of AppendSystemFunction.
        append = AppendSystemFunction()

        # Calls cons.
        forms = Parser.parse("('(1 2) '(3))")
        retval = append(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Chesk return value.
        self.assertTrue(retval, Cons(Integer(1), Cons(Integer(2), Cons(Integer(3), Null()))))

    def testAppendSystemFunction_call_dotted_list(self):
        # Makes an instance of AppendSystemFunction.
        append = AppendSystemFunction()

        # Calls cons.
        forms = Parser.parse("('(1 2) 3)")
        retval = append(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Chesk return value.
        self.assertTrue(retval, Cons(Integer(1), Cons(Integer(2), Integer(3))))


class AddSystemFunctionUnitTestCase(unittest.TestCase):
    def testAddSystemFunction(self):
        # Makes an instance of AddSystemFunction.
        add = AddSystemFunction()

        # Checks official representation.
        self.assertRegex(str(add), r"#<SYSTEM-FUNCTION \+ \{[0-9A-Z]+\}>")

    def testAddSystemFunction_call(self):
        # Makes an instance of AddSystemFunction.
        add = AddSystemFunction()

        # Calls add.
        forms = Parser.parse('(1 2 3)')
        retval = add(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Chesk return value.
        self.assertTrue(retval, Integer(6))


class SubSystemFunctionUnitTestCase(unittest.TestCase):
    def testSubSystemFunction(self):
        # Makes an instance of SubSystemFunction.
        sub = SubSystemFunction()

        # Checks official representation.
        self.assertRegex(str(sub), r"#<SYSTEM-FUNCTION - \{[0-9A-Z]+\}>")

    def testSubSystemFunction_call(self):
        # Makes an instance of SubSystemFunction.
        sub = SubSystemFunction()

        # Calls sub.
        forms = Parser.parse('(1 2 3)')
        retval = sub(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Chesk return value.
        self.assertTrue(retval, Integer(-4))


class MulSystemFunctionUnitTestCase(unittest.TestCase):
    def testMulSystemFunction(self):
        # Makes an instance of MulSystemFunction.
        mul = MulSystemFunction()

        # Checks official representation.
        self.assertRegex(str(mul), r"#<SYSTEM-FUNCTION \* \{[0-9A-Z]+\}>")

    def testMulSystemFunction_call(self):
        # Makes an instance of MulSystemFunction.
        mul = MulSystemFunction()

        # Calls mul.
        forms = Parser.parse('(1 2 3)')
        retval = mul(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Chesk return value.
        self.assertTrue(retval, Integer(6))


class DivSystemFunctionUnitTestCase(unittest.TestCase):
    def testDivSystemFunction(self):
        # Makes an instance of DivSystemFunction.
        div = DivSystemFunction()

        # Checks official representation.
        self.assertRegex(str(div), r"#<SYSTEM-FUNCTION / \{[0-9A-Z]+\}>")

    def testMulSystemFunction_call(self):
        # Makes an instance of DivSystemFunction.
        div = DivSystemFunction()

        # Calls cons.
        forms = Parser.parse('(1 2 3)')
        retval = div(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Chesk return value.
        self.assertTrue(retval, Ratio('1/6'))
