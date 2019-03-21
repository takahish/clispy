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
from clispy.function.system_function import *
from clispy.interpreter import Interrupt
from clispy.package import PackageManager
from clispy.parser import Parser
from clispy.type import Cons, Integer, Keyword, Null, Ratio, String, Symbol


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
        self.assertEqual(retval, Integer(8))


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
        self.assertEqual(retval, Symbol('FUNC'))

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


class EqSystemFunctionUnitTestCase(unittest.TestCase):
    def testEqSystemFunction(self):
        # Make an instance of EqSystemFunction.
        eq = EqSystemFunction()

        # Checks official representation.
        self.assertRegex(str(eq), r"#<SYSTEM-FUNCTION EQ \{[0-9A-Z]+\}>")

    def testEqSystemFunction_call_t(self):
        # Make an instance of EqSystemFunction.
        eq = EqSystemFunction()

        # If args are the same objects, eq must return T.
        forms = Parser.parse('(1 1)')
        retval = eq(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks return value.
        self.assertEqual(retval, T())

    def testEqSystemFunction_call_nil(self):
        # Make an instance of EqSystemFunction.
        eq = EqSystemFunction()

        # If args are not the same objects, eq return nil.
        forms = Parser.parse('(1 1.0)')
        retval = eq(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks return value.
        self.assertEqual(retval, Null())

    def testEqSystemFunction_call_cons(self):
        # Make an instance of EqSystemFunction.
        eq = EqSystemFunction()

        # If args are cons, eq return nil because cons returns a different object.
        forms = Parser.parse('((cons 1 1) (cons 1 1))')
        retval = eq(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks return value
        self.assertEqual(retval, Null())


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

        # Chesk return value.
        self.assertEqual(str(retval), '(1 . 2)')


class CarSystemFunctionUnitTestCase(unittest.TestCase):
    def testCarSystemFunction(self):
        # Makes an instance of CarSystemFunction.
        car = CarSystemFunction()

        # Checks official representation.
        self.assertRegex(str(car), r"#<SYSTEM-FUNCTION CAR \{[0-9A-Z]+\}>")

    def testConsSystemFunction_call(self):
        # Makes an instance of CarSystemFunction.
        car = CarSystemFunction()

        # Calls car.
        forms = Parser.parse("('(1 2))")
        retval = car(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Chesk return value.
        self.assertEqual(retval, Integer(1))


class CdrSystemFunctionUnitTestCase(unittest.TestCase):
    def testCdrSystemFunction(self):
        # Makes an instance of CdrSystemFunction.
        cdr = CdrSystemFunction()

        # Checks official representation.
        self.assertRegex(str(cdr), r"#<SYSTEM-FUNCTION CDR \{[0-9A-Z]+\}>")

    def testConsSystemFunction_call(self):
        # Makes an instance of CdrSystemFunction.
        cdr = CdrSystemFunction()

        # Calls cdr.
        forms = Parser.parse("('(1 2))")
        retval = cdr(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Chesk return value.
        self.assertEqual(str(retval), '(2)')


class AppendSystemFunctionUniteTestCase(unittest.TestCase):
    def testAppendSystemFunction(self):
        # Makes an instance of AppendSystemFunction.
        append = AppendSystemFunction()

        # Checks official representation.
        self.assertRegex(str(append), r"#<SYSTEM-FUNCTION APPEND \{[0-9A-Z]+\}>")

    def testAppendSystemFunction_call(self):
        # Makes an instance of AppendSystemFunction.
        append = AppendSystemFunction()

        # Calls append.
        forms = Parser.parse("('(1 2) '(3))")
        retval = append(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Chesk return value.
        self.assertEqual(str(retval), '(1 2 3)')


    def testAppendSystemFunction_call_dotted_list(self):
        # Makes an instance of AppendSystemFunction.
        append = AppendSystemFunction()

        # Calls append.
        forms = Parser.parse("('(1 2) 3)")
        retval = append(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Chesk return value.
        self.assertEqual(str(retval), '(1 2 . 3)')


class FindSymbolSytemFunctionUnitTestCase(unittest.TestCase):
    def testFindSystemFunction(self):
        # Make an instance of FindSymbolSystemFunction.
        find_symbol = FindSymbolSystemFunction()

        # Checks official representation.
        self.assertRegex(str(find_symbol), r"#<SYSTEM-FUNCTION FIND-SYMBOL \{[0-9A-Z]+\}>")

    def testFindSystemFunction_call_existed_symbol(self):
        # Make an instance of FindSymbolSystemFunction.
        find_symbol = FindSymbolSystemFunction()

        # Calls find_symbol.
        forms = Parser.parse('("CAR")')
        retval = find_symbol(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Chesk return value.
        self.assertEqual(retval[0], Symbol('CAR'))
        self.assertEqual(retval[1], Keyword(':EXTERNAL'))

    def testFindSymbolFunction_call_not_existed_symbol(self):
        # Make an instance of FindSymbolSystemFunction.
        find_symbol = FindSymbolSystemFunction()

        # Calls find_symbol.
        forms = Parser.parse('("G0")')
        retval = find_symbol(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Chesk return value.
        self.assertEqual(retval[0], Null())
        self.assertEqual(retval[1], Null())


class InternSystemFunctionUnitTestCase(unittest.TestCase):
    def testInternSystemFunction(self):
        # Make an instance of InternSystemFunction.
        intern_ = InternSystemFunction()

        # Checks official representation.
        self.assertRegex(str(intern_), r"#<SYSTEM-FUNCTION INTERN \{[0-9A-Z]+\}>")

    def testInternSystemFunction_call(self):
        # Make an instance of InternSystemFunction.
        intern_ = InternSystemFunction()

        # Calls intern_.
        forms = Parser.parse('("G1")')
        retval = intern_(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Chesk return value.
        self.assertEqual(retval[0], Symbol('G1'))
        self.assertEqual(retval[1], Null())

        # Second calls intern_.
        forms = Parser.parse('("G1")')
        retval = intern_(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Chesk second return value.
        self.assertEqual(retval[0], Symbol('G1'))
        self.assertEqual(retval[1], Keyword(':INTERNAL'))


class ExportSystemFunctionUnitTestCase(unittest.TestCase):
    def testExportSystemFunction(self):
        # Make an instance of ExportSystemFunction.
        export = ExportSystemFunction()

        # Checks official representation.
        self.assertRegex(str(export), r"#<SYSTEM-FUNCTION EXPORT \{[0-9A-Z]+\}>")

    def testExportSystemFunction_call(self):
        # Make an instance of FindSymbolSystemFunction, InternSystemFunction
        # and ExportSystemFunction.
        find_symbol = FindSymbolSystemFunction()
        intern_ = InternSystemFunction()
        export = ExportSystemFunction()

        # Calls intern_.
        forms = Parser.parse('("G2")')
        retval = intern_(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Chesk return value.
        self.assertEqual(retval[0], Symbol('G2'))
        self.assertEqual(retval[1], Null())

        # Calls export.
        forms = Parser.parse("('G2)")
        retval = export(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Chesk return value.
        self.assertEqual(retval, T())

        # Calls find_symbol.
        forms = Parser.parse('("G2")')
        retval = find_symbol(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Chesk return value.
        self.assertEqual(retval[0], Symbol('G2'))
        self.assertEqual(retval[1], Keyword(':EXTERNAL'))


class InPackageSystemFunctionUnitTestCase(unittest.TestCase):
    def testInPackageSystemFunction(self):
        # Makes an instance of InPackageSystemFunction.
        in_package = InPackageSystemFunction()

        # Checks official representation.
        self.assertRegex(str(in_package), r"#<SYSTEM-FUNCTION IN-PACKAGE \{[0-9A-Z]+\}>")

    def testInPackcageSytemFunction_call(self):
        # Makes an instance of InPackageSystemFunction.
        in_package = InPackageSystemFunction()

        # Checks current package.
        self.assertEqual(str(PackageManager.current_package), '#<PACKAGE COMMON-LISP>')

        # Calls in_pakcage.
        forms = Parser.parse('("COMMON-LISP-USER")')
        retval = in_package(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks return value
        self.assertEqual(str(retval), '#<PACKAGE COMMON-LISP-USER>')

        # Checks current package.
        self.assertEqual(str(PackageManager.current_package), '#<PACKAGE COMMON-LISP-USER>')


class UsePackageSystemFunctionUnitTestCase(unittest.TestCase):
    def testUsePackageSystemFunction(self):
        # Makes an instance of UsePackageSystemFunction.
        use_package = UsePackageSystemFunction()

        # Checks official representation.
        self.assertRegex(str(use_package), r"#<SYSTEM-FUNCTION USE-PACKAGE \{[0-9A-Z]+\}>")

    def testUsePackageSystemFunction_call(self):
        # Makes an instance of UsePackageSystemFunction.
        use_package = UsePackageSystemFunction()

        # Calls use_package.
        forms = Parser.parse("('COMMON-LISP 'COMMON-LISP-USER)")
        retval = use_package(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks return value.
        self.assertEqual(retval, T())


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
        self.assertEqual(retval, Integer(6))


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
        self.assertEqual(retval, Integer(-4))


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
        self.assertEqual(retval, Integer(6))


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
        self.assertEqual(retval, Ratio('1/6'))


class NumericalEqualSystemFunctionUnitTestCase(unittest.TestCase):
    def testNumericalEqualSystemFunction(self):
        # Makes an instance of NumericalEqualSystemFunction.
        numerical_equal = NumericalEqualSystemFunction()

        # Checks official representation.
        self.assertRegex(str(numerical_equal), r"#<SYSTEM-FUNCTION = \{[0-9A-Z]+\}>")

    def testNumericalEqualSystemFunction_call_true_case(self):
        # Makes an instance of NumericalEqualSystemFunction.
        numerical_equal = NumericalEqualSystemFunction()

        # Calls numerical_euqal, true case.
        forms = Parser.parse('(1 1 1 1 1)')
        retval = numerical_equal(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks return value.
        self.assertEqual(retval, T())

    def testNumericalEqualSystemFunction_call_false_case(self):
        # Makes an instance of NumericalEqualSystemFunction.
        numerical_equal = NumericalEqualSystemFunction()

        # Calls numerical_euqal, false case.
        forms = Parser.parse('(1 2 3 4 5)')
        retval = numerical_equal(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks return value.
        self.assertEqual(retval, Null())


class NumericalNotEqualSystemFunctionUnitTestCase(unittest.TestCase):
    def testNumericalEqualSystemFunction(self):
        """Makes an instance of NumericalNotEqualSystemFunction.
        """
        numerical_not_equal = NumericalNotEqualSystemFunction()

        # Checks official representation.
        self.assertRegex(str(numerical_not_equal), r"#<SYSTEM-FUNCTION /= \{[0-9A-Z]+\}>")

    def testNumericalNotEqualSystemFunction_call_true_case(self):
        """Makes an instance of NumeciralNotEqualSystemFunction.
        """
        numerical_not_equal = NumericalNotEqualSystemFunction()

        # Calls numerical_not_euqal, true case.
        forms = Parser.parse('(1 2 3 4 5)')
        retval = numerical_not_equal(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks return value.
        self.assertEqual(retval, T())

    def testNumericalNotEqualSystemFunction_call_false_case(self):
        """Makes an instance of NumeciralNotEqualSystemFunction.
        """
        numerical_not_equal = NumericalNotEqualSystemFunction()

        # Calls numerical_not_euqal, true case.
        forms = Parser.parse('(1 1 1 1 1 1)')
        retval = numerical_not_equal(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks return value.
        self.assertEqual(retval, Null())


class LessThanSystemFunctionUnitTestCase(unittest.TestCase):
    def testLessThanSystemFunction(self):
        """Makes an instance of LessThanSystemFunction.
        """
        less_than = LessThanSystemFunction()

        # Checks official representation.
        self.assertRegex(str(less_than), r"#<SYSTEM-FUNCTION < \{[0-9A-Z]+\}>")

    def testLessThanSystemFunction_call_true_case(self):
        """Makes an instance of LessThanSystemFunction.
        """
        less_than = LessThanSystemFunction()

        # Calls less_than, true case.
        forms = Parser.parse('(1 2 3 4 5)')
        retval = less_than(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks return value.
        self.assertEqual(retval, T())

    def testLessThanSystemFunction_call_false_case(self):
        """Makes an instance of LessThanSystemFunction.
        """
        less_than = LessThanSystemFunction()

        # Calls less_than, false case.
        forms = Parser.parse('(1 1 3 4 5)')
        retval = less_than(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks return value.
        self.assertEqual(retval, Null())


class GreaterThanSystemFunctionUnitTestCase(unittest.TestCase):
    def testGeraterThanSystemFunction(self):
        """Makes an instance of GreaterThanSystemFunction.
        """
        greater_than = GreaterThanSystemFunction()

        # Checks official representation.
        self.assertRegex(str(greater_than), r"#<SYSTEM-FUNCTION > \{[0-9A-Z]+\}>")

    def testGreaterThanSystemFunction_call_true_case(self):
        """Makes an instance of GreaterThanSystemFunction.
        """
        greater_than = GreaterThanSystemFunction()

        # Calls greater_than, true case.
        forms = Parser.parse('(5 4 3 2 1)')
        retval = greater_than(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks return value.
        self.assertEqual(retval, T())

    def testGreaterThanSystemFunction_call_false_case(self):
        """Makes an instance of GreaterThanSystemFunction.
        """
        greater_than = GreaterThanSystemFunction()

        # Calls greater_than, false case.
        forms = Parser.parse('(5 5 3 2 1)')
        retval = greater_than(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks return value.
        self.assertEqual(retval, Null())

class LessThanEqualSystemFunctionUnitTestCase(unittest.TestCase):
    def testLessThanEqualSystemFunction(self):
        """Makes an instance of LessThanEqualSystemFunction.
        """
        less_than_equal = LessThanEqualSystemFunction()

        # Checks official representation.
        self.assertRegex(str(less_than_equal), r"#<SYSTEM-FUNCTION <= \{[0-9A-Z]+\}>")

    def testLessThanEqualSystemFunction_call_true_case(self):
        """Makes an instance of LessThanEqualSystemFunction.
        """
        less_than_equal = LessThanEqualSystemFunction()

        # Calls less_than_equal, true case.
        forms = Parser.parse('(1 1 3 4 5)')
        retval = less_than_equal(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks return value.
        self.assertEqual(retval, T())

    def testLessThanEqualSystemFunction_call_false_case(self):
        """Makes an instance of LessThanEqualSystemFunction.
        """
        less_than_equal = LessThanEqualSystemFunction()

        # Calls less_than_equal, false case.
        forms = Parser.parse('(1 2 1 4 5)')
        retval = less_than_equal(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks return value.
        self.assertEqual(retval, Null())


class GreaterThanEqualSystemFunctionUnitTestCase(unittest.TestCase):
    def testGeraterThanEqualSystemFunction(self):
        """Makes an instance of GreaterThanEqualSystemFunction.
        """
        greater_than_equal = GreaterThanEqualSystemFunction()

        # Checks official representation.
        self.assertRegex(str(greater_than_equal), r"#<SYSTEM-FUNCTION >= \{[0-9A-Z]+\}>")

    def testGreaterThanEqualSystemFunction_call_true_case(self):
        """Makes an instance of GreaterThanEqualSystemFunction.
        """
        greater_than_equal = GreaterThanEqualSystemFunction()

        # Calls greater_than_equal, true case.
        forms = Parser.parse('(5 5 3 2 1)')
        retval = greater_than_equal(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks return value.
        self.assertEqual(retval, T())

    def testGreaterThanEqualSystemFunction_call_false_case(self):
        """Makes an instance of GreaterThanEqualSystemFunction.
        """
        greater_than_equal = GreaterThanEqualSystemFunction()

        # Calls greater_than_equal, false case.
        forms = Parser.parse('(5 4 5 2 1)')
        retval = greater_than_equal(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks return value.
        self.assertEqual(retval, Null())


class QuitSystemFunctionUnitTestCase(unittest.TestCase):
    def testQuitSystemFunction(self):
        # Makes an instance of QuitSystemFunction.
        quit_ = QuitSystemFunction()

        # Checks official representation.
        self.assertRegex(str(quit_), r"#<SYSTEM-FUNCTION QUIT \{[0-9A-Z]+\}>")

    def testQuitSystemFunction_call(self):
        # Import Interrupt exception class form clispy.interpreter.
        from clispy.interpreter import Interrupt

        # Makes an instance of QuitSystemFunction.
        quit_ = QuitSystemFunction()

        # Checks raise exception.
        with self.assertRaises(Interrupt):
            quit_(
                Parser.parse('()'),
                PackageManager.current_package.env['VARIABLE'],
                PackageManager.current_package.env['FUNCTION'],
                PackageManager.current_package.env['MACRO']
            )
