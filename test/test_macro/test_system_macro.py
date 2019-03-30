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
from clispy.macro.system_macro import *
from clispy.package import PackageManager
from clispy.parser import Parser


class SystemMacroUnitTestCase(unittest.TestCase):
    def testSystemMacro(self):
        # Makes an instance of SystemMacro.
        macro = SystemMacro()

        # Checks official representation.
        self.assertRegex(str(macro), r"#<SYSTEM-MACRO SYSTEM-MACRO \{[0-9A-Z]+\}>")


class BlockSystemMacroUnitTestCase(unittest.TestCase):
    def testBlockSystemMacro(self):
        # Makes an instance of BlockSystemMacro.
        macro = BlockSystemMacro()

        # Checks official representation.
        self.assertRegex(str(macro), r"#<SYSTEM-MACRO BLOCK \{[0-9A-Z]+\}>")

    def testBlockSystemMacro_call(self):
        # Makes an instance of BlockSystemMacro.
        macro = BlockSystemMacro()

        # Checks official representation.
        forms = Parser.parse('(NAME (+ 1 2 3))')
        forms = macro(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks expanded forms.
        self.assertEqual(str(forms), '(BLOCK NAME (PROGN (+ 1 2 3)))')


class FletSystemMacroUnitTestCase(unittest.TestCase):
    def testFletSystemMacro(self):
        # Makes an instance of FletSystemMacro.
        macro = FletSystemMacro()

        # Checks official representation.
        self.assertRegex(str(macro), r"#<SYSTEM-MACRO FLET \{[0-9A-Z]+\}>")

    def testFletSystemMacro_call(self):
        # Makes an instance of FletSystemMacro.
        macro = FletSystemMacro()

        # Checks official representation.
        forms = Parser.parse('(((TEST (X) (* X X X))) (TEST 10))')
        forms = macro(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks expanded forms.
        self.assertEqual(str(forms), '(FLET ((TEST (X) (* X X X))) (PROGN (TEST 10)))')


class IfSystemMacroUnitTestCase(unittest.TestCase):
    def testIfSystemMacro(self):
        # Makes an instance of IfSystemMacro.
        macro = IfSystemMacro()

        # Checks official representation.
        self.assertRegex(str(macro), r"#<SYSTEM-MACRO IF \{[0-9A-Z]+\}>")

    def testIfSystemMacro_call(self):
        # Makes an instance of IfSystemMacro.
        macro = IfSystemMacro()

        # Checks official representation.
        self.assertRegex(str(macro), r"#<SYSTEM-MACRO IF \{[0-9A-Z]+\}>")

        # Checks official representation.
        forms = Parser.parse('((= 1 2) 3)')
        forms = macro(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks expanded forms.
        self.assertEqual(str(forms), '(IF (= 1 2) 3 NIL)')


class LabelsSystemMacroUnitTestCase(unittest.TestCase):
    def testLabelsSystemMacro(self):
        # Makes an instance of LabelsSystemMacro.
        macro = LabelsSystemMacro()

        # Checks official representation.
        self.assertRegex(str(macro), r"#<SYSTEM-MACRO LABELS \{[0-9A-Z]+\}>")

    def testLabelsSystemMacro_call(self):
        # Makes an instance of LabelsSystemMacro.
        macro = LabelsSystemMacro()

        # Checks official representation.
        forms = Parser.parse('(((TEST (X) (* X X X))) (TEST 10))')
        forms = macro(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks expanded forms.
        self.assertEqual(str(forms), '(LABELS ((TEST (X) (* X X X))) (PROGN (TEST 10)))')


class LetSystemMacroUnitTestCase(unittest.TestCase):
    def testLetSystemMacro(self):
        # Makes an instance of LetSystemMacro.
        macro = LetSystemMacro()

        # Checks official representation.
        self.assertRegex(str(macro), r"#<SYSTEM-MACRO LET \{[0-9A-Z]+\}>")

    def testLetSystemMacro_call(self):
        # Makes an instance of LetSystemMacro.
        macro = LetSystemMacro()

        # Checks official representation.
        forms = Parser.parse('(((TEST 10)) (CONS TEST NIL))')
        forms = macro(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks expanded forms.
        self.assertEqual(str(forms), '(LET ((TEST 10)) (PROGN (CONS TEST NIL)))')


class LetAsterSystemMacroUnitTestCase(unittest.TestCase):
    def testLetAsterSystemMacro(self):
        # Makes an instance of LetAsterSystemMacro.
        macro = LetAsterSystemMacro()

        # Checks official representation.
        self.assertRegex(str(macro), r"#<SYSTEM-MACRO LET\* \{[0-9A-Z]+\}>")

    def testLetAsterSystemMacro_call(self):
        # Makes an instance of LetAsterSystemMacro.
        macro = LetAsterSystemMacro()

        # Checks official representation.
        forms = Parser.parse('(((TEST 10)) (CONS TEST NIL))')
        forms = macro(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks expanded forms.
        self.assertEqual(str(forms), '(LET* ((TEST 10)) (PROGN (CONS TEST NIL)))')


class QuoteSystemMacroUnitTestCase(unittest.TestCase):
    def testQuoteSystemMacro(self):
        # Makes an instance of QuoteSystemMacro.
        macro = QuoteSystemMacro()

        # Checks official representation.
        self.assertRegex(str(macro), r"#<SYSTEM-MACRO QUOTE \{[0-9A-Z]+\}>")

    def testQuoteSystemMacro_call(self):
        # Makes an instance of QuoteSystemMacro.
        macro = QuoteSystemMacro()

        # Checks official representation.
        forms = Parser.parse('(A)')
        forms = macro(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks expanded forms.
        self.assertEqual(str(forms), '(QUOTE A)')


class LambdaSystemMacroUnitTestCase(unittest.TestCase):
    def testLambdaSystemMacro(self):
        # Makes an instance of LambdaSystemMacro.
        macro = LambdaSystemMacro()

        # Checks official representation.
        self.assertRegex(str(macro), r"#<SYSTEM-MACRO LAMBDA \{[0-9A-Z]+\}>")

    def testLambdaSystemMacro_call(self):
        # Makes an instance of LambdaSystemMacro.
        macro = LambdaSystemMacro()

        # Checks official representation.
        forms = Parser.parse('((X) (* X X X))')
        forms = macro(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks expanded forms.
        self.assertEqual(str(forms), '(LAMBDA (X) (PROGN (* X X X)))')


class DefunSystemMacroUnitTestCase(unittest.TestCase):
    def testDefunSystemMacro(self):
        # Makes an instance of DefunSystemMacro.
        macro = DefunSystemMacro()

        # Checks official representation.
        self.assertRegex(str(macro), r"#<SYSTEM-MACRO DEFUN \{[0-9A-Z]+\}>")

    def testDefunSystemMacro_call(self):
        # Makes an instance of DefunSystemMacro.
        macro = DefunSystemMacro()

        # Checks official representation.
        forms = Parser.parse('(NAME (X) (* X X X))')
        forms = macro(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks expanded forms.
        self.assertEqual(str(forms), '(DEFUN NAME (X) (BLOCK NAME (PROGN (* X X X))))')


class DefmacroSystemMacroUnitTestCase(unittest.TestCase):
    def testDefmacroSystemMacro(self):
        # Makes an instance of DefmacroSystemMacro.
        macro = DefmacroSystemMacro()

        # Checks official representation.
        self.assertRegex(str(macro), r"#<SYSTEM-MACRO DEFMACRO \{[0-9A-Z]+\}>")

    def testDefmacroSystemMacro_call(self):
        # Makes an instance of DefunSystemMacro.
        macro = DefmacroSystemMacro()

        # Checks official representation.
        forms = Parser.parse('(NAME (X) (* X X X))')
        forms = macro(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks expanded forms.
        self.assertEqual(str(forms), '(DEFMACRO NAME (X) (BLOCK NAME (PROGN (* X X X))))')


class BackquoteSystemMacroUnitTestCase(unittest.TestCase):
    def testBackquoteSystemMacro(self):
        # Makes an instance of BackquoteSystemMacro.
        macro = BackquoteSystemMacro()

        # Checks official representation.
        self.assertRegex(str(macro), r"#<SYSTEM-MACRO BACKQUOTE \{[0-9A-Z]+\}>")

    def testBackquoteSystemMacro_call(self):
        # Makes an instance of DefunSystemMacro.
        macro = BackquoteSystemMacro()

        # Checks official representation.
        forms = Parser.parse('(((UNQUOTE X) (UNQUOTE-SPLICING Y)))')
        forms = macro(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )
        # Checks expanded forms.
        self.assertEqual(str(forms), '(CONS X (APPEND Y (QUOTE NIL)))')
