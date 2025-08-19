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
from clispy.macro.system_macro import *
from clispy.package import PackageManager
from clispy.parser import Parser


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
