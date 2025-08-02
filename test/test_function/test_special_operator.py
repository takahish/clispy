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
from clispy.function.special_operator import (
    LetSpecialOperator,
    BlockSpecialOperator,
    TagbodySpecialOperator,
)
from clispy.package import PackageManager
from clispy.parser import Parser
from clispy.type import Integer, Null


class LetSpecialOperatorUnitTestCase(unittest.TestCase):
    def testLetSpecialOperator(self):
        let_op = LetSpecialOperator()

        # Checks official representation.
        self.assertRegex(str(let_op), r"#<SPECIAL-OPERATOR LET \{[0-9A-Z]+\}>")

    def testLetSpecialOperator_call(self):
        let_op = LetSpecialOperator()

        # Evaluates a LET form
        forms = Parser.parse('(((x 1) (y 2)) (+ x y))')
        retval = let_op(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO'],
        )

        self.assertEqual(retval, Integer(3))


class BlockSpecialOperatorUnitTestCase(unittest.TestCase):
    def testBlockSpecialOperator(self):
        block_op = BlockSpecialOperator()

        # Checks official representation.
        self.assertRegex(str(block_op), r"#<SPECIAL-OPERATOR BLOCK \{[0-9A-Z]+\}>")

    def testBlockSpecialOperator_call(self):
        block_op = BlockSpecialOperator()

        # Evaluates a BLOCK form without return-from
        forms = Parser.parse('(TEST (+ 1 2))')
        retval = block_op(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO'],
        )

        self.assertEqual(retval, Integer(3))

    def testBlockSpecialOperator_return_from(self):
        block_op = BlockSpecialOperator()

        # Evaluates a BLOCK form that exits via RETURN-FROM
        forms = Parser.parse('(TEST (RETURN-FROM TEST 5) 1)')
        retval = block_op(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO'],
        )

        self.assertEqual(retval, Integer(5))


class TagbodySpecialOperatorUnitTestCase(unittest.TestCase):
    def testTagbodySpecialOperator_go(self):
        tagbody_op = TagbodySpecialOperator()

        # Execute a TAGBODY form containing GO statements
        forms = Parser.parse('(1 (SETQ X 100) (GO 3) 2 (SETQ X 200) 3 (SETQ X 300))')

        retval = tagbody_op(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO'],
        )

        self.assertEqual(
            PackageManager.current_package.env['VARIABLE'].find('X')['X'],
            Integer(300),
        )
        # Tagbody returns NIL
        self.assertTrue(retval is Null())
