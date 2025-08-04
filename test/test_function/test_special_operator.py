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
    CatchSpecialOperator,
    ThrowSpecialOperator,
    UnwindProtectSpecialOperator,
)
from clispy.package import PackageManager
from clispy.parser import Parser
from clispy.type import Integer, Null, T
from clispy.function.system_function import DefunSystemFunction


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


class CatchThrowSpecialOperatorUnitTestCase(unittest.TestCase):
    def test_catch_throw_representation(self):
        catch_op = CatchSpecialOperator()
        throw_op = ThrowSpecialOperator()
        self.assertRegex(str(catch_op), r"#<SPECIAL-OPERATOR CATCH \{[0-9A-Z]+\}>")
        self.assertRegex(str(throw_op), r"#<SPECIAL-OPERATOR THROW \{[0-9A-Z]+\}>")

    def test_catch_throw(self):
        catch_op = CatchSpecialOperator()

        forms = Parser.parse("('dummy-tag 1 2 (throw 'dummy-tag 3) 4)")
        retval = catch_op(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO'],
        )

        self.assertEqual(retval, Integer(3))

    def test_catch_without_throw(self):
        catch_op = CatchSpecialOperator()

        forms = Parser.parse("('dummy-tag 1 2 3 4)")
        retval = catch_op(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO'],
        )

        self.assertEqual(retval, Integer(4))

    def test_catch_with_throw_back(self):
        catch_op = CatchSpecialOperator()

        defun = DefunSystemFunction()
        defun_forms = Parser.parse('(THROW-BACK (TAG) (THROW TAG T))')
        defun(
            defun_forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO'],
        )

        forms = Parser.parse("('dummy-tag (THROW-BACK 'dummy-tag) 2)")
        retval = catch_op(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO'],
        )

        self.assertTrue(retval is T())

    def test_nested_catch(self):
        catch_op = CatchSpecialOperator()

        forms = Parser.parse(
            "('c (FLET ((C1 () (THROW 'c 1))) (PROGN (CATCH 'c (C1) (PRINT 'unreachable)) 2)))"
        )
        retval = catch_op(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO'],
        )

        self.assertEqual(retval, Integer(2))


class UnwindProtectSpecialOperatorUnitTestCase(unittest.TestCase):
    def test_unwind_protect_representation(self):
        unwind_op = UnwindProtectSpecialOperator()
        self.assertRegex(
            str(unwind_op), r"#<SPECIAL-OPERATOR UNWIND-PROTECT \{[0-9A-Z]+\}>"
        )

    def test_unwind_protect_with_return_from(self):
        block_op = BlockSpecialOperator()
        forms = Parser.parse(
            "(NIL (UNWIND-PROTECT (RETURN-FROM NIL 1) (RETURN-FROM NIL 2)))"
        )
        retval = block_op(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO'],
        )

        self.assertEqual(retval, Integer(2))

    def test_unwind_protect_with_throw(self):
        catch_op = CatchSpecialOperator()
        forms = Parser.parse("(NIL (UNWIND-PROTECT (THROW NIL 1) (THROW NIL 2)))")
        retval = catch_op(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO'],
        )

        self.assertEqual(retval, Integer(2))

    def test_unwind_protect_with_go(self):
        tagbody_op = TagbodySpecialOperator()
        forms = Parser.parse(
            "((SETQ X 3) (SETQ Y 0) (UNWIND-PROTECT (GO OUT) (SETQ Y X)) (SETQ Y 99) OUT (SETQ Z Y))"
        )
        tagbody_op(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO'],
        )

        self.assertEqual(
            PackageManager.current_package.env['VARIABLE'].find('Y')['Y'],
            Integer(3),
        )
