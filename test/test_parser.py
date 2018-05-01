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
import io
from clispy.symbol import *
from clispy.parser import Parser, InPort


class UnitTestCase(unittest.TestCase):
    def setUp(self):
        self.parser = Parser()

    def testInPort(self):
        inport = InPort(io.StringIO('(list 2 3 "string")'))

        self.assertIsInstance(inport, InPort)

        # tokens
        self.assertEqual(inport.next_token(), '(')
        self.assertEqual(inport.next_token(), 'list')
        self.assertEqual(inport.next_token(), '2')
        self.assertEqual(inport.next_token(), '3')
        self.assertEqual(inport.next_token(), '"string"')
        self.assertEqual(inport.next_token(), ')')

        # #<eof-object>
        self.assertIsInstance(inport.next_token(), Symbol)
        self.assertEqual(inport.next_token(), EOF_OBJECT)

    def test_atom(self):
        self.assertTrue(self.parser._Parser__convert_to_atom('t'))
        self.assertFalse(self.parser._Parser__convert_to_atom('nil'))
        self.assertEqual(self.parser._Parser__convert_to_atom('"string"'), 'string')

        self.assertIsInstance(self.parser._Parser__convert_to_atom('2'), int)
        self.assertEqual(self.parser._Parser__convert_to_atom('2'), 2)
        self.assertIsInstance(self.parser._Parser__convert_to_atom('2.3'), float)
        self.assertEqual(self.parser._Parser__convert_to_atom('2.3'), 2.3)
        self.assertIsInstance(self.parser._Parser__convert_to_atom('sym'), Symbol)
        
    def test_read_ahead(self):
        # success to tokenize
        inport = InPort(io.StringIO('(list 2 3 "string")'))
        token = inport.next_token()
        self.assertEqual(self.parser._Parser__read_ahead(token, inport), ['LIST', 2, 3, "string"])

        # quote
        inport = InPort(io.StringIO("'(+ 2 3)"))
        token = inport.next_token()
        self.assertEqual(self.parser._Parser__read_ahead(token, inport), [QUOTE, ['+', 2, 3]])

        # atom
        inport = InPort(io.StringIO('+'))
        token = inport.next_token()
        self.assertEqual(self.parser._Parser__read_ahead(token, inport), Symbol('+'))

        # fail to tokenize
        inport = InPort(io.StringIO(')'))
        token = inport.next_token()
        self.assertRaisesRegex(SyntaxError, "unexpected \)", self.parser._Parser__read_ahead, token, inport)

        # fail to tokenize
        inport = InPort(io.StringIO('(+ 2 3'))
        token = inport.next_token()
        self.assertRaisesRegex(SyntaxError, "unexpected EOF in list", self.parser._Parser__read_ahead, token, inport)

    def test_read(self):
        # test only EOF
        inport = InPort(io.StringIO(''))
        eof = self.parser._Parser__read(inport)
        self.assertIsInstance(eof, Symbol)
        self.assertEqual(eof, EOF_OBJECT)

    def test_parse(self):
        inport = '(+ 2 3)'
        self.assertEqual(self.parser.parse(inport), ['+', 2, 3])

        inport = InPort(io.StringIO('(+ 2 3)'))
        self.assertEqual(self.parser.parse(inport), ['+', 2, 3])

    def test_readchar(self):
        inport = InPort(io.StringIO('a'))
        self.assertEqual(self.parser._Parser__readchar(inport), 'a')
        self.assertEqual(self.parser._Parser__readchar(inport), EOF_OBJECT)

