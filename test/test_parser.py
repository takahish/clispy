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
import numpy as np
from clispy.type import *
from clispy.parser import Parser, InPort


class UnitTestCase(unittest.TestCase):
    def testInPort(self):
        in_port = InPort(io.StringIO('(list 1 2.3 "string")'))

        self.assertIsInstance(in_port, InPort)

        # tokens
        self.assertEqual(in_port.next_token(), '(')
        self.assertEqual(in_port.next_token(), 'list')
        self.assertEqual(in_port.next_token(), '1')
        self.assertEqual(in_port.next_token(), '2.3')
        self.assertEqual(in_port.next_token(), '"string"')
        self.assertEqual(in_port.next_token(), ')')

        # #<eof-object>
        self.assertIsInstance(in_port.next_token(), Symbol)
        self.assertTrue(in_port.next_token() is Symbol('#<EOF-OJBECT>'))

    def test_atom(self):
        self.assertIsInstance(Parser._convert_to_atom('"string"'), String)
        self.assertIsInstance(Parser._convert_to_atom('1'), Integer)
        self.assertIsInstance(Parser._convert_to_atom('2.3'), SingleFloat)
        self.assertIsInstance(Parser._convert_to_atom('symbol'), Symbol)

        # check values
        self.assertEqual(Parser._convert_to_atom('"string"').value, 'string')
        self.assertEqual(Parser._convert_to_atom('1').value, np.int(1))
        self.assertEqual(Parser._convert_to_atom('2.3').value, np.float32(2.3))
        self.assertEqual(Parser._convert_to_atom('symbol').value, 'SYMBOL')

    def test_tokenize_atom(self):
        inport = InPort(io.StringIO('+'))
        token = inport.next_token()
        token_list = Parser._read_ahead(token, inport)
        self.assertIsInstance(token_list, Symbol)

    def test_tokenize(self):
        in_port = InPort(io.StringIO('(list 1 2.3 "string")'))
        token = in_port.next_token()
        token_list = Parser._read_ahead(token, in_port)

        self.assertIsInstance(token_list[0], Symbol)
        self.assertIsInstance(token_list[1], Integer)
        self.assertIsInstance(token_list[2], SingleFloat)
        self.assertIsInstance(token_list[3], String)

    def test_tokenize_unexpected_closed_parentheses(self):
        # fail to tokenize
        in_port = InPort(io.StringIO(')'))
        token = in_port.next_token()

        self.assertRaisesRegex(SyntaxError, "unexpected \)", Parser._read_ahead, token, in_port)

    def test_tokenize_unclosed_parentheses(self):
        # fail to tokenize
        in_port = InPort(io.StringIO('(+ 1 2.3'))
        token = in_port.next_token()

        self.assertRaisesRegex(SyntaxError, "unexpected EOF in list", Parser._read_ahead, token, in_port)

    def test_tokenize_quote(self):
        in_port = InPort(io.StringIO("'(+ 1 2.3)"))
        token = in_port.next_token()
        token_list = Parser._read_ahead(token, in_port)

        # token_list must be [QUOTE, [+, 1, 2.3]]

        self.assertIsInstance(token_list[0], Symbol)
        self.assertTrue(token_list[0] is Symbol('QUOTE'))

        self.assertIsInstance(token_list[1][0], Symbol)
        self.assertIsInstance(token_list[1][1], Integer)
        self.assertIsInstance(token_list[1][2], SingleFloat)

    def test_tokenize_quasiquote(self):
        in_port = InPort(io.StringIO("`(+ 1 2.3)"))
        token = in_port.next_token()
        token_list = Parser._read_ahead(token, in_port)

        # token_list must be [QUASIQUOTE, [+, 1, 2.3]]

        self.assertIsInstance(token_list[0], Symbol)
        self.assertTrue(token_list[0] is Symbol('QUASIQUOTE'))

        self.assertIsInstance(token_list[1][0], Symbol)
        self.assertIsInstance(token_list[1][1], Integer)
        self.assertIsInstance(token_list[1][2], SingleFloat)

    def test_tokenize_unquote(self):
        in_port = InPort(io.StringIO("`(+ 1 ,(- 2 3))"))
        token = in_port.next_token()
        token_list = Parser._read_ahead(token, in_port)

        # token_list must be [QUASIQUOTE, [+, 1, [UNQUOTE, [-, 2, 3]]]]

        self.assertIsInstance(token_list[0], Symbol)
        self.assertTrue(token_list[0] is Symbol('QUASIQUOTE'))

        self.assertIsInstance(token_list[1][0], Symbol)
        self.assertIsInstance(token_list[1][1], Integer)

        self.assertIsInstance(token_list[1][2][0], Symbol)
        self.assertTrue(token_list[1][2][0] is Symbol('UNQUOTE'))

        self.assertIsInstance(token_list[1][2][1][0], Symbol)
        self.assertIsInstance(token_list[1][2][1][1], Integer)
        self.assertIsInstance(token_list[1][2][1][2], Integer)

    def test_tokenize_unquote_splicing(self):
        in_port = InPort(io.StringIO("`(+ 1 ,@(- 2 3))"))
        token = in_port.next_token()
        token_list = Parser._read_ahead(token, in_port)

        # token_list must be [QUASIQUOTE, [+, 1, [UNQUOTE-SPLICING, [-, 2, 3]]]]

        self.assertIsInstance(token_list[0], Symbol)
        self.assertTrue(token_list[0] is Symbol('QUASIQUOTE'))

        self.assertIsInstance(token_list[1][0], Symbol)
        self.assertIsInstance(token_list[1][1], Integer)

        self.assertIsInstance(token_list[1][2][0], Symbol)
        self.assertTrue(token_list[1][2][0] is Symbol('UNQUOTE-SPLICING'))

        self.assertIsInstance(token_list[1][2][1][0], Symbol)
        self.assertIsInstance(token_list[1][2][1][1], Integer)
        self.assertIsInstance(token_list[1][2][1][2], Integer)

    def test_tokenize_sharpquote(self):
        in_port = InPort(io.StringIO("#'+"))
        token = in_port.next_token()
        token_list = Parser._read_ahead(token, in_port)

        # token_list must be [FUNCTION +]

        self.assertIsInstance(token_list[0], Symbol)
        self.assertTrue(token_list[0] is Symbol('FUNCTION'))

        self.assertIsInstance(token_list[1], Symbol)

    def test_read(self):
        # test only EOF
        in_port = InPort(io.StringIO(''))
        eof = Parser._read(in_port)

        self.assertIsInstance(eof, Symbol)
        self.assertEqual(eof, Parser.eof_object)

    def test_parse_from_string(self):
        in_port = '(+ 1 2.3)'
        cons = Parser.parse(in_port)

        self.assertIsInstance(cons, Cons)

    def test_parse_from_stream(self):
        in_port = InPort(io.StringIO('(+ 1 2.3)'))
        cons = Parser.parse(in_port)

        self.assertIsInstance(cons, Cons)

    def test_parse_atom(self):
        in_port = '1'
        atom = Parser.parse(in_port)

        self.assertIsInstance(atom, Integer)

    def test_parse_atom_eof(self):
        in_port = ''
        atom = Parser.parse(in_port)

        self.assertIsInstance(atom, Symbol)
        self.assertTrue(atom, Parser.eof_object)
