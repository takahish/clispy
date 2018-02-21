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
from clispy import parse
from clispy import symbol


class UnitTestCase(unittest.TestCase):
    def testInPort(self):
        inport = parse._InPort(io.StringIO('(list 2 3 "string")'))

        self.assertIsInstance(inport, parse._InPort)

        # tokens
        self.assertEqual(inport.next_token(), '(')
        self.assertEqual(inport.next_token(), 'list')
        self.assertEqual(inport.next_token(), '2')
        self.assertEqual(inport.next_token(), '3')
        self.assertEqual(inport.next_token(), '"string"')
        self.assertEqual(inport.next_token(), ')')

        # #<eof-object>
        self.assertIsInstance(inport.next_token(), symbol.Symbol)
        self.assertEqual(inport.next_token(), symbol.EOF_OBJECT)

    def test_atom(self):
        self.assertTrue(parse._convert_to_atom('t'))
        self.assertFalse(parse._convert_to_atom('nil'))
        self.assertEqual(parse._convert_to_atom('"string"'), 'string')

        self.assertIsInstance(parse._convert_to_atom('2'), int)
        self.assertEqual(parse._convert_to_atom('2'), 2)
        self.assertIsInstance(parse._convert_to_atom('2.3'), float)
        self.assertEqual(parse._convert_to_atom('2.3'), 2.3)
        self.assertIsInstance(parse._convert_to_atom('2+3i'), complex)
        self.assertEqual(parse._convert_to_atom('2+3i'), 2 + 3j)
        self.assertIsInstance(parse._convert_to_atom('sym'), symbol.Symbol)
        
    def test_read_ahead(self):
        # success to tokenize
        inport = parse._InPort(io.StringIO('(list 2 3 "string")'))
        token = inport.next_token()
        self.assertEqual(parse._read_ahead(token, inport), ['LIST', 2, 3, "string"])

        # quote
        inport = parse._InPort(io.StringIO("'(+ 2 3)"))
        token = inport.next_token()
        self.assertEqual(parse._read_ahead(token, inport), [symbol.QUOTE, ['+', 2, 3]])

        # atom
        inport = parse._InPort(io.StringIO('+'))
        token = inport.next_token()
        self.assertEqual(parse._read_ahead(token, inport), symbol.Symbol('+'))

        # fail to tokenize
        inport = parse._InPort(io.StringIO(')'))
        token = inport.next_token()
        self.assertRaisesRegex(SyntaxError, "unexpected \)", parse._read_ahead, token, inport)

        # fail to tokenize
        inport = parse._InPort(io.StringIO('(+ 2 3'))
        token = inport.next_token()
        self.assertRaisesRegex(SyntaxError, "unexpected EOF in list", parse._read_ahead, token, inport)

    def test_read(self):
        # test only EOF
        inport = parse._InPort(io.StringIO(''))
        eof = parse._read(inport)
        self.assertIsInstance(eof, symbol.Symbol)
        self.assertEqual(eof, symbol.EOF_OBJECT)

    def test_parse(self):
        inport = '(+ 2 3)'
        self.assertEqual(parse.parse(inport), ['+', 2, 3])

        inport = parse._InPort(io.StringIO('(+ 2 3)'))
        self.assertEqual(parse.parse(inport), ['+', 2, 3])

    def test_readchar(self):
        inport = parse._InPort(io.StringIO('a'))
        self.assertEqual(parse._readchar(inport), 'a')
        self.assertEqual(parse._readchar(inport), symbol.EOF_OBJECT)

