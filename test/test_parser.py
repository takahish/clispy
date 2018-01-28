import unittest
import io
import symbol
import macro
import parser
import ops

class UnitTestCase(unittest.TestCase):
    def testInPort(self):
        inport = parser._InPort(io.StringIO('(list 2 3 "string")'))

        self.assertIsInstance(inport, parser._InPort)

        # tokens
        self.assertEqual(inport.next_token(), '(')
        self.assertEqual(inport.next_token(), 'list')
        self.assertEqual(inport.next_token(), '2')
        self.assertEqual(inport.next_token(), '3')
        self.assertEqual(inport.next_token(), '"string"')
        self.assertEqual(inport.next_token(), ')')

        # #<eof-object>
        self.assertIsInstance(inport.next_token(), symbol._Symbol)
        self.assertEqual(inport.next_token(), symbol._eof_object)

    def test_atom(self):
        self.assertTrue(parser._atom('#t'))
        self.assertFalse(parser._atom('#f'))
        self.assertEqual(parser._atom('"string"'), 'string')

        self.assertIsInstance(parser._atom('2'), int)
        self.assertEqual(parser._atom('2'), 2)
        self.assertIsInstance(parser._atom('2.3'), float)
        self.assertEqual(parser._atom('2.3'), 2.3)
        self.assertIsInstance(parser._atom('2+3i'), complex)
        self.assertEqual(parser._atom('2+3i'), 2+3j)
        self.assertIsInstance(parser._atom('sym'), symbol._Symbol)
        
    def test_read_ahead(self):
        # success to tokenize
        inport = parser._InPort(io.StringIO('(list 2 3 "string")'))
        token = inport.next_token()
        self.assertEqual(parser._read_ahead(token, inport), ['list', 2, 3, "string"])

        # quote
        inport = parser._InPort(io.StringIO("'(+ 2 3)"))
        token = inport.next_token()
        self.assertEqual(parser._read_ahead(token, inport), [symbol._quote, ['+', 2, 3]])

        # atom
        inport = parser._InPort(io.StringIO('+'))
        token = inport.next_token()
        self.assertEqual(parser._read_ahead(token, inport), symbol._Symbol('+'))

        # fail to tokenize
        inport = parser._InPort(io.StringIO(')'))
        token = inport.next_token()
        self.assertRaisesRegex(SyntaxError, "unexpected \)", parser._read_ahead, token, inport)

        # fail to tokenize
        inport = parser._InPort(io.StringIO('(+ 2 3'))
        token = inport.next_token()
        self.assertRaisesRegex(SyntaxError, "unexpected EOF in list", parser._read_ahead, token, inport)

    def test_read(self):
        # test only EOF
        inport = parser._InPort(io.StringIO(''))
        eof = parser._read(inport)
        self.assertIsInstance(eof, symbol._Symbol)
        self.assertEqual(eof, symbol._eof_object)

    def test_parse(self):
        inport = '(+ 2 3)'
        self.assertEqual(parser._parse(inport), ['+', 2, 3])

        inport = parser._InPort(io.StringIO('(+ 2 3)'))
        self.assertEqual(parser._parse(inport), ['+', 2, 3])

    def test_readchar(self):
        inport = parser._InPort(io.StringIO('a'))
        self.assertEqual(parser._readchar(inport), 'a')
        self.assertEqual(parser._readchar(inport), symbol._eof_object)

    def test_to_string(self):
        self.assertEqual(parser._to_string(True), '#t')
        self.assertEqual(parser._to_string(False), '#f')
        self.assertEqual(parser._to_string(symbol._Symbol('x')), 'x')
        self.assertEqual(parser._to_string("string"), '"string"')
        self.assertEqual(parser._to_string([1, 2, 3]), '(1 2 3)')
        self.assertEqual(parser._to_string(2+3j), '(2+3i)')
        self.assertEqual(parser._to_string(1), '1')

    def test_require(self):
        x = []
        self.assertRaisesRegex(SyntaxError, "() wrong length", parser._require, x, x!=[])

    def test_expand_quasiquote(self):
        self.assertEqual(parser._expand_quasiquote(symbol._Symbol('symbol')), [symbol._quote, symbol._Symbol('symbol')])
        self.assertEqual(parser._expand_quasiquote([symbol._unquote, [symbol._Symbol('+'), 1, 2]]), [symbol._Symbol('+'), 1, 2])
        self.assertEqual(parser._expand_quasiquote([[symbol._unquote_splicing, [1, 2]], 3]),
                         [ops._append, [1, 2], [ops._cons, [symbol._quote, 3], [symbol._quote, []]]])

    def test_expand(self):
        # constant => unchanged
        self.assertEqual(parser._expand(3), 3)

        # (quote exp)
        self.assertEqual(parser._expand([symbol._quote, [1, 2]]), [symbol._quote, [1, 2]])

        # (if t c) => (if t c None)
        self.assertEqual(parser._expand([symbol._if, True, 2]), [symbol._if, True, 2, None])

        # (define (f args) body) => (define f (lambda (args) body))
        self.assertEqual(parser._expand([symbol._define, [symbol._Symbol('func'), symbol._Symbol('x')],
                                  [symbol._Symbol('*'), symbol._Symbol('x'), symbol._Symbol('x')]]),
                         [symbol._define, symbol._Symbol('func'), [symbol._lambda, [symbol._Symbol('x')],
                                                     [symbol._Symbol('*'), symbol._Symbol('x'), symbol._Symbol('x')]]])

        # (define-macro v proc) => None; add {v: proc} to macro_table
        parser._expand([symbol._define_macro, symbol._Symbol('add'), symbol._Symbol('+')], top_level=True)
        self.assertTrue(callable(macro._macro_table[symbol._Symbol('add')]))

        # (begin) => None
        self.assertEqual(parser._expand([symbol._begin]), None)

        # (lambda (x) e1 e2) => (lambda (x) (begin e1 e2))
        self.assertEqual(parser._expand([symbol._lambda, [symbol._Symbol('x')],
                                  [symbol._Symbol('*'), symbol._Symbol('x'), symbol._Symbol('x')],
                                  [symbol._Symbol('+'), symbol._Symbol('x'), symbol._Symbol('x')]]),
                         [symbol._lambda, [symbol._Symbol('x')],
                          [symbol._begin,
                           [symbol._Symbol('*'), symbol._Symbol('x'), symbol._Symbol('x')],
                           [symbol._Symbol('+'), symbol._Symbol('x'), symbol._Symbol('x')]]])

        # `x => expand_quasiquote(x)
        self.assertEqual(parser._expand([symbol._quasiquote, 3]), [symbol._quote, 3])

        # (m arg...) => macroexpand if m isinstance macro
        self.assertEqual(parser._expand([symbol._Symbol('add'), 1, 2]), 3)
