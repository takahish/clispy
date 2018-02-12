import unittest
import io
import env
import symbol
import parser

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
        self.assertTrue(parser._atom('t'))
        self.assertFalse(parser._atom('nil'))
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
        self.assertEqual(parser._read_ahead(token, inport), ['LIST', 2, 3, "string"])

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
        self.assertEqual(parser._to_string(True), 'T')
        self.assertEqual(parser._to_string(False), 'NIL')
        self.assertEqual(parser._to_string(symbol._Symbol('x')), 'x')
        self.assertEqual(parser._to_string("string"), '"string"')
        self.assertEqual(parser._to_string([1, 2, 3]), '(1 2 3)')
        self.assertEqual(parser._to_string(2+3j), '(2+3i)')
        self.assertEqual(parser._to_string(1), '1')

    def test_require(self):
        x = []
        self.assertRaisesRegex(SyntaxError, "() wrong length", parser._require, x, x!=[])

    def test_expand_quasiquote(self):
        _symbol  = symbol._Symbol('symbol')
        _quote   = symbol._quote
        _unquote = symbol._unquote
        _unquote_splicing = symbol._unquote_splicing
        _cons    = symbol._cons
        _append  = symbol._append
        _add     = symbol._Symbol('+')

        self.assertEqual(parser._expand_quasiquote(_symbol), [symbol._quote, _symbol])
        self.assertEqual(parser._expand_quasiquote([_unquote, [_add, 1, 2]]), [_add, 1, 2])
        self.assertEqual(parser._expand_quasiquote([[_unquote_splicing, [1, 2]], 3]),
                         [_append, [1, 2], [_cons, [_quote, 3], [_quote, []]]])

    def test_expand(self):
        _quote = symbol._quote
        _if    = symbol._if
        _defun = symbol._defun
        _progn = symbol._progn
        _lambda     = symbol._lambda
        _defmacro   = symbol._defmacro
        _quasiquote = symbol._quasiquote
        _unquote    = symbol._unquote
        _func  = symbol._Symbol('func')
        _x     = symbol._Symbol('x')
        _y     = symbol._Symbol('y')
        _add   = symbol._Symbol('+')
        _mul   = symbol._Symbol('*')
        _test  = symbol._Symbol('test')

        # constant => unchanged
        self.assertEqual(parser._expand(3), 3)

        # (quote exp)
        self.assertEqual(parser._expand([_quote, [1, 2]]), [_quote, [1, 2]])

        # (if t c) => (if t c False)
        self.assertEqual(parser._expand([_if, True, 2]), [_if, True, 2, False])

        # (defun f (args) body) => (defun f (lambda (args) body))
        self.assertEqual(parser._expand([_defun, _func, [_x], [_mul, _x, _x]]),
                         [_defun, _func, [_lambda, [_x], [_mul, _x, _x]]])

        # (defmacro v proc) => None; add {v: proc} to macro_table
        parser._expand([_defmacro, _test, [_x, _y],
                        [_quasiquote, [_add, [_unquote, _x], [_unquote, _y]]]])
        self.assertTrue(callable(env._macro_env[_test]))

        # (progn) => None
        self.assertEqual(parser._expand([_progn]), None)

        # (lambda (x) e1 e2) => (lambda (x) (begin e1 e2))
        self.assertEqual(parser._expand([_lambda, [_x], [_mul, _x, _x], [_add, _x, _x]]),
                         [_lambda, [_x], [_progn, [_mul, _x, _x], [_add, _x, _x]]])

        # `x => expand_quasiquote(x)
        self.assertEqual(parser._expand([_quasiquote, 3]), [_quote, 3])

        # (m arg...) => macroexpand if m isinstance macro
        self.assertEqual(parser._expand([_test, 1, 2]), [_add, 1, 2])
