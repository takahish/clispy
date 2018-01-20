from unittest import TestCase
from lispy.symbol import _Symbol
from lispy.parser import _tokenize, _atom, _read_from_tokens

class UnitTestCase(TestCase):
    def test_tokenize(self):
        program = '(+ 5 (- 4 3))'
        expectation = ['(', '+', '5', '(', '-', '4', '3', ')', ')']
        tokens = _tokenize(program)
        self.assertEqual(expectation, tokens)

    def test_atom(self):
        self.assertIsInstance(_atom('5'), int)
        self.assertIsInstance(_atom('5.5'), float)
        self.assertIsInstance(_atom('+'), _Symbol)

    def test_read_from_tokens(self):
        self.assertRaises(SyntaxError, _read_from_tokens, [])
        self.assertRaises(SyntaxError, _read_from_tokens, [')'])
        
        self.assertEqual(_read_from_tokens(['3']), 3)
        
        tokens = ['(', '+', '5', '5.5', ')']
        expectation = ['+', 5, 5.5]
        python_tokens = _read_from_tokens(tokens)
        self.assertEqual(expectation, python_tokens)
