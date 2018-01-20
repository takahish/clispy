from unittest import TestCase
from lispy.type import Symbol
from lispy.parser import tokenize, atom, read_from_tokens

class UnitTestCase(TestCase):
    def test_tokenize(self):
        program = '(+ 5 (- 4 3))'
        expectation = ['(', '+', '5', '(', '-', '4', '3', ')', ')']
        tokens = tokenize(program)
        self.assertEqual(expectation, tokens)

    def test_atom(self):
        self.assertIsInstance(atom('5'), int)
        self.assertIsInstance(atom('5.5'), float)
        self.assertIsInstance(atom('+'), Symbol)

    def test_read_from_tokens(self):
        self.assertRaises(SyntaxError, read_from_tokens, [])
        self.assertRaises(SyntaxError, read_from_tokens, [')'])
        
        self.assertEqual(read_from_tokens(['3']), 3)
        
        tokens = ['(', '+', '5', '5.5', ')']
        expectation = ['+', 5, 5.5]
        python_tokens = read_from_tokens(tokens)
        self.assertEqual(expectation, python_tokens)
