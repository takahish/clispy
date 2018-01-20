from unittest import TestCase
from lispy.repl import lisp_str

class UnitTestCase(TestCase):
    def test_lisp_str(self):
        self.assertEqual(lisp_str(3), '3')
        self.assertEqual(lisp_str(['+', 2, 3]), '(+ 2 3)')
