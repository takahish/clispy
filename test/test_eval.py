from unittest import TestCase
from lispy.env import _global_env
from lispy.eval import Procedure, eval

class UnitTestCase(TestCase):
    def testProcedure(self):
        # (lambda (x) (* x x))
        params = ['x']
        body = ['*', 'x', 'x']
        proc = Procedure(params, body, _global_env)
        self.assertIsInstance(proc, Procedure)

    def test_eval(self):
        # Symbol
        self.assertEqual(eval('round'), round)
        # Atom
        self.assertEqual(eval(5), 5)
        # standard procedure call
        self.assertEqual(eval(['+', 3, 4]), 7)
        # quote
        self.assertEqual(eval(['quote', ['+', 3, 4]]), ['+', 3, 4])
        # if
        self.assertEqual(eval(['if', ['>', 3, 4], 5, 6]), 6)
        # define
        self.assertEqual(eval(['define', 'a', ['+', 3, 4]]), 7)
        self.assertIn('a', _global_env) # Added to _global_env
        self.assertEqual(_global_env.find('a')['a'], 7) # Value
        # set!(substitution)
        self.assertEqual(eval(['set!', 'a', 10]), 10)
        self.assertEqual(_global_env.find('a')['a'], 10)
        # defined procedure
        self.assertIsInstance(eval(['define', 'func', ['lambda', ['x'], ['*', 'x', 'x']]]), Procedure)
        self.assertEqual(eval(['func', 5]), 25)
        self.assertNotIn('x', _global_env) # Not remaining in _global_env