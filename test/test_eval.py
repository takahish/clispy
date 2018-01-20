from unittest import TestCase
from lispy.env import _global_env
from lispy.eval import _Procedure, _eval

class UnitTestCase(TestCase):
    def testProcedure(self):
        # (lambda (x) (* x x))
        params = ['x']
        body = ['*', 'x', 'x']
        proc = _Procedure(params, body, _global_env)
        self.assertIsInstance(proc, _Procedure)

    def test_eval(self):
        # Symbol
        self.assertEqual(_eval('round'), round)
        # Atom
        self.assertEqual(_eval(5), 5)
        # standard procedure call
        self.assertEqual(_eval(['+', 3, 4]), 7)
        # quote
        self.assertEqual(_eval(['quote', ['+', 3, 4]]), ['+', 3, 4])
        # if
        self.assertEqual(_eval(['if', ['>', 3, 4], 5, 6]), 6)
        # define
        self.assertEqual(_eval(['define', 'a', ['+', 3, 4]]), 7)
        self.assertIn('a', _global_env) # Added to _global_env
        self.assertEqual(_global_env.find('a')['a'], 7) # Value
        # set!(substitution)
        self.assertEqual(_eval(['set!', 'a', 10]), 10)
        self.assertEqual(_global_env.find('a')['a'], 10)
        # defined procedure
        self.assertIsInstance(_eval(['define', 'func', ['lambda', ['x'], ['*', 'x', 'x']]]), _Procedure)
        self.assertEqual(_eval(['func', 5]), 25)
        self.assertNotIn('x', _global_env) # Not remaining in _global_env