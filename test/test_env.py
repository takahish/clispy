from unittest import TestCase
from lispy.env import Env, standard_env
import operator as op

class UnitTestCase(TestCase):
    def test_standard_env(self):
        self.assertIsInstance(standard_env(), Env)

    def test_standard_procedures(self):
        env = standard_env()

        # +, -, *, /
        self.assertEqual(env.find('+')['+'](2, 3, 4), 9)
        self.assertEqual(env.find('-')['-'](2, 3, 4), -5)
        self.assertEqual(env.find('*')['*'](2, 3, 4), 24)
        self.assertEqual(env.find('/')['/'](4, 2, 2), 1)

        # >, <, >=, <=, =
        self.assertTrue(env.find('>')['>'](4, 3, 2))
        self.assertFalse(env.find('>')['>'](3, 2, 4))
        self.assertTrue(env.find('<')['<'](2, 3, 4))
        self.assertFalse(env.find('<')['<'](3, 4, 2))
        self.assertTrue(env.find('>=')['>='](3, 3, 2))
        self.assertFalse(env.find('>=')['>='](3, 2, 3))
        self.assertTrue(env.find('<=')['<='](3, 3, 4))
        self.assertFalse(env.find('<=')['<='](3, 4, 3))
        self.assertTrue(env.find('=')['='](2, 2, 2))
        self.assertFalse(env.find('=')['='](2, 2, 3))

        # abs, round
        self.assertEqual(env.find('abs')['abs'](-3), 3)
        self.assertEqual(env.find('round')['round'](3.4), 3)
        self.assertEqual(env.find('round')['round'](3.5), 4)

        # append
        self.assertEqual(env.find('append')['append']([2, 3], [4], [5, 6]), [2, 3, 4, 5, 6])

        # apply
        self.assertEqual(env.find('apply')['apply'](op.add, [2, 3]), 5)

        # begin
        self.assertEqual(env.find('begin')['begin'](2, 3, 4), 4)

        # car, cdr, cons
        self.assertEqual(env.find('car')['car']([2, 3, 4]), 2)
        self.assertEqual(env.find('cdr')['cdr']([2, 3, 4]), [3, 4])
        self.assertEqual(env.find('cons')['cons'](2, [3, 4]), [2, 3, 4])

        # eq?, equal?
        self.assertTrue(env.find('eq?')['eq?'](2, 2))
        self.assertFalse(env.find('eq?')['eq?']([2, 3], [2, 3]))
        self.assertTrue(env.find('equal?')['equal?']([2, 3], [2, 3]))

        # length, list, list?
        self.assertEqual(env.find('length')['length']([2, 3, 4]), 3)
        self.assertEqual(env.find('list')['list'](2, 3, 4), [2, 3, 4])
        self.assertTrue(env.find('list?')['list?']([2, 3, 4]))

        # map
        self.assertEqual(env.find('map')['map'](abs, [-2, -3, -4]), [2, 3, 4])

        # max, min
        self.assertEqual(env.find('max')['max'](2, 4, 3), 4)
        self.assertEqual(env.find('min')['min'](4, 2, 3), 2)

        # not, null?
        self.assertTrue(env.find('not')['not'](op.gt(2, 3)))
        self.assertTrue(env.find('null?')['null?']([]))

        # number?, procedure?, symbol?
        self.assertTrue(env.find('number?')['number?'](3))
        self.assertTrue(env.find('procedure?')['procedure?'](op.add))
        self.assertTrue(env.find('symbol?')['symbol?']('+'))
