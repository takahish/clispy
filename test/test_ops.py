import unittest
import operator as op
import symbol
import ops

class UnitTestCase(unittest.TestCase):
    def test_is_pair(self):
        self.assertTrue(ops._is_pair([1]))
        self.assertFalse(ops._is_pair([]))

    def test_append(self):
        self.assertEqual(ops._append([2, 3], [4], [5, 6]), [2, 3, 4, 5, 6])

    def test_cons(self):
        self.assertEqual(ops._cons(1, [2, 3]), [1, 2, 3])

    def test_comp(self):
        self.assertTrue(ops._comp(op.gt, 5, 4, 3))
        self.assertFalse(ops._comp(op.gt, 4, 3, 5))

    def test_builtin_operator(self):
        # +, -, *, /
        self.assertEqual(ops._builtin_operator['+'](2, 3, 4), 9)
        self.assertEqual(ops._builtin_operator['-'](2, 3, 4), -5)
        self.assertEqual(ops._builtin_operator['*'](2, 3, 4), 24)
        self.assertEqual(ops._builtin_operator['/'](4, 2, 2), 1)

        # >, <, >=, <=, =
        self.assertTrue(ops._builtin_operator['>'](4, 3, 2))
        self.assertFalse(ops._builtin_operator['>'](3, 2, 4))
        self.assertTrue(ops._builtin_operator['<'](2, 3, 4))
        self.assertFalse(ops._builtin_operator['<'](3, 4, 2))
        self.assertTrue(ops._builtin_operator['>='](3, 3, 2))
        self.assertFalse(ops._builtin_operator['>='](3, 2, 3))
        self.assertTrue(ops._builtin_operator['<='](3, 3, 4))
        self.assertFalse(ops._builtin_operator['<='](3, 4, 3))
        self.assertTrue(ops._builtin_operator['='](2, 2, 2))
        self.assertFalse(ops._builtin_operator['='](2, 2, 3))

        # abs, round
        self.assertEqual(ops._builtin_operator['abs'](-3), 3)
        self.assertEqual(ops._builtin_operator['round'](3.4), 3)
        self.assertEqual(ops._builtin_operator['round'](3.5), 4)

        # append
        self.assertEqual(ops._builtin_operator['append']([2, 3], [4], [5, 6]), [2, 3, 4, 5, 6])

        # apply
        self.assertEqual(ops._builtin_operator['apply'](op.add, [2, 3]), 5)

        # begin
        self.assertEqual(ops._builtin_operator['begin'](2, 3, 4), 4)

        # car, cdr, cons
        self.assertEqual(ops._builtin_operator['car']([2, 3, 4]), 2)
        self.assertEqual(ops._builtin_operator['cdr']([2, 3, 4]), [3, 4])
        self.assertEqual(ops._builtin_operator['cons'](2, [3, 4]), [2, 3, 4])

        # eq?, equal?
        self.assertTrue(ops._builtin_operator['eq?'](2, 2))
        self.assertFalse(ops._builtin_operator['eq?']([2, 3], [2, 3]))
        self.assertTrue(ops._builtin_operator['equal?']([2, 3], [2, 3]))

        # length, list, list?
        self.assertEqual(ops._builtin_operator['length']([2, 3, 4]), 3)
        self.assertEqual(ops._builtin_operator['list'](2, 3, 4), [2, 3, 4])
        self.assertTrue(ops._builtin_operator['list?']([2, 3, 4]))

        # map
        self.assertEqual(ops._builtin_operator['map'](abs, [-2, -3, -4]), [2, 3, 4])

        # max, min
        self.assertEqual(ops._builtin_operator['max'](2, 4, 3), 4)
        self.assertEqual(ops._builtin_operator['min'](4, 2, 3), 2)

        # not, null?
        self.assertTrue(ops._builtin_operator['not'](op.gt(2, 3)))
        self.assertTrue(ops._builtin_operator['null?']([]))

        # number?, procedure?, symbol?
        self.assertTrue(ops._builtin_operator['number?'](3))
        self.assertTrue(ops._builtin_operator['procedure?'](op.add))
        self.assertTrue(ops._builtin_operator['symbol?'](symbol._quote))
