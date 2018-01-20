import math
import operator as op
from functools import reduce

from lispy.type import Symbol, List, Number

class Env(dict):
    """An environment: a dict of {'var': val} pairs, with an outer Env.
    """
    def __init__(self, params=(), args=(), outer=None):
        self.update(zip(params, args))
        self.outer = outer

    def find(self, var):
        """Find the innermost Env where var appears.
        """
        return self if (var in self) else self.outer.find(var)

def standard_env():
    """An environment with some Scheme standard procedures.
    """
    env = Env()
    env.update(vars(math))
    env.update({
        '+': lambda *args: reduce(op.add, args),
        '-': lambda *args: reduce(op.sub, args),
        '*': lambda *args: reduce(op.mul, args),
        '/': lambda *args: reduce(op.truediv, args),
        '>': lambda *args: _comp(op.gt, *args),
        '<': lambda *args: _comp(op.lt, *args),
        '>=': lambda *args: _comp(op.ge, *args),
        '<=': lambda *args: _comp(op.le, *args),
        '=': lambda *args: _comp(op.eq, *args),
        'abs': abs,
        'append': lambda *args: reduce(op.add, args),
        'apply': lambda func, args: func(*args),
        'begin': lambda *args: args[-1],
        'car': lambda lst: lst[0],
        'cdr': lambda lst: lst[1:],
        'cons': lambda x, lst: [x] + lst,
        'eq?': op.is_,
        'equal?': op.eq,
        'length': len,
        'list': lambda *args: list(args),
        'list?': lambda lst: isinstance(lst, List),
        'map': lambda func, args: list(map(func, args)),
        'max': lambda *args: reduce(max, args),
        'min': lambda *args: reduce(min, args),
        'not': op.not_,
        'null?': lambda lst: lst == [],
        'number?': lambda x: isinstance(x, Number),
        'procedure?': callable,
        'round': round,
        'symbol?': lambda x: isinstance(x, Symbol)
    })
    return env

def _comp(op, *args):
    """comparison operator for variable arguments

    Args:
        op: comparison operator
        *args: variable arguments

    Return:
        boolean
    """
    if len(args) < 2:
        return True
    else:
        if op(args[0], args[1]):
            return _comp(op, *args[1:])
        else:
            return False
