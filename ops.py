import math
import cmath
import operator as op
from functools import reduce
import symbol

class _BuiltInOperator(dict):
    """Built-In Operator, sub-class of dictionary.
    """
    pass

def _is_pair(x):
    """Test a whether list or not.
    """
    return x != [] and isinstance(x, list)

def _append(*args):
    """Append some lists.
    """
    return reduce(op.add, args)

def _cons(x, y):
    return [x] + y

def _comp(cp, *args):
    """Comparison operator for variable arguments.
    """
    if len(args) < 2:
        return True
    else:
        if cp(args[0], args[1]):
            return _comp(cp, *args[1:])
        else:
            return False

_builtin_operator = _BuiltInOperator()

_builtin_operator.update(vars(math))
_builtin_operator.update(vars(cmath))
_builtin_operator.update({
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
    'list?': lambda lst: isinstance(lst, list),
    'map': lambda func, args: list(map(func, args)),
    'max': lambda *args: reduce(max, args),
    'min': lambda *args: reduce(min, args),
    'not': op.not_,
    'null?': lambda lst: lst == [],
    'number?': lambda x: isinstance(x, (int, float)),
    'procedure?': callable,
    'round': round,
    'symbol?': lambda x: isinstance(x, symbol._Symbol)
})
