import symbol

class _Env(dict):
    """An environment: a dict of {'var': val} pairs, with an outer Env.
    """
    def __init__(self, params=(), args=(), outer=None):
        self.update(zip(params, args))
        self.outer = outer

        # Bind param list to corresponding args, or single param to list of args
        if isinstance(params, symbol._Symbol):
            self.update({params: list(args)})
        else:
            if len(args) != len(params):
                raise TypeError('expected %s, given %s, ' % (params, args))
            self.update(zip(params, args))

    def find(self, var):
        """Find the innermost Env where var appears.
        """
        if var in self:
            return self
        elif self.outer is None:
            raise LookupError(var)
        else:
            return self.outer.find(var)

def _add_globals(self):
    """Add some scheme standard procedures
    """
    import math
    import cmath
    import operator as op
    from functools import reduce

    self.update(vars(math))
    self.update(vars(cmath))
    self.update({
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
    return self

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

_global_env = _add_globals(_Env())
