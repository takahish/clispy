import operator as op
from functools import reduce

class _BuiltInFunction(dict):
    """Built-In Function, sub-class of dictionary.
    """
    def __init__(self):
        self.update({
            'NULL': _null,
            'CONSP': _consp,
            'LISTP': _listp,
            'ATOM': _atom,
            'CONS': _cons,
            'CAR': _car,
            'CDR': _cdr,
            'APPEND': _append,
            'LIST': _list,
            'EQ': _eq,
            'EQL': _eql,
            'NOT': _not,
            'NUMBERP': _numberp,
            '+': _add,
            '-': _sub,
            '*': _mul,
            '/': _div,
            '>': _gt,
            '<': _lt,
            '>=': _ge,
            '<=': _le,
            '=': _neq,
            'MAX': _max,
            'MIN': _min
        })

def _null(x):
    """Test whether x is nil or not.
    """
    return x is False or x == []

def _consp(x):
    """Test whether x is a cons sell or not.
    """
    if isinstance(x, list) and len(x) > 0:
        return True
    return False

def _listp(lst):
    """Test whether lst is a list or not.
    """
    return _null(lst) or _consp(lst)

def _atom(x):
    """Test whether x is an atom nor not.
    """
    if _consp(x):
        return False
    return True

def _cons(x, lst):
    """cons
    """
    if _null(lst):
        lst = []
    elif _atom(lst):
        lst = [lst]
    return [x] + lst

def _car(lst):
    """car
    """
    if _null(lst):
        return False
    elif _consp(lst):
        return lst[0]
    raise TypeError("The value " + str(lst) + " is not LIST.")

def _cdr(lst):
    """cdr
    """
    if _null(lst) or (_consp(lst) and len(lst) <= 1):
        return False
    elif _consp(lst) and len(lst) > 1:
        return lst[1:]
    raise TypeError("The value " + str(lst) + " is not LIST.")

def _append(*args):
    """Append some lists.
    """
    if len(args) == 0:
        return False
    elif len(args) == 1:
        return args[0]
    else:
        return reduce(op.add, [arg for arg in args if _null(arg) is False])

def _list(*args):
    """Return a list containing the supplied objects.
    """
    if len(args) == 0:
        return False
    return list(args)

def _eq(x, y):
    """Equal
    """
    return x is y

def _eql(x, y):
    """Equal
    """
    if _consp(x) and _consp(y):
        if len(x) != len(y):
            return False
        else:
            return _eql(x[0], y[0]) or _eql(x[1:], y[1:])
    else:
        return _eq(x, y)

def _not(x):
    """Not
    """
    return op.not_(x)

def _numberp(x):
    """Test whether x is number or not.
    """
    return isinstance(x, int) or isinstance(x, float)

def _arithemetic(arithemetic, args):
    """Arithemetic operator for variable arguments.
    """
    if all([_numberp(arg) for arg in args]):
        return reduce(arithemetic, args)
    else:
        raise TypeError("The value must be NUMBER.")

def _add(*args):
    """Add
    """
    return _arithemetic(op.add, args)

def _sub(*args):
    """Subtruct
    """
    return _arithemetic(op.sub, args)

def _mul(*args):
    """Multiply
    """
    return _arithemetic(op.mul, args)

def _div(*args):
    """Divide
    """
    return _arithemetic(op.truediv, args)

def _comp(comp, *args):
    """Comparison function for variable arguments.
    """
    if len(args) == 1:
        return True
    else:
        if comp(args[0], args[1]):
            return _comp(comp, *args[1:])
        else:
            return False

def _comp_with_argument_check(comp, *args):
    """comparison function with argument check.
    """
    if len(args) == 0:
        raise TypeError("At least 1 expected.")

    if all([_numberp(arg) for arg in args]):
        return _comp(comp, *args)
    else:
        raise TypeError("The value must be NUMBER.")

def _gt(*args):
    """Greater than
    """
    return _comp_with_argument_check(op.gt, *args)

def _lt(*args):
    """Less than
    """
    return _comp_with_argument_check(op.lt, *args)

def _ge(*args):
    """Greater than equal
    """
    return _comp_with_argument_check(op.ge, *args)

def _le(*args):
    """Less than equal
    """
    return _comp_with_argument_check(op.le, *args)

def _neq(*args):
    """Number equal
    """
    return _comp_with_argument_check(op.eq, *args)

def _max(*args):
    """Maximum
    """
    return _arithemetic(max, args)

def _min(*args):
    """Minimum
    """
    return _arithemetic(min, args)
