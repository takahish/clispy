# Copyright 2018 Takahiro Ishikawa. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# ==============================================================================

import operator as op
from functools import reduce
from clispy import cons


class BuiltInFunction(dict):
    """Built-In Function, sub-class of dictionary.
    """
    def __init__(self):
        """Inits BuiltInFunction.
        """
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

    Args:
        x: S-expression.

    Returns:
        Boolean.
    """
    return x is False or x == []

def _consp(x):
    """Test whether x is a cons sell or not.

    Args:
        x: S-expression.

    Returns:
        Boolean.
    """
    if isinstance(x, cons.Cons) and len(x) > 0:
        return True
    return False

def _listp(lst):
    """Test whether lst is a list or not.

    Args:
        x: S-expression.

    Returns:
        Boolean.
    """
    return _null(lst) or _consp(lst)

def _atom(x):
    """Test whether x is an atom nor not.

    Args:
        x: S-expression.

    Returns:
        Boolean.
    """
    if _consp(x):
        return False
    return True

def _cons(x, lst):
    """cons.

    Args:
        x: S-expression.
        lst: List.

    Returns:
        Cons or DottedPair.
    """
    if isinstance(lst, cons.DottedPair):  # (cons 1 '(2 . 3)) => (1 2 . 3)
        return cons.DottedPair([x] + lst)
    if not _null(lst) and _atom(lst):     # (cons 1 2) => (1 . 2)
        return cons.DottedPair([x, lst])
    elif _null(lst):                      # (cons 1 nil) => (1)
        return [x]
    return [x] + lst                      # (cons 1 '(2)) => (1 2)

def _car(lst):
    """car.

    Args:
        lst: List.

    Returns:
        Atom.
    """
    if _null(lst):
        return False
    elif _consp(lst):
        return lst[0]
    raise TypeError("The value " + str(lst) + " is not LIST.")

def _cdr(lst):
    """cdr.

    Args:
        lst: List.

    Returns:
        List or Atom.
    """
    if _null(lst) or (_consp(lst) and len(lst) <= 1):         # (cdr nil) or (cdr '()) => NIL
        return False
    elif isinstance(lst, cons.DottedPair) and len(lst) == 2: # (cdr '(1 . 2)) => 2
        return lst[1]
    elif isinstance(lst, cons.DottedPair) and len(lst) > 2:  # (cdr '(1 2 . 3)) => (2 . 3)
        return cons.DottedPair(lst[1:])
    elif _consp(lst) and len(lst) > 1:                        # (cdr '(1 2)) => (2)
        return lst[1:]
    raise TypeError("The value " + str(lst) + " is not LIST.")

def _append(*args):
    """Append some lists.

    Args:
        args: Some lists

    Returns:
        List.
    """
    if len(args) == 0:
        return False
    elif len(args) == 1:
        return args[0]
    else:
        return reduce(op.add, [arg for arg in args if _null(arg) is False])

def _list(*args):
    """Return a list containing the supplied objects.

    Args:
        args: Some atoms.

    Return:
        List.
    """
    if len(args) == 0:
        return False
    return list(args)

def _eq(x, y):
    """eq.

    Args:
        x: S-expression.
        y: S-expression.

    Returns:
        Boolean.
    """
    return x is y

def _eql(x, y):
    """eql.

    Args:
        x: S-expression.
        y: S-expression.

    Returns:
        Boolean.
    """
    if _consp(x) and _consp(y):
        if len(x) != len(y):
            return False
        else:
            return _eql(x[0], y[0]) or _eql(x[1:], y[1:])
    else:
        return _eq(x, y)

def _not(x):
    """not.

    Args:
        x: S-expression.

    Returns:
        Boolean.
    """
    return op.not_(x)

def _numberp(x):
    """Test whether x is number or not.

    Args:
        x: S-expression.

    Returns:
        Boolean.
    """
    return isinstance(x, int) or isinstance(x, float)

def _arithemetic(arithemetic, args):
    """Arithemetic operator for variable arguments.

    Args:
        arithemetic: Arithemetic operator.
        args: Some numbers.

    Returns:
        Number.
    """
    if all([_numberp(arg) for arg in args]):
        return reduce(arithemetic, args)
    else:
        raise TypeError("The value must be NUMBER.")

def _add(*args):
    """Add.

    Args:
        args: Some numbers.

    Returns:
        Number.
    """
    return _arithemetic(op.add, args)

def _sub(*args):
    """Subtruct.

    Args:
        args: Some numbers.

    Returns:
        Number.
    """
    return _arithemetic(op.sub, args)

def _mul(*args):
    """Multiply.

    Args:
        args: Some numbers.

    Returns:
        Number.
    """
    return _arithemetic(op.mul, args)

def _div(*args):
    """Divide.

    Args:
        args: Some numbers.

    Returns:
        Number.
    """
    return _arithemetic(op.truediv, args)

def _comp(comp, *args):
    """Comparison function for variable arguments.

    Args:
        comp: Comparison operator.
        args: Some numbers.

    Returns:
        Boolean.
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

    Args:
        comp: Comparison operator.
        args: Some numbers.

    Returns:
        Boolean.
    """
    if len(args) == 0:
        raise TypeError("At least 1 expected.")

    if all([_numberp(arg) for arg in args]):
        return _comp(comp, *args)
    else:
        raise TypeError("The value must be NUMBER.")

def _gt(*args):
    """Greater than.

    Args:
        args: Some numbers.

    Returns:
        Boolean.
    """
    return _comp_with_argument_check(op.gt, *args)

def _lt(*args):
    """Less than.

    Args:
        args: Some numbers.

    Returns:
        Boolean.
    """
    return _comp_with_argument_check(op.lt, *args)

def _ge(*args):
    """Greater than equal.

    Args:
        args: Some numbers.

    Returns:
        Boolean.
    """
    return _comp_with_argument_check(op.ge, *args)

def _le(*args):
    """Less than equal.

    Args:
        args: Some numbers.

    Returns:
        Boolean.
    """
    return _comp_with_argument_check(op.le, *args)

def _neq(*args):
    """Number equal.

    Args:
        args: Some numbers.

    Returns:
        Boolean.
    """
    return _comp_with_argument_check(op.eq, *args)

def _max(*args):
    """Maximum.

    Args:
        args: Some numbers.

    Returns:
        Boolean.
    """
    return _arithemetic(max, args)

def _min(*args):
    """Minimum.

    Args:
        args: Some numbers.

    Returns:
        Boolean.
    """
    return _arithemetic(min, args)
