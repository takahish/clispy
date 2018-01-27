import re
import io
import operator as op
from functools import reduce
from clispy.symbol import _Symbol, _symbol_table
from clispy.symbol import _quote, _if, _set, _define, _lambda, _begin, _define_macro
from clispy.symbol import _quasiquote, _unquote, _unquote_splicing
from clispy.symbol import _quotes
from clispy.symbol import _eof_object
from clispy.eval import _eval
from clispy.macro import _macro_table

class _InPort(object):
    """An input port. Retains a line of chars.
    """
    __tokenizer = r"""\s*(,@|[('`,)]|"(?:[\\].|[^\\"])*"|;.*|[^\s('"`,;)]*)(.*)"""

    def __init__(self, file):
        self.file = file
        self.line = ''

    def next_token(self):
        """Return the next token, reading new text into line buffer if needed.
        """
        while True:
            if self.line == '':
                self.line = self.file.readline()
            if self.line == '':
                return _eof_object
            token, self.line = re.match(_InPort.__tokenizer, self.line).groups()
            if token != '' and not token.startswith(';'):
                return token

def _atom(token):
    """Numbers become numbers; #t and #f are booleans; "..." string; otherwise Symbol.
    """
    if token == '#t':
        return True
    elif token == '#f':
        return False
    elif token[0] == '"':
        encoded_str = token[1:-1].encode('unicode_escape')
        return encoded_str.decode('unicode_escape')

    try:
        return int(token)
    except ValueError:
        try:
            return float(token)
        except ValueError:
            try:
                return complex(token.replace('i', 'j', 1))
            except ValueError:
                return _symbol_table[token]

def _read_ahead(token, inport):
    ###Helper function of read.
    ###
    if '(' == token:
        L = []
        while True:
            token = inport.next_token()
            if token == ')':
                return L
            else:
                L.append(_read_ahead(token, inport))
    elif ')' == token:
        raise SyntaxError('unexpected )')
    elif token in _quotes:
        return [_quotes[token], _read(inport)]
    elif token is _eof_object:
        raise SyntaxError('unexpected EOF in list')
    else:
        return _atom(token)

def _read(inport):
    """Read scheme expression from an inport port.
    """
    # body of _read
    token1 = inport.next_token()
    return _eof_object if token1 is _eof_object else _read_ahead(token1, inport)

def _parse(inport):
    """Parse a program: read and expand/error-check it.
    """
    if isinstance(inport, str):
        inport = _InPort(io.StringIO(inport))
    return _expand(_read(inport), top_level=True)

def _readchar(inport):
    """Read the next character from an input port.
    """
    if inport.line != '':
        char, inport.line = inport.line[0], inport.line[1:]
        return char
    else:
        return inport.file.read(1) or _eof_object

def _to_string(x):
    """Convert a Python object back into a Lisp-readable string.
    """
    if x is True:
        return '#t'
    elif x is False:
        return '#f'
    elif isinstance(x, _Symbol):
        return x
    elif isinstance(x, str):
        return '"%s"' % x.encode('unicode_escape').decode('unicode_escape').replace('"', r'\"')
    elif isinstance(x, list):
        return '(' + ' '.join(map(_to_string, x)) + ')'
    elif isinstance(x, complex):
        return str(x).replace('j', 'i')
    else:
        return str(x)

def _require(x, predicate, msg="wrong length"):
    """Signal a syntax error if predicate is false.
    """
    if not predicate:
        raise SyntaxError(_to_string(x) + ': ' + msg)

def _is_pair(x):
    return x != [] and isinstance(x, list)

def _append(*args):
    return reduce(op.add, args)

def _cons(x, y):
    return [x] + y

def _expand_quasiquote(x):
    """Expand `x => 'x; `,x => x; `(,@x y) => (append x y).
    """
    if not _is_pair(x):
        return [_quote, x]
    _require(x, x[0] is not _unquote_splicing, "can't splice here")
    if x[0] is _unquote:
        _require(x, len(x)==2)
        return x[1]
    elif _is_pair(x[0]) and x[0][0] is _unquote_splicing:
        _require(x[0], len(x[0])==2)
        return [_append, x[0][1], _expand_quasiquote(x[1:])]
    else:
        return [_cons, _expand_quasiquote(x[0]), _expand_quasiquote(x[1:])]

def _expand(x, top_level=False):
    """Walk tree of x, making optimizations/fixes, and sinaling SyntaxError
    """
    _require(x, x!=[])                   # () => Error
    if not isinstance(x, list):          # constant => unchanged
        return x
    elif x[0] is _quote:                 # (quote exp)
        _require(x, len(x)==2)
        return x
    elif x[0] is _if:
        if len(x) == 3:
            x = x + [None]               # (if t c) => (if t c None)
        _require(x, len(x)==4)
        return [_expand(xi, top_level) for xi in x]
    elif x[0] is _set:
        _require (x, len(x)==3)
        var = x[1]
        require(x, isinstance(var, _Symbol), msg="can set! only a symbol")
        return [_set, var, expand(x[2])]
    elif x[0] is _define or x[0] is _define_macro:
        _require(x, len(x)>=3)
        _def, v, body = x[0], x[1], x[2:]
        if isinstance(v, list) and v:    # (define (f args) body)
            f, args = v[0], v[1:]        #  => (define f (lambda (args) body))
            return _expand([_def, f, [_lambda, args]+body])
        else:
            _require(x, len(x)==3)       # (define non-var/list exp) => Error
            _require(x, isinstance(v, _Symbol), "can define only a symbol")
            exp = _expand(x[2])
            if _def is _define_macro:
                _require(x, top_level, "define-macro only allowed at top level")
                proc = _eval(exp)
                _require(x, callable(proc), "macro must be a purocedure")
                _macro_table[v] = proc   # (define-macro v exp)
                return None              #  => None; add {v: exp} to macro_table
            return [_define, v, exp]
    elif x[0] is _begin:
        if len(x) == 1:
            return None                  # (begin) => None
        else:
            return [_expand(xi, top_level) for xi in x]
    elif x[0] is _lambda:                # (lambda (x) e1 e2)
        _require(x, len(x)>=3)           #  => (lambda (x) (begin e1 e2))
        vars, body = x[1], x[2:]
        _require(x, (isinstance(vars, list) and all(isinstance(v, _Symbol) for v in vars))
                 or isinstance(vars, _Symbol), "illegal lambda argument list")
        exp = body[0] if len(body) == 1 else [_begin] + body
        return [_lambda, vars, _expand(exp)]
    elif x[0] is _quasiquote:            # `x => expand_quasiquote(x)
        _require(x, len(x)==2)
        return _expand_quasiquote(x[1])
    elif isinstance(x[0], _Symbol) and x[0] in _macro_table.table:
        return _expand(_macro_table[x[0]](*x[1:]), top_level) # (m arg...) => macroexpand if m isinstance macro
    else:
        return [_expand(xi, top_level) for xi in x]
