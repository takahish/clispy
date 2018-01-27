import re
import io
import operator as op
import symbol
import eval
import macro
from functools import reduce

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
                return symbol._eof_object
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
                return symbol._symbol_table[token]

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
    elif token in symbol._quotes:
        return [symbol._quotes[token], _read(inport)]
    elif token is symbol._eof_object:
        raise SyntaxError('unexpected EOF in list')
    else:
        return _atom(token)

def _read(inport):
    """Read scheme expression from an inport port.
    """
    # body of _read
    token1 = inport.next_token()
    return symbol._eof_object if token1 is symbol._eof_object else _read_ahead(token1, inport)

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
        return inport.file.read(1) or symbol._eof_object

def _to_string(x):
    """Convert a Python object back into a Lisp-readable string.
    """
    if x is True:
        return '#t'
    elif x is False:
        return '#f'
    elif isinstance(x, symbol._Symbol):
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
        return [symbol._quote, x]
    _require(x, x[0] is not symbol._unquote_splicing, "can't splice here")
    if x[0] is symbol._unquote:
        _require(x, len(x)==2)
        return x[1]
    elif _is_pair(x[0]) and x[0][0] is symbol._unquote_splicing:
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
    elif x[0] is symbol._quote:                 # (quote exp)
        _require(x, len(x)==2)
        return x
    elif x[0] is symbol._if:
        if len(x) == 3:
            x = x + [None]               # (if t c) => (if t c None)
        _require(x, len(x)==4)
        return [_expand(xi, top_level) for xi in x]
    elif x[0] is symbol._set:
        _require (x, len(x)==3)
        var = x[1]
        require(x, isinstance(var, symbol._Symbol), msg="can set! only a symbol")
        return [symbol._set, var, expand(x[2])]
    elif x[0] is symbol._define or x[0] is symbol._define_macro:
        _require(x, len(x)>=3)
        _def, v, body = x[0], x[1], x[2:]
        if isinstance(v, list) and v:    # (define (f args) body)
            f, args = v[0], v[1:]        #  => (define f (lambda (args) body))
            return _expand([_def, f, [symbol._lambda, args]+body])
        else:
            _require(x, len(x)==3)       # (define non-var/list exp) => Error
            _require(x, isinstance(v, symbol._Symbol), "can define only a symbol")
            exp = _expand(x[2])
            if _def is symbol._define_macro:
                _require(x, top_level, "define-macro only allowed at top level")
                proc = eval._eval(exp)
                _require(x, callable(proc), "macro must be a purocedure")
                macro._macro_table[v] = proc   # (define-macro v exp)
                return None              #  => None; add {v: exp} to macro_table
            return [symbol._define, v, exp]
    elif x[0] is symbol._begin:
        if len(x) == 1:
            return None                  # (begin) => None
        else:
            return [_expand(xi, top_level) for xi in x]
    elif x[0] is symbol._lambda:                # (lambda (x) e1 e2)
        _require(x, len(x)>=3)           #  => (lambda (x) (begin e1 e2))
        vars, body = x[1], x[2:]
        _require(x, (isinstance(vars, list) and all(isinstance(v, symbol._Symbol) for v in vars))
                 or isinstance(vars, symbol._Symbol), "illegal lambda argument list")
        exp = body[0] if len(body) == 1 else [symbol._begin] + body
        return [symbol._lambda, vars, _expand(exp)]
    elif x[0] is symbol._quasiquote:            # `x => expand_quasiquote(x)
        _require(x, len(x)==2)
        return _expand_quasiquote(x[1])
    elif isinstance(x[0], symbol._Symbol) and x[0] in macro._macro_table.table:
        return _expand(macro._macro_table[x[0]](*x[1:]), top_level) # (m arg...) => macroexpand if m isinstance macro
    else:
        return [_expand(xi, top_level) for xi in x]
