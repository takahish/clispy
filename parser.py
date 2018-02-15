import re
import io
import symbol
import env
import eval
import function
import cons

class _InPort(object):
    """An input port. Retains a line of chars.
    """
    __tokenizer = r"""\s*(,@|#'|[('`,)]|"(?:[\\].|[^\\"])*"|;.*|[^\s('"`,;)]*)(.*)"""

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
    if token == 't':
        return True
    elif token == 'nil':
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
    return _expand(_read(inport))

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
        return 'T'
    elif x is False:
        return 'NIL'
    elif isinstance(x, symbol._Symbol):
        return x
    elif isinstance(x, str):
        return '"%s"' % x.encode('unicode_escape').decode('unicode_escape').replace('"', r'\"')
    elif isinstance(x, cons._DottedPair):
        x = x[:-1] + [symbol._dot] + [x[-1]]
        return '(' + ' '.join(map(_to_string, x)) + ')'
    elif isinstance(x, cons._Cons):
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

def _expand_quasiquote(x):
    """Expand `x => 'x; `,x => x; `(,@x y) => (append x y).
    """
    if not function._consp(x):
        return [symbol._quote, x]
    _require(x, x[0] is not symbol._unquote_splicing, "can't splice here")
    if x[0] is symbol._unquote:
        _require(x, len(x)==2)
        return x[1]
    elif function._consp(x[0]) and x[0][0] is symbol._unquote_splicing:
        _require(x[0], len(x[0])==2)
        return [symbol._append, x[0][1], _expand_quasiquote(x[1:])]
    else:
        return [symbol._cons, _expand_quasiquote(x[0]), _expand_quasiquote(x[1:])]

def _replace_expression(exp, old, new):
    """Replace expression in nested list.
    """
    if not isinstance(exp, list) or len(exp) == 0:
        if exp == old:
            return new
        else:
            return exp
    else:
        return [_replace_expression(exp[0], old, new)] + _replace_expression(exp[1:], old, new)

def _expand(x):
    """Walk tree of x, making optimizations/fixes, and sinaling SyntaxError
    """
    _require(x, x!=[])                         # () => Error
    if not isinstance(x, list):                # constant => unchanged
        return x
    elif x[0] is symbol._quote:                # (quote exp)
        _require(x, len(x)==2)
        return x
    elif x[0] is symbol._if:
        if len(x) == 3:
            x = x + [False]                    # (if t c) => (if t c nil)
        _require(x, len(x)==4)
        return [_expand(xi) for xi in x]
    elif x[0] is symbol._setq:
        _require (x, len(x)==3)
        var = x[1]
        _require(x, isinstance(var, symbol._Symbol), msg="can set! only a symbol")
        return [symbol._setq, var, _expand(x[2])]
    elif x[0] is symbol._defun or x[0] is symbol._defmacro:
        if len(x) >= 4:                        # (defun f (args) body)
                                               #  => (defun f (lambda (args) body))
            _def, f, args, body = x[0], x[1], x[2], x[3:]
            if isinstance(args, list) and args:
                return _expand([_def, f, [symbol._lambda, args]+body])
        else:
            _require(x, len(x)==3)             # (defun non-var/list exp) => Error
            _def, f, exp = x[0], x[1], x[2]
            exp = _expand(x[2])
            if _def is symbol._defmacro:       # (defmacro v exp)
                                               #  => None; add {f: exp} to function env
                proc = eval._eval(exp)
                _require(x, callable(proc), "macro must be a purocedure")
                try:
                    env._macro_env.find(f)[f] = proc
                except LookupError:
                    env._macro_env[f] = proc
                return env._macro_env[f]
            return [symbol._defun, f, exp]
    elif x[0] is symbol._progn:
        if len(x) == 1:
            return False                       # (progn) => NIL
        else:
            return [_expand(xi) for xi in x]
    elif x[0] is symbol._lambda:               # (lambda (x) e1 e2)
        _require(x, len(x)>=3)                 #  => (lambda (x) (progn e1 e2))
        vars, body = x[1], x[2:]
        _require(x, (isinstance(vars, list) and all(isinstance(v, symbol._Symbol) for v in vars))
                 or isinstance(vars, symbol._Symbol), "illegal lambda argument list")
        exp = body[0] if len(body) == 1 else [symbol._progn] + body
        return [symbol._lambda, vars, _expand(exp)]
    elif x[0] is symbol._quasiquote:           # `x => expand_quasiquote(x)
        _require(x, len(x)==2)
        return _expand_quasiquote(x[1])
    elif x[0] is symbol._let:                  # (let ((var val)) body)
        _require(x, len(x)>2)                  #  => ((lambda (var) body) val)
        bindings, body = x[1], x[2:]
        _require(x, all(isinstance(b, list) and len(b) == 2 and isinstance(b[0], symbol._Symbol)
                 for b in bindings), "illegal bindig list")
        vars, vals = zip(*bindings)
        return [[symbol._lambda, list(vars)] + [[symbol._progn] + list(map(_expand, body))]] + list(map(_expand, vals))
    elif x[0] is symbol._flet:                 # (flet ((func var exp)) body)
        _require(x, len(x)==3)                 #  => (progn body_replaced_func_to_lambda)
        bindings, body = x[1], x[2:]
        _require(x, all(isinstance(b, list) and len(b) == 3 and isinstance(b[0], symbol._Symbol)
                        and isinstance(b[1], list) and isinstance(b[2], list)
                        for b in bindings), "illegal binding list")
        for binding in bindings:
            body = _replace_expression(body, binding[0], [symbol._lambda, binding[1], binding[2]])
        return [symbol._progn] + list(map(_expand, body))
    elif isinstance(x[0], symbol._Symbol) and x[0] in env._macro_env:
        return _expand(env._macro_env.find(x[0])[x[0]](*x[1:]))
                                               # (m arg...) => macroexpand if m isinstance macro
    else:
        _require(x, isinstance(x[0], symbol._Symbol)
                 or (isinstance(x[0], list) and x[0][0] is symbol._lambda),
                 "illegal function object")
        return [_expand(xi) for xi in x]
