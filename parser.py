import re
import io
from clispy.symbol import _Symbol, _symbol_table, _quotes

# Note uninterned; can't be read
_eof_object = _Symbol('#<eof-object>')

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
    return _read(inport)

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
