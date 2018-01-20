from lispy.symbol import _Symbol

def _parse(program):
    """Read a Scheme expression from a tring.
    """
    return _read_from_tokens(_tokenize(program))

def _tokenize(s):
    """Convert a string into a list of tokens.
    """
    return s.replace('(', ' ( ').replace(')', ' ) ').split()

def _read_from_tokens(tokens):
    """Read an expression from a sequence of tokens."
    """
    if len(tokens) == 0:
        raise SyntaxError('unexpected EOF while reading')
    token = tokens.pop(0)
    if '(' == token:
        L = []
        while tokens[0] != ')':
            L.append(_read_from_tokens(tokens))
        tokens.pop(0) # pop off ')'
        return L
    elif ')' == token:
        raise SyntaxError('unexpected )')
    else:
        return _atom(token)

def _atom(token):
    """Numbers become numbers; every other token is a Symbol.
    """
    try: return int(token)
    except ValueError:
        try: return float(token)
        except ValueError:
            return _Symbol(token)
    