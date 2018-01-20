from lispy.type import Symbol

def parse(program):
    """Read a Scheme expression from a tring.
    """
    return read_from_tokens(tokenize(program))

def tokenize(s):
    """Convert a string into a list of tokens.
    """
    return s.replace('(', ' ( ').replace(')', ' ) ').split()

def read_from_tokens(tokens):
    """Read an expression from a sequence of tokens."
    """
    if len(tokens) == 0:
        raise SyntaxError('unexpected EOF while reading')
    token = tokens.pop(0)
    if '(' == token:
        L = []
        while tokens[0] != ')':
            L.append(read_from_tokens(tokens))
        tokens.pop(0) # pop off ')'
        return L
    elif ')' == token:
        raise SyntaxError('unexpected )')
    else:
        return atom(token)

def atom(token):
    """Numbers become numbers; every other token is a Symbol.
    """
    try: return int(token)
    except ValueError:
        try: return float(token)
        except ValueError:
            return Symbol(token)
    