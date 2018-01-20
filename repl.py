from lispy.parser import _parse
from lispy.eval import _eval

def repl(prompt='lispy> ' ):
    """A prompt-read-eval-print loop."
    """
    while True:
        val = _eval(_parse(input(prompt)))
        if val is not None:
            print(_lisp_str(val))

def _lisp_str(exp):
    """Convert a Python object back into a Lisp-readable string.
    """
    if isinstance(exp, list):
        return '(' + ' '.join(map(_lisp_str, exp)) + ')'
    else:
        return str(exp)

