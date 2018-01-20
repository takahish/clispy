from lispy.parser import parse
from lispy.eval import eval

def repl(prompt='lispy> ' ):
    """A prompt-read-eval-print loop."
    """
    while True:
        val = eval(parse(input(prompt)))
        if val is not None:
            print(lisp_str(val))

def lisp_str(exp):
    """Convert a Python object back into a Lisp-readable string.
    """
    if isinstance(exp, list):
        return '(' + ' '.join(map(lisp_str, exp)) + ')'
    else:
        return str(exp)

