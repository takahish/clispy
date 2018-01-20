from lispy.type import Symbol, List
from lispy.env import Env, standard_env
from lispy.parser import parse

_global_env = standard_env()

class Procedure(object):
    """A user-defined scheme procedure.
    """
    def __init__(self, params, body, env):
        self.params, self.body, self.env = params, body, env

    def __call__(self, *args):
        return eval(self.body, Env(self.params, args, self.env))

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
    if isinstance(exp, List):
        return '(' + ' '.join(map(lisp_str, exp)) + ')'
    else:
        return str(exp)

def eval(x, env=_global_env):
    """Evaluate an expression in an environment.
    """
    if isinstance(x, Symbol):      # variable reference
        return env.find(x)[x]
    elif not isinstance(x, List):  # constant literal
        return x
    elif x[0] == 'quote':          # (quote exp)
        (_, exp) = x
        return exp
    elif x[0] == 'if':             # if test conseq alt
        (_, test, conseq, alt) = x
        exp = (conseq if eval(test, env) else alt)
        return eval(exp, env)
    elif x[0] == 'define':
        (_, var, exp) = x          # (define var exp)
        env[var] = eval(exp, env)
        return env[var]
    elif x[0] == 'set!':           # (set! var exp)
        (_, var, exp) = x
        env.find(var)[var] = eval(exp, env)
        return env.find(var)[var]
    elif x[0] == 'lambda':         # (lambda (var...) body)
        (_, params, body) = x
        return Procedure(params, body, env)
    else:
        proc = eval(x[0], env)
        args = [eval(exp, env) for exp in x[1:]]
        return proc(*args)
