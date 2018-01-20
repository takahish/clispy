from lispy.symbol import Symbol
from lispy.env import Env, _global_env

class Procedure(object):
    """A user-defined scheme procedure.
    """
    def __init__(self, params, body, env):
        self.params, self.body, self.env = params, body, env

    def __call__(self, *args):
        return eval(self.body, Env(self.params, args, self.env))

def eval(x, env=_global_env):
    """Evaluate an expression in an environment.
    """
    if isinstance(x, Symbol):      # variable reference
        return env.find(x)[x]
    elif not isinstance(x, list):  # constant literal
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
