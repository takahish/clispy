from lispy.symbol import _Symbol
from lispy.env import _Env, _global_env

class _Procedure(object):
    """A user-defined scheme procedure.
    """
    def __init__(self, params, body, env):
        self.params, self.body, self.env = params, body, env

    def __call__(self, *args):
        return _eval(self.body, _Env(self.params, args, self.env))

def _eval(x, env=_global_env):
    """Evaluate an expression in an environment.
    """
    if isinstance(x, _Symbol):      # variable reference
        return env.find(x)[x]
    elif not isinstance(x, list):  # constant literal
        return x
    elif x[0] == 'quote':          # (quote exp)
        (_, exp) = x
        return exp
    elif x[0] == 'if':             # if test conseq alt
        (_, test, conseq, alt) = x
        exp = (conseq if _eval(test, env) else alt)
        return _eval(exp, env)
    elif x[0] == 'define':
        (_, var, exp) = x          # (define var exp)
        env[var] = _eval(exp, env)
        return env[var]
    elif x[0] == 'set!':           # (set! var exp)
        (_, var, exp) = x
        env.find(var)[var] = _eval(exp, env)
        return env.find(var)[var]
    elif x[0] == 'lambda':         # (lambda (var...) body)
        (_, params, body) = x
        return _Procedure(params, body, env)
    else:
        proc = _eval(x[0], env)
        args = [_eval(exp, env) for exp in x[1:]]
        return proc(*args)
