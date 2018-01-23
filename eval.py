from clispy.symbol import _Symbol, _symbol_table, _quote, _if, _set, _define, _lambda, _begin
from clispy.env import _Env, _global_env

class _Procedure(object):
    """A user-defined scheme procedure.
    """
    def __init__(self, params, exp, env):
        self.params, self.exp, self.env = params, exp, env

    def __call__(self, *args):
        return _eval(self.exp, _Env(self.params, args, self.env))

def _eval(x, env=_global_env):
    """Evaluate an expression in an environment.
    """
    while True:
        if isinstance(x, _Symbol):    # variable reference
            return env.find(x)[x]
        elif not isinstance(x, list): # constant literal
            return x
        elif x[0] is _quote:          # (quote exp)
            (_, exp) = x
            return exp
        elif x[0] is _if:             # if test conseq alt
            (_, test, conseq, alt) = x
            x = (conseq if _eval(test, env) else alt)
        elif x[0] is _define:
            (_, var, exp) = x         # (define var exp)
            env[var] = _eval(exp, env)
            return None
        elif x[0] is _set:            # (set! var exp)
            (_, var, exp) = x
            env.find(var)[var] = _eval(exp, env)
            return None
        elif x[0] is _lambda:         # (lambda (var...) body)
            (_, params, exp) = x
            return _Procedure(params, exp, env)
        elif x[0] is _begin:          # (begin exp+)
            for exp in x[1:-1]:
                _eval(exp, env)
            x = x[-1]
        else:
            exps = [_eval(exp, env) for exp in x]
            proc = exps.pop(0)
            if isinstance(proc, _Procedure):
                x = proc.exp
                env = _Env(proc.params, exps, proc.env)
            else:
                return proc(*exps)
