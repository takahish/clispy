import symbol
import env

class _Procedure(object):
    """A user-defined scheme procedure.
    """
    def __init__(self, params, exp, env):
        self.params, self.exp, self.env = params, exp, env

    def __call__(self, *args):
        return _eval(self.exp, env._Env(self.params, args, self.env))

def _eval(x, e=env._global_env):
    """Evaluate an expression in an environment.
    """
    while True:
        if isinstance(x, symbol._Symbol):    # variable reference
            return e.find(x)[x]
        elif not isinstance(x, list): # constant literal
            return x
        elif x[0] is symbol._quote:          # (quote exp)
            (_, exp) = x
            return exp
        elif x[0] is symbol._if:             # if test conseq alt
            (_, test, conseq, alt) = x
            x = (conseq if _eval(test, e) else alt)
        elif x[0] is symbol._define:
            (_, var, exp) = x         # (define var exp)
            e[var] = _eval(exp, e)
            return None
        elif x[0] is symbol._set:            # (set! var exp)
            (_, var, exp) = x
            e.find(var)[var] = _eval(exp, e)
            return None
        elif x[0] is symbol._lambda:         # (lambda (var...) body)
            (_, params, exp) = x
            return _Procedure(params, exp, e)
        elif x[0] is symbol._begin:          # (begin exp+)
            for exp in x[1:-1]:
                _eval(exp, e)
            x = x[-1]
        else:
            exps = [_eval(exp, e) for exp in x]
            proc = exps.pop(0)
            if isinstance(proc, _Procedure):
                x = proc.exp
                e = env._Env(proc.params, exps, proc.env)
            else:
                return proc(*exps)
