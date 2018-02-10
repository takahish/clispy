import symbol
import env
import cons

class _Procedure(object):
    """A user-defined scheme procedure.
    """
    def __init__(self, params, exps, var_env, func_env):
        self.params = params
        self.exps = exps

        # Environment
        self.var_env = var_env
        self.func_env = func_env

    def __call__(self, *args):
        return _eval(
            self.exps,
            env._VariableEnvironment(self.params, args, self.var_env),
            env._FunctionEnvironment([], [], self.func_env)
        )

def _cons(lst):
    """Create cons sell (or dotted pair) object.
    """
    if len(lst) > 2 and lst[-2] == symbol._dot:
        if lst[-1] is not False:
            lst.remove(symbol._dot)
            return cons._DottedPair(lst)
        else:
            return lst[:-2]
    else:
        return lst

def _eval(x, var_env=env._var_env, func_env=env._func_env):
    """Evaluate an expression in an environment.
    """
    while True:
        if isinstance(x, symbol._Symbol):    # variable reference
            return var_env.find(x)[x]
        elif not isinstance(x, list):        # constant literal
            return x
        elif x[0] is symbol._quote:          # (quote exp)
            (_, exp) = x
            if isinstance(exp, list):        # list literal
                return _cons(exp)
            return exp
        elif x[0] is symbol._if:             # if test conseq alt
            (_, test, conseq, alt) = x
            x = (conseq if _eval(test, var_env, func_env) else alt)
        elif x[0] is symbol._defun:
            (_, func, exp) = x               # (defun f, var exp)
            func_env[func] = _eval(exp, var_env, func_env)
            return func_env[func]
        elif x[0] is symbol._setq:           # (setq var exp)
            (_, var, exp) = x
            var_env[var] = _eval(exp, var_env, func_env)
            return var_env[var]
        elif x[0] is symbol._lambda:         # (lambda (var...) body)
            (_, params, exp) = x
            return _Procedure(params, exp, var_env, func_env)
        elif x[0] is symbol._progn:          # (progn exp+)
            for exp in x[1:-1]:
                _eval(exp, var_env, func_env)
            x = x[-1]
        elif x[0] is symbol._function:       # (function func)
            (_, func) = x
            return func_env.find(func)[func]
        elif x[0] is symbol._funcall:        # (funcall func args)
            (_, func, *exps) = x
            proc = var_env.find(func)[func]
            exps = [_eval(exp, var_env, func_env) for exp in exps]
            return proc(*exps)
        else:
            if isinstance(x[0], symbol._Symbol):
                proc = func_env.find(x[0])[x[0]]
            elif isinstance(x[0], list) and x[0][0] is symbol._lambda:
                proc = _eval(x[0])
            exps = [_eval(exp, var_env, func_env) for exp in x[1:]]
            if isinstance(proc, _Procedure):
                x = proc.exps
                var_env = env._VariableEnvironment(proc.params, exps, proc.var_env)
                func_env = env._FunctionEnvironment([], [], proc.func_env)
            else:
                return proc(*exps)
