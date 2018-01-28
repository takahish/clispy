import symbol
import ops

class _Env(dict):
    """An environment: a dict of {'var': val} pairs, with an outer Env.
    """
    def __init__(self, params=(), args=(), outer=None):
        self.update(zip(params, args))
        self.outer = outer

        # Bind param list to corresponding args, or single param to list of args
        if isinstance(params, symbol._Symbol):
            self.update({params: list(args)})
        else:
            if len(args) != len(params):
                raise TypeError('expected %s, given %s, ' % (params, args))
            self.update(zip(params, args))

    def find(self, var):
        """Find the innermost Env where var appears.
        """
        if var in self:
            return self
        elif self.outer is None:
            raise LookupError(var)
        else:
            return self.outer.find(var)

_global_env = _Env()
_global_env.update(ops._builtin_operator)
