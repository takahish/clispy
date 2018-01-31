import symbol
import ops

class _Env(dict):
    """An environment: a dict of {'var': val} pairs, with an outer Env.
    """
    def __init__(self, params=(), args=(), outer=None):
        # Bind param list to corresponding args, or single param to list of args
        self.outer = outer
        if isinstance(params, symbol._Symbol):
            self.update({params: list(args)})
        else:
            # bind rest parameters for lambda
            params, args = _Env._parse_rest_argument(params, args)
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

    @staticmethod
    def _parse_rest_argument(params, args):
        """Fine rest argument to parse params and args.
        """
        if '.' in params:
            params, args = list(params), list(args) # for slicing and appending
            # params=['x', '.', 'y'], args=[1, 2, 3, 4, 5]
            #  => params=['x', 'y'], args=[1, [2, 3, 4, 5]]
            rest_index = params.index('.')
            params = params[:rest_index] + params[rest_index+1:]
            args = args[:rest_index] + [args[rest_index:]]
        return params, args

_global_env = _Env()
_global_env.update(ops._builtin_operator)
