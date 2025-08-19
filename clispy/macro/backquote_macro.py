from clispy.macro.macro import Macro
from clispy.package import assign_helper
from clispy.type import Cons, Null, Symbol


class BackquoteMacro(Macro):
    """The backquote introduces a template of a data structure to be built."""

    def __new__(cls, *args, **kwargs):
        cls.__name__ = 'BACKQUOTE'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Expand backquote forms."""
        return self.expand_helper(forms.car)

    @classmethod
    def expand_helper(cls, forms):
        """Expand quotes recursively."""
        if not isinstance(forms, Cons):
            # An argument is not an instance of Cons, it is quoted.
            return Cons(Symbol('QUOTE'), Cons(forms, Null()))

        if forms.car is Symbol('UNQUOTE'):
            # Unquote (,)
            return forms.cdr.car
        elif isinstance(forms.car, Cons) and forms.car.car is Symbol('UNQUOTE-SPLICING'):
            # Unquote-splicing (,@)
            return Cons(
                Symbol('APPEND'),
                Cons(forms.car.cdr.car, Cons(cls.expand_helper(forms.cdr), Null())),
            )
        else:
            # Expands recursively and returns cons.
            return Cons(
                Symbol('CONS'),
                Cons(cls.expand_helper(forms.car), Cons(cls.expand_helper(forms.cdr), Null())),
            )


assign_helper(
    symbol_name='BACKQUOTE',
    value=BackquoteMacro(),
    package_name='COMMON-LISP',
    env='MACRO',
    status=':EXTERNAL',
)
