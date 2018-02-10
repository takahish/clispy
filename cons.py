# Cons cell is equal to list.
_Cons = list

class _DottedPair(_Cons):
    """Dotted Pair.
    """
    def __init__(self, args):
        _Cons.__init__(self, args)
