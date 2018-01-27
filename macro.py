import symbol

class _MacroTable():
    """A macro table: a dict of {'name': proc} pairs.
    """
    def __init__(self, symbol_table=symbol._symbol_table):
        self.symbol_table = symbol_table
        self.table = {}

    def __setitem__(self, name, proc):
        n = self.symbol_table[name]
        self.table[n] = proc

    def __getitem__(self, name):
        if name in self.table:
            return self.table[name]
        else:
            raise LookupError(name)

_macro_table = _MacroTable()
