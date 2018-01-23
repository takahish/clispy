class _Symbol(str): pass

class _SymbolTable():
    """Find or create unique Symbol entry for str s in symbol table.
    """
    def __init__(self):
        self.table = {}

    def __getitem__(self, s):
        if s not in self.table:
            self.table[s] = _Symbol(s)
        return self.table[s]

_symbol_table = _SymbolTable()

_quote        = _symbol_table['quote']
_if           = _symbol_table['if']
_set          = _symbol_table['set!']
_define       = _symbol_table['define']
_lambda       = _symbol_table['lambda']
_begin        = _symbol_table['begin']
_define_macro = _symbol_table['define-macro']

_quasiquote       = _symbol_table['quasiquote']
_unquote          = _symbol_table['unquote']
_unquote_splicing = _symbol_table['unquote-splicing']

_append = _symbol_table['append']
_cons   = _symbol_table['cons']
_let    = _symbol_table['let']

# Syntactic sugar
_quotes = {
    "'": _quote,
    "`": _quasiquote,
    ",": _unquote,
    ",@": _unquote_splicing
}
