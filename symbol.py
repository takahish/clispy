class _Symbol(str): pass

class _SymbolTable():
    """Find or create unique Symbol entry for str s in symbol table.
    """
    def __init__(self):
        self.table = {}

    def __getitem__(self, s):
        s = s.upper()
        if s not in self.table:
            self.table[s] = _Symbol(s)
        return self.table[s]

_symbol_table = _SymbolTable()

_quote    = _symbol_table['quote']
_if       = _symbol_table['if']
_setq     = _symbol_table['setq']
_defun    = _symbol_table['defun']
_lambda   = _symbol_table['lambda']
_progn    = _symbol_table['progn']
_defmacro = _symbol_table['defmacro']
_function = _symbol_table['function']
_funcall  = _symbol_table['funcall']

_cons     = _symbol_table['cons']
_dot      = _symbol_table['.']
_append   = _symbol_table['append']

_quasiquote       = _symbol_table['quasiquote']
_unquote          = _symbol_table['unquote']
_unquote_splicing = _symbol_table['unquote-splicing']
_sharpquote       = _symbol_table['function']

# Syntactic sugar
_quotes = {
    "'":  _quote,
    "`":  _quasiquote,
    ",":  _unquote,
    ",@": _unquote_splicing,
    "#'": _sharpquote
}

# Note uninterned; can't be read
_eof_object = _symbol_table['#<eof-object>']
