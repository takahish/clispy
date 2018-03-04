# Copyright 2018 Takahiro Ishikawa. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# ==============================================================================

class Symbol(str):
    """Symbol class, sub class of str.
    """
    pass

class _SymbolTable():
    """Find or create unique Symbol entry for str s in symbol table.
    """
    def __init__(self):
        """Inits _SymbolTable.
        """
        self.table = {}

    def __getitem__(self, s):
        """Get an item like a dictionary.

        Args:
            s: String.

        Returns:
            Symbol object.
        """
        s = s.upper()
        if s not in self.table:
            self.table[s] = Symbol(s)
        return self.table[s]


# Instance of _SymboleTable.
symbol_table = _SymbolTable()


# Add some symbles to symbol_table.
QUOTE    = symbol_table['quote']
IF       = symbol_table['if']
SETQ     = symbol_table['setq']
DEFUN    = symbol_table['defun']
LAMBDA   = symbol_table['lambda']
PROGN    = symbol_table['progn']
DEFMACRO = symbol_table['defmacro']
FUNCTION = symbol_table['function']
FUNCALL  = symbol_table['funcall']

CONS     = symbol_table['cons']
DOT      = symbol_table['.']
APPEND   = symbol_table['append']

QUASIQUOTE       = symbol_table['quasiquote']
UNQUOTE          = symbol_table['unquote']
UNQUOTE_SPLICING = symbol_table['unquote-splicing']
SHARPQUOTE       = symbol_table['function']

LET = symbol_table['let']
LET_ASTER = symbol_table['let*']
FLET = symbol_table['flet']
LABELS = symbol_table['labels']

# Note uninterned; can't be read
EOF_OBJECT = symbol_table['#<eof-object>']


# Symbol for syntactic sugar of quotes.
QUOTES = {
    "'":  QUOTE,
    "`":  QUASIQUOTE,
    ",":  UNQUOTE,
    ",@": UNQUOTE_SPLICING,
    "#'": SHARPQUOTE
}
