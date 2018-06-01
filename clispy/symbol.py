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


class SymbolTable():
    """Find or create unique Symbol entry for str s in symbol table.
    """
    def __init__(self):
        """Initialize SymbolTable.
        """
        self.__table = {}

    @property
    def table(self):
        return self.__table

    def __getitem__(self, name):
        """Get a symbol from table.

        Args:
            name: String.

        Returns:
            Symbol object.
        """
        if name not in self.__table:
            raise KeyError(name + " is not existed in symbol table")
        return self.__table[name]

    def __setitem__(self, name, symbol_object):
        """Set a symbol into table.

        Args:
            name: String.
            symbol_object: Symbol.
        """
        if name in self.__table:
            raise SymbolError(name + " is already existed in symbol table")

        if not isinstance(symbol_object, Symbol):
            raise SymbolError(str(symbol_object) + " must be clispy.symbol.Symbol")

        self.__table[name] = symbol_object

    def update(self, symbol_dict):
        self.__table.update(symbol_dict)


class SymbolError(Exception):
    """SymbolError is raised by SymbolTable set item.
    """
    def __init__(self, message):
        """Inits SymbolError with message,
        """
        super().__init__(message)


# Instance of _SymboleTable.
symbol_table = SymbolTable()

#########################
# backward compatibility
#########################
symbol_table['NIL'] = Symbol('NIL')
symbol_table['T'] = Symbol('T')
symbol_table['QUOTE'] = Symbol('QUOTE')
symbol_table['IF'] = Symbol('IF')
symbol_table['SETQ'] = Symbol('SETQ')
symbol_table['PROGN'] = Symbol('PROGN')
symbol_table['FUNCTION'] = Symbol('FUNCTION')
symbol_table['LET'] = Symbol('LET')
symbol_table['LET*'] = Symbol('LET*')
symbol_table['FLET'] = Symbol('FLET')
symbol_table['LABLES'] = Symbol('LABELS')
symbol_table['MACROLET'] = Symbol('MACROLET')
symbol_table['BLOCK'] = Symbol('BLOCK')
symbol_table['RETURN-FROM'] = Symbol('RETURN-FROM')
symbol_table['TAGBODY'] = Symbol('TAGBODY')
symbol_table['GO'] = Symbol('GO')
symbol_table['CATCH'] = Symbol('CATCH')
symbol_table['THROW'] = Symbol('THROW')
symbol_table['QUASIQUOTE'] = Symbol('QUASIQUOTE')
symbol_table['UNQUOTE'] = Symbol('UNQUOTE')
symbol_table['UNQUOTE-SPLICING'] = Symbol('UNQUOTE-SPLICING')
symbol_table['SHARPQUOTE'] = Symbol('SHARPQUOTE')
symbol_table['DEFUN'] = Symbol('DEFUN')
symbol_table['LAMBDA'] = Symbol('LAMBDA')
symbol_table['DEFMACRO'] = Symbol('DEFMACRO')
symbol_table['CONS'] = Symbol('CONS')
symbol_table['.'] = Symbol('.')
symbol_table['APPEND'] = Symbol('APPEND')
symbol_table['#<EOF-OJBECT>'] = Symbol('#<EOF-OBJECT>')

# Symbol for boolean.
NIL = symbol_table['NIL']
T = symbol_table['T']

# Symbol for special forms
QUOTE = symbol_table['QUOTE']
IF = symbol_table['IF']
SETQ = symbol_table['SETQ']
PROGN = symbol_table['PROGN']
FUNCTION = symbol_table['FUNCTION']
LET = symbol_table['LET']
LET_ASTER = symbol_table['LET*']
FLET = symbol_table['FLET']
LABELS = symbol_table['LABLES']
MACROLET = symbol_table['MACROLET']
BLOCK = symbol_table['BLOCK']
RETURN_FROM = symbol_table['RETURN-FROM']
TAGBODY = symbol_table['TAGBODY']
GO = symbol_table['GO']
CATCH = symbol_table['CATCH']
THROW = symbol_table['THROW']

# Symbol for quotes.
QUASIQUOTE = symbol_table['QUASIQUOTE']
UNQUOTE = symbol_table['UNQUOTE']
UNQUOTE_SPLICING = symbol_table['UNQUOTE-SPLICING']
SHARPQUOTE = symbol_table['SHARPQUOTE']

# Symbol for syntactic sugar.
QUOTES = {
    "'":  QUOTE,
    "`":  QUASIQUOTE,
    ",":  UNQUOTE,
    ",@": UNQUOTE_SPLICING,
    "#'": SHARPQUOTE
}

# Symbol for others
DEFUN = symbol_table['DEFUN']
LAMBDA = symbol_table['LAMBDA']
DEFMACRO = symbol_table['DEFMACRO']
CONS = symbol_table['CONS']
DOT = symbol_table['.']
APPEND = symbol_table['APPEND']

# Note uninterned; can't be read
EOF_OBJECT = symbol_table['#<EOF-OJBECT>']
