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

import re
import io
from clispy.type import *


class Parser(object):
    """Provide a method to parse input stream
    """
    # End of file object as an indicator
    eof_object = Symbol('#<EOF-OJBECT>')

    # Quotes mapping to convert a character into Symbol
    # TODO: to add reader macros as the same as quotes
    quotes = {
        "'": Symbol('QUOTE'),
        "`": Symbol('QUASIQUOTE'),
        ",": Symbol('UNQUOTE'),
        ",@": Symbol('UNQUOTE-SPLICING'),
        "#'": Symbol('SHARPQUOTE')
    }

    @classmethod
    def parse(cls, in_port):
        """Parse a program: read and expand/error-check it.

        Args:
            in_port: InPort object or string.

        Return:
            Quote expression or atom.
        """
        if isinstance(in_port, str):
            in_port = InPort(io.StringIO(in_port))

        token_list = cls._read(in_port)
        if isinstance(token_list, list):
            return Cons(token_list)
        else:
            return token_list

    @classmethod
    def _convert_to_atom(cls, token):
        """Numbers become numbers; t and nil are booleans; "..." string; otherwise Symbol.

        Args:
            token: token extracted from in_port.

        Returns:
            Int, Float or Symbol.
        """
        try:
            return Integer(token)
        except ValueError:
            try:
                return SingleFloat(token)
            except ValueError:
                if token[0] == '"':
                    return String(token[1:-1])
                else:
                    return Symbol(token.upper())

    @classmethod
    def _read_ahead(cls, token, in_port):
        """Helper function of read.

        Args:
            token: token extracted from inport.
            in_port: _InPort object.

        Returns:
            Quote expression or atom.
        """
        if '(' == token:
            L = []
            while True:
                token = in_port.next_token()
                if token == ')':
                    return L
                else:
                    L.append(cls._read_ahead(token, in_port))
        elif ')' == token:
            raise SyntaxError('unexpected )')
        elif token in cls.quotes:
            return [cls.quotes[token], cls._read(in_port)]
        elif token is cls.eof_object:
            raise SyntaxError('unexpected EOF in list')
        else:
            return cls._convert_to_atom(token)

    @classmethod
    def _read(cls, inport):
        """Read scheme expression from an inport port.

        Args:
            inport: _InPort object.

        Returns:
            Quote expression or atom.
        """
        # body of _read
        token1 = inport.next_token()
        return cls.eof_object if token1 is cls.eof_object else cls._read_ahead(token1, inport)


class InPort(object):
    """An input port. Retains a line of chars.
    """
    tokenizer = r"""\s*(,@|#'|[('`,)]|"(?:[\\].|[^\\"])*"|;.*|[^\s('"`,;)]*)(.*)"""

    def __init__(self, file):
        """Inits _InPort with file pointer.
        """
        self.file = file
        self.line = ''

    def next_token(self):
        """Return the next token, reading new text into line buffer if needed.
        """
        while True:
            if self.line == '':
                self.line = self.file.readline()
            if self.line == '':
                return Parser.eof_object
            token, self.line = re.match(InPort.tokenizer, self.line).groups()
            if token != '' and not token.startswith(';'):
                return token
