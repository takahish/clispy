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

from clispy.types.t import T


class Symbol(T):
    """Symbols are used for their object identity to name various entities
    in Common Lisp, including (but not limited to) linguistic such as
    variables and functions.
    """
    def __init__(self, value):
        """Initialize Symbol.

        Args:
             value: String. It could be converted into uppercase.
        """
        if not isinstance(value, str):
            raise TypeError("The value " + str(value) + " is not of type str")
        self.__value = value.upper()

    @property
    def value(self):
        """Getter for self.__value.

        Returns:
             String as symbol.
        """
        return self.__value

    def __repr__(self):
        return self.__value


class Keyword(Symbol):
    """The type Keyword includes all symbols interned the KEYWORD package.

    Interning a symbol in the KEYWORD package has three automatic effects:
        1. It causes the symbol to become bound to itself.
        2. It causes the symbol to become an external symbol of the KEYWORD package.
        3. It causes the symbol to become a constant variable.
    """
    pass
