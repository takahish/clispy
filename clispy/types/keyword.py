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

from clispy.types.base import Symbol, SymbolObject


class Keyword(Symbol):
    """The type Keyword includes all symbols interned the KEYWORD package.

    Interning a symbol in the KEYWORD package has three automatic effects:
        1. It causes the symbol to become bound to itself.
        2. It causes the symbol to become an external symbol of the KEYWORD package.
        3. It causes the symbol to become a constant variable.
    """
    def __new__(cls, *args, **kwargs):
        """Initialize Keyword. If an instance of Keyword is already existed
        in object_table, return the instance. Otherwise, an instance is made.
        """
        cls.__name__ = 'KEYWORD'
        return SymbolObject.get_instance(cls, *args)
