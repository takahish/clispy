# Copyright 2019 Takahiro Ishikawa. All Rights Reserved.
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


# ==============================================================================
# Defines base classes.
#
#     Macro
# ==============================================================================


class Macro(object):
    """The Common Lisp macro facility allows the user to define arbitrary
    functions that convert certain Lisp forms into different forms before
    evaluating or compiling them. This is done at the expression level,
    not at the character-string level as in most other languages. Macros
    are important in the writing of good code: they make it possible to
    write code that is clear and elegant at the user level but that is
    converted to a more complex or more efficient internal form for execution.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates Macro.
        """
        cls.__name__ = 'MACRO'
        return object.__new__(cls)

    def __repr__(self):
        """The official string representation.
        """
        return "#<MACRO {0} {{{1:X}}}>".format(self.__class__.__name__, id(self))
