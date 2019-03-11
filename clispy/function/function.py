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
#     Function
# ==============================================================================


class Function(object):
    """A function is a construct to structure programs. This is known
    in most programming languages, sometimes also called subroutine or
    procedure. The bulk of lisp itself consists of functions.

    Despite the importance of macros to The Lisp Way, in the end all
    real functionality is provided by functions. Macros run at compile time,
    so the code they generate will consist entirely of calls to functions
    and special operators.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates SpecialOperator.
        """
        cls.__name__ = 'FUNCTION'
        return object.__new__(cls)

    def __repr__(self):
        """The official string representation.
        """
        return "#<FUNCTION {0} {{{1:X}}}>".format(self.__class__.__name__, id(self))
