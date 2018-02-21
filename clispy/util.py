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

from clispy import symbol
from clispy import cons


def to_string(x):
    """Convert a Python object back into a Lisp-readable string.

    Args:
        x: Abstract syntax tree of common lisp consisted of list.

    Returns:
        String for display.
    """
    if x is True:
        return 'T'
    elif x is False:
        return 'NIL'
    elif isinstance(x, symbol.Symbol):
        return x
    elif isinstance(x, str):
        return '"%s"' % x.encode('unicode_escape').decode('unicode_escape').replace('"', r'\"')
    elif isinstance(x, cons.DottedPair):
        x = x[:-1] + [symbol.DOT] + [x[-1]]
        return '(' + ' '.join(map(to_string, x)) + ')'
    elif isinstance(x, cons.Cons):
        return '(' + ' '.join(map(to_string, x)) + ')'
    elif isinstance(x, complex):
        return str(x).replace('j', 'i')
    else:
        return str(x)

def require(x, predicate, msg="wrong length"):
    """Signal a syntax error if predicate is false.

    Args:
        x: Abstract syntax tree of common lisp consisted of list.
        predicate: Test whether error or not.
        msg: Message of error.
    """
    if not predicate:
        raise SyntaxError(to_string(x) + ': ' + msg)