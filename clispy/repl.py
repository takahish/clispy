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

import sys
from clispy import parse
from clispy import symbol
from clispy import util
from clispy import expand
from clispy import eval


def repl(prompt='clispy> ', inport=parse._InPort(sys.stdin), out=sys.stdout):
    """A prompt-read-eval-print loop."
    """
    sys.stderr.write("CLisPy Version 0.2\n")
    sys.stderr.flush() # flush buffer explicitly
    while True:
        try:
            if prompt:
                sys.stderr.write(prompt)
                sys.stderr.flush() # flush buffer explicitly
            x = expand._expand(parse._parse(inport))
            if x is symbol._eof_object:
                print(file=out)
                return
            val = eval._eval(x)
            if val is not None and out:
                print(util._to_string(val), file=out)
        except Exception as e:
            print('%s: %s' % (type(e).__name__, e))
