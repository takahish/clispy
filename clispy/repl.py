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
from clispy.symbol import *
from clispy.env import VarEnv, FuncEnv, MacroEnv
from clispy.eval import Evaluator
from clispy.expand import Expander
from clispy.func import BuiltInFunction
from clispy.parse import Parser, InPort
from clispy.utils import to_string


def repl(prompt='clispy> ', inport=InPort(sys.stdin), out=sys.stdout):
    """A prompt-read-eval-print loop."

    Args:
        prompt: Prompt string for read.
        inport: _InPort object.
        out: Output stream.
    """
    sys.stderr.write("CLisPy Version 0.2\n")
    sys.stderr.flush() # flush buffer explicitly

    # Global variable environment.
    global_var_env = VarEnv()

    # Global function environment.
    global_func_env = FuncEnv()
    global_func_env.update(BuiltInFunction())

    # Make instance of Evaluator.
    evaluator = Evaluator(global_var_env, global_func_env)

    # Global macro environment.
    global_macro_env = MacroEnv()

    # Make instance of Expander.
    expander = Expander(evaluator, global_macro_env)

    # Make instance of Parser.
    parser = Parser()

    while True:
        try:
            if prompt:
                sys.stderr.write(prompt)
                sys.stderr.flush() # flush buffer explicitly
            x = expander.expand(parser.parse(inport))
            if x is EOF_OBJECT:
                print(file=out)
                return
            val = evaluator.eval(x)
            if val is not None and out:
                print(to_string(val), file=out)
        except Exception as e:
            print('%s: %s' % (type(e).__name__, e))
