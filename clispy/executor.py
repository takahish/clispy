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
import glob
from clispy.symbol import *
from clispy.environment import VariableEnvironment, FunctionEnvironment, MacroEnvironment
from clispy.evaluator import Evaluator
from clispy.expander import Expander
from clispy.functions import BuiltInFunction
from clispy.parser import Parser, InPort
from clispy.utilities import to_string


class Executor(object):
    """Provide a method of console. See also clispy/__init.py.
    """
    def __init__(self):
        """Inits Console
        """
        # Global variable environment.
        global_var_env = VariableEnvironment()

        # Global function environment.
        global_func_env = FunctionEnvironment()
        global_func_env.update(BuiltInFunction())

        # Make instance of Evaluator.
        self.evaluator = Evaluator(global_var_env, global_func_env)

        # Global macro environment.
        global_macro_env = MacroEnvironment()

        # Make instance of Expander.
        self.expander = Expander(self.evaluator, global_macro_env)

        # Make instance of Parser.
        self.parser = Parser()

    def console(self, prompt='clispy> ', inport=InPort(sys.stdin), out=sys.stdout):
        """A prompt of read-eval-print-loop.

        Args:
            prompt: Prompt string for read.
            inport: InPort object.
            out: Output stream.
        """
        if prompt:
            sys.stderr.write("CLisPy Version 0.2\n")
            sys.stderr.flush()  # flush buffer explicitly

        while True:
            try:
                if prompt:
                    sys.stderr.write(prompt)
                    sys.stderr.flush()  # flush buffer explicitly

                # Parse inport.
                token = self.parser.parse(inport)
                if token is EOF_OBJECT:
                    if prompt:
                        print(file=out)
                    return

                # Expand token.
                x = self.expander.expand(token)

                # Evaluate expression.
                val = self.evaluator.eval(x)
                if val is not None and out:
                    print(to_string(val), file=out)

            except Exception as e:
                print('%s: %s' % (type(e).__name__, e))

    def load(self, directory_path):
        """Load bootstrap common lisp files.

        Args:
            directory_path: Path of directory that contain common lisp files.
        """
        if directory_path[-1] != "/":
            directory_path = directory_path + "/"

        file_path = directory_path + '*.lisp'
        files = glob.glob(file_path)

        for file in files:
            with open(file) as f:
                self.console(prompt=None, inport=InPort(f), out=None)
