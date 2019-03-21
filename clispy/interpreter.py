# Copyright 2018, 2019 Takahiro Ishikawa. All Rights Reserved.
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
import random
from clispy.evaluator import Evaluator
from clispy.expander import Expander
from clispy.package import PackageManager
from clispy.parser import InPort, Parser
from clispy.type import String, Symbol


class Interpreter(object):
    # The Zen of Python, by Tim Peters.
    the_zen_of_python = (
        "Beautiful is better than ugly.",
        "Explicit is better than implicit.",
        "Simple is better than complex.",
        "Complex is better than complicated.",
        "Flat is better than nested.",
        "Sparse is better than dense.",
        "Readability counts.",
        "Special cases aren't special enough to break the rules.",
        "Although practicality beats purity.",
        "Errors should never pass silently.",
        "Unless explicitly silenced.",
        "In the face of ambiguity, refuse the temptation to guess.",
        "There should be one-- and preferably only one --obvious way to do it.",
        "Although that way may not be obvious at first unless you're Dutch.",
        "Now is better than never.",
        "Although never is often better than *right* now.",
        "If the implementation is hard to explain, it's a bad idea.",
        "If the implementation is easy to explain, it may be a good idea.",
        "Namespaces are one honking great idea -- let's do more of those!"
    )

    @classmethod
    def repl(cls, prompt='=>', inport=InPort(sys.stdin), out=sys.stdout, zen=True):
        PackageManager.in_package(String("COMMON-LISP-USER"))
        while True:
            try:
                if prompt is not None:
                    # Set prompt.
                    try:
                        prompt = PackageManager.current_package.package_nicknames[0] + '=>'
                    except IndexError:
                        prompt = PackageManager.current_package.package_name + '=>'

                    # Wait input.
                    print(prompt, end=' ', file=out, flush=True)

                # Parse inport.
                forms = Parser.parse(inport)

                # Check eof.
                if forms is Symbol('#<EOF-OJBECT>'):
                    return

                # Expand token.
                forms = Expander.expand(
                    forms,
                    var_env=PackageManager.current_package.env['VARIABLE'],
                    func_env=PackageManager.current_package.env['FUNCTION'],
                    macro_env=PackageManager.current_package.env['MACRO']
                )

                # Evaluate expression.
                retval = Evaluator.eval(
                    forms,
                    var_env=PackageManager.current_package.env['VARIABLE'],
                    func_env=PackageManager.current_package.env['FUNCTION'],
                    macro_env=PackageManager.current_package.env['MACRO']
                )

                # Print return value.
                if out is not None:
                    print(retval, end="\n\n", file=out, flush=True)

            except Interrupt:
                if zen:
                    # Print the zen of python at random.
                    print(random.choices(cls.the_zen_of_python)[0], end="\n\n", file=out, flush=True)
                return

            except Exception as e:
                print("------------------------------------------------------------")
                print("{}: {}".format(type(e).__name__, e), end="\n\n", file=out, flush=True)


class Interrupt(Exception):
    """Intrrupt interpreter.
    """
    pass
