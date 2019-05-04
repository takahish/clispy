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

from clispy.type import Cons, Null


class CallCC(object):
    def __init__(self, proc):
        from clispy.python import PyObject

        self.ball = RuntimeWarning("Sorry, can't continue this continuation any longer.")
        self.ball.retval = Null()
        self.proc = proc
        self.args = Cons(PyObject(Invoke(self)), Null())

    def __call__(self, var_env, func_env, macro_env):
        try:
            return self.proc(self.args, var_env, func_env, macro_env)
        except RuntimeWarning as w:
            if w is self.ball:
                return self.ball.retval
            else:
                raise w

class Invoke(object):
    def __init__(self, callcc):
        self.callcc = callcc

    def __call__(self, retval):
        self.callcc.ball.retval = retval
        raise self.callcc.ball
