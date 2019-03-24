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

from clispy.function import SystemFunction
from clispy.package import assign_helper
from clispy.python import PyObject
from clispy.type import Cons, Null, T


# ==============================================================================
# Defines base classes.
#
#     ObjectManipulation
# ==============================================================================

class ObjectManipulation(SystemFunction):
    """Python object manipulation.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates ObjectManipulation.
        """
        cls.__name__ = 'OBJECT-MANIPULATION'
        return object.__new__(cls)

    def __repr__(self):
        """The official string representation.
        """
        return "#<OBJECT-MANIPULATION {0} {{{1:X}}}>".format(self.__class__.__name__, id(self))


class CallObjectManipulation(ObjectManipulation):
    """Call python object with message (method) and args.
    """
    def __new__(cls, *args, **kwargs):
        """Instaintiates CallObjectManipulation.
        """
        cls.__name__ = 'CALL'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of CallObjectManipulation.
        """
        args = self.eval_forms(forms, var_env, func_env, macro_env)

        # Sets identifier of python object and attribute.
        py_object = args.car
        py_attr = args.cdr.car

        # Sets an attribute
        attr = getattr(py_object.value, py_attr.value)

        # If an attribute is callable, executes the method.
        if callable(attr):
            rest_args = args.cdr.cdr
            py_args = []

            while rest_args is not Null():
                py_args.append(rest_args.car)
                rest_args = rest_args.cdr

            return PyObject(attr(*py_args))

        else:
            return PyObject(attr)


# ==============================================================================
# Set functions related on object manipulation
# ==============================================================================

assign_helper(symbol_name='CALL', value=CallObjectManipulation(), package_name='PYTHON', env='FUNCTION', status=':EXTERNAL')
