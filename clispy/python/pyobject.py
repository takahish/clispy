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

import operator as op
from clispy.type import Symbol, T


class PyObject(T):
    """PyObject is wapper of python objects.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates PyObject.
        """
        # Don't use BuiltInClass.get_instance.
        cls.__name__ = 'PYTHON-OBJECT'
        return object.__new__(cls)

    def __init__(self, value):
        """Initializes PyObject.
        """
        self.value = value

    def __repr__(self):
        """The official string representation.
        """
        return str(self.value)

    def __add__(self, other):
        """Hook of self + other.
        """
        return PyObject.__calculate(op.add, self, other)

    def __sub__(self, other):
        """Hook of self - other.
        """
        return PyObject.__calculate(op.sub, self, other)

    def __mul__(self, other):
        """Hook of self * other.
        """
        return PyObject.__calculate(op.mul, self, other)

    def __truediv__(self, other):
        """Hook of self / other.
        """
        return PyObject.__calculate(op.truediv, self, other)

    @staticmethod
    def __calculate(operator, self, other):
        """Calculate arithmetic.
        """
        return PyObject(operator(self.value, other.value))

    @classmethod
    def class_of(cls):
        """Returns the class of which the object is a direct instance.
        """
        return cls

    @classmethod
    def type_of(cls):
        """Returns a type specifier for a type that has the objects as an element.
        """
        return Symbol('PYTHON-OBJECT')
