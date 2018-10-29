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

import re
import numpy as np
from clispy.type.basecls import BuiltInClass, T
from clispy.type.sequence import Sequence


# ==============================================================================
# Defines array classes.
#
#     Array
#     Vector
#     String
# ==============================================================================


class Array(T):
    """An array contains objects arranged according to a Cartesian coordinate
    system. An array provides mappings from a set of fixnums {i0,i1,...,ir-1}
    to corresponding elements of the array, where 0 <=ij < dj, r is the rank
    of the array, and dj is the size of dimension j of the array.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiate Array.
        """
        return BuiltInClass.get_instance(cls, 'ARRAY', *args)

    def __init__(self, value):
        """Initialize Array. A value must be np.ndarray.

        Args:
           value: np.ndarray.
        """
        if not isinstance(value, np.ndarray):
            raise TypeError("The value " + str(value) + " must be numpy.ndarray")
        self.value = value

        # set a header for official string representation
        dimension = len(self.value.shape)
        if dimension == 1:
            self.__repr_head = '#'
        else:
            self.__repr_head = '#' + str(dimension) + 'A'

    def __repr__(self):
        """The official string representation.
        """
        return re.sub(
            r"\s+",
            " ",
            self.__repr_head + str(self.value).replace('[', '(').replace(']', ')').replace('\n', '')
        )


class Vector(Array, Sequence):
    """The type Vector is a subtype of type Array; for all types x, (vector x)
    is the same as (array x (*)).
    """
    def __new__(cls, *args, **kwargs):
        """Instantiate Vector.
        """
        return BuiltInClass.get_instance(cls, 'VECTOR', *args)

    def __init__(self, value):
        """Initialize Vector. A value must be np.ndarray and have one dimension.

        Args:
            value: np.ndarray.
        """
        if not isinstance(value, np.ndarray):
            raise TypeError("the value " + str(value) + " must be numpy.ndarray")
        elif not len(value.shape) == 1:
            raise ValueError("The dimension of value must be one")
        self.value = value

    def __repr__(self):
        """The official string representation.
        """
        return re.sub(
            r"\s+",
            " ",
            '#' + str(self.value).replace('[', '(').replace(']', ')').replace('\n', '')
        )


class String(Vector):
    """A string is a specialized vector whose elements are of type character
    or a subtype of type character.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiate String.
        """
        return BuiltInClass.get_instance(cls, 'String', *args)

    def __init__(self, value):
        """Initialize String. A value must be str.

        Args:
            value: str.
        """
        if not isinstance(value, str):
            raise TypeError("the value " + str(value) + " is not of type str")
        self.value = value

    def __repr__(self):
        """The official string representation.
        """
        return '"' + self.value + '"'
