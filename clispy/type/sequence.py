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
from clispy.type.basecls import BuiltInClass, T, Nil, Symbol


# ==============================================================================
# Defines sequence classes.
#
#     Sequence
#     List
#     Cons
#     Null
# ==============================================================================


class Sequence(T):
    """Sequences are ordered collections of objects, called the elements of
    sequence.
    The type Vector and type List are disjoint of type sequence.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates Sequence. If an instance of Sequence is already existed
        in object_table, returns the instance. Otherwise, a new instance is made.
        """
        return BuiltInClass.get_instance(cls, 'SEQUENCE', True)


class List(Sequence):
    """ A List is a chain of conses in which the car of each cons is an
    element of the list, and the cdr of each cons is either the next link
    in the chain or a terminating atom.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates List. If an instance of List is already existed
        in object_table, returns the instance. Otherwise, a new instance is made.
        """
        return BuiltInClass.get_instance(cls, 'LIST', True)


class Cons(List):
    """A Cons is compound object having two object called car and cdr.
    Each component can be any object.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates Cons. If an instance of Cons is already existed
        in object_table, returns the instance. Otherwise, a new instance is made.
        """
        return BuiltInClass.get_instance(cls, 'CONS', *args)

    def __init__(self, car, cdr=None):
        """Initializes Cons.

        Args:
            car: mix.
            cdr: Cons.
        """
        self._car = car
        self._cdr = cdr

    def __repr__(self):
        """The official string representation.
        """
        return '(' + Cons.__repr_helper(self).strip() + ')'

    @property
    def value(self):
        """Getter for self.__value, Lazy evaluation of self.__value.
        """
        try:
            _ = self._value
        except AttributeError:
            print("Info: lazy evaluation of an attribute", file=sys.stderr)
            self._value = self.tolist()
        finally:
            return self._value

    def car(self):
        """Returns an object of car.
        """
        return self._car

    def cdr(self):
        """Returns an object of cdr.
        """
        return self._cdr

    def tolist(self):
        """Returns a python list.
        """
        return Cons.__tolist_helper(self, [])

    @staticmethod
    def __repr_helper(cons):
        """Helper function for the official string representation.
        """
        if isinstance(cons, Null):        # end of cons
            return ''
        elif not isinstance(cons, Cons):  # for dotted pair
            return '. ' + str(cons)
        else:
            return str(cons.car()) + ' ' + Cons.__repr_helper(cons.cdr())

    @staticmethod
    def __tolist_helper(cons, acc):
        """Helper function for tolist.
        """
        if isinstance(cons, Null):        # end of cons
            return acc
        elif not isinstance(cons, Cons):  # for dotted pair
            acc.append(cons)
            return acc
        else:
            car = cons.car()
            if not isinstance(car, Cons):
                acc.append(car)
            else:                         # when car is instance of Cons
                acc.append(Cons.__tolist_helper(cons.car(), []))
            return Cons.__tolist_helper(cons.cdr(), acc)


class Null(Symbol, List):
    """The only object of type null is nil, which represents the empty
    list and can also be notated ().
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates Null. If an instance of Null is already existed
        in object_table, returns the instance. Otherwise, a new instance is made.
        """
        return BuiltInClass.get_instance(cls, 'NULL', False)

    def __init__(self, value=False):
        """Initializes Null.
        """
        self._value = Nil(value)

    def __repr__(self):
        """The official string representation.
        """
        return str(self.value)

    def car(self):
        """Returns an object of car (itself).
        """
        return self

    def cdr(self):
        """Returns an object of cdr (itself).
        """
        return self
