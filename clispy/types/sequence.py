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
from clispy.types.base import LispObject, T, Nil, Symbol


class Sequence(T):
    """Sequences are ordered collections of objects, called the elements of
    sequence.
    The type Vector and type List are disjoint of type sequence.
    """
    def __new__(cls, *args, **kwargs):
        """Initialize Sequence. If an instance of Sequence is already existed
        in object_table, return the instance. Otherwise, an instance is made.
        """
        cls.__name__ = 'SEQUENCE'
        return LispObject.get_instance(cls, True)


class List(Sequence):
    """ A List is a chain of conses in which the car of each cons is an
    element of the list, and the cdr of each cons is either the next link
    in the chain or a terminating atom.
    """
    def __new__(cls, *args, **kwargs):
        """Initialize List. If an instance of List is already existed
        in object_table, return the instance. Otherwise, an instance is made.
        """
        cls.__name__ = 'LIST'
        return LispObject.get_instance(cls, True)


class Cons(List):
    """A Cons is compound object having two object called car and cdr.
    Each component can be any object.
    """
    def __new__(cls, *args, **kwargs):
        """Initialize Cons. If an instance of Cons is already existed
        in object_table, return the instance. Otherwise, an instance is made.
        """
        cls.__name__ = 'CONS'
        return LispObject.get_instance(cls, *args)

    def __init__(self, car, cdr):
        """Initialize Cons.

        Args:
            car: mix.
            cdr: Cons.
        """
        self.__car = car
        self.__cdr = cdr

    @property
    def value(self):
        """Getter for self.__value, Lazy evaluation of self.__value.
        """
        try:
            _ = self.__value
        except AttributeError:
            print("Info: lazy evaluation of an attribute", file=sys.stderr)
            self.__value = self.tolist()
        finally:
            return self.__value

    def __repr__(self):
        """The official string representation.
        """
        return '(' + Cons.__repr_helper(self).strip() + ')'

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

    def car(self):
        """Return an object of car.
        """
        return self.__car

    def cdr(self):
        """Return an object of cdr.
        """
        return self.__cdr

    def tolist(self):
        """Return a python list.
        """
        return Cons.__tolist_helper(self, [])

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
        """Initialize Null. If an instance of Null is already existed
        in object_table, return the instance. Otherwise, an instance is made.
        """
        cls.__name__ = 'NULL'
        return LispObject.get_instance(cls, False)

    def __init__(self, value=False):
        """Initialize Null.
        """
        self.__value = Nil(value)

    @property
    def value(self):
        """Getter for self.__value.
        """
        return self.__value

    def __repr__(self):
        """The official string representation.
        """
        return str(self.value)

    def car(self):
        """Return an object of car (itself).
        """
        return self

    def cdr(self):
        """Return an object of cdr (itself).
        """
        return self
