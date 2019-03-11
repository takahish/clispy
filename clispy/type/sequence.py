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
        self.value = Nil(value)

        # Defines car and cdr that are itself
        self.car = self
        self.cdr = self

    def __repr__(self):
        """The official string representation.
        """
        return str(self.value)


class Cons(List):
    """A Cons is compound object having two object called car and cdr.
    Each component can be any object.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates Cons. If an instance of Cons is already existed
        in object_table, returns the instance. Otherwise, a new instance is made.
        """
        if len(args) == 1 and isinstance(args[0], list) and len(args[0]) == 0:
            return Null()  # when Cons([]) is executed
        else:
            return BuiltInClass.get_instance(cls, 'CONS', *args)

    def __init__(self, car, cdr=Null()):
        """Initializes Cons.

        Args:
            car: mix.
            cdr: Cons.
        """
        # If car is list, converted into Cons.
        if isinstance(car, list):
            if len(car) == 0:
                car = Null()
            else:
                car = Cons.tocons(car)

        # If cdr is list, converted into Cons.
        if isinstance(cdr, list):
            if len(cdr) == 0:
                cdr = Null()
            else:
                cdr = Cons.tocons(cdr)

        # Sets car and cdr.
        self.car = car
        self.cdr = cdr

        # Sets value as list
        self.value = self._tolist()

    def __repr__(self):
        """The official string representation.
        """
        return '(' + Cons._repr_helper(self).strip() + ')'

    @classmethod
    def _repr_helper(cls, cons):
        """Helper function for the official string representation.
        """
        if isinstance(cons, Null):        # end of cons
            return ''
        elif not isinstance(cons, Cons):  # for dotted pair
            return '. ' + str(cons)
        else:
            return str(cons.car) + ' ' + cls._repr_helper(cons.cdr)

    def _tolist(self):
        """Returns a python list.
        """
        return Cons._tolist_helper(self, [])

    @classmethod
    def _tolist_helper(cls, cons, acc):
        """Helper function for tolist.

        Args:
            cons: clispy.type.sequence.Cons
            acc: list. an accumulator

        Returns:
            list
        """
        if isinstance(cons, Null):        # end of cons
            return acc
        elif not isinstance(cons, Cons):  # for dotted pair
            acc.append(cons)
            return acc
        else:
            car = cons.car
            if not isinstance(car, Cons):
                acc.append(car)
            else:                         # when car is instance of Cons
                acc.append(cls._tolist_helper(cons.car, []))
            return cls._tolist_helper(cons.cdr, acc)

    @classmethod
    def tocons(cls, lst):
        """tocons converts python list into cons.
        """
        if (len(lst) >= 3) and lst[-2] is Symbol('.'):
            return cls._tocons_helper(lst[:-2], lst[-1])
        else:
            return cls._tocons_helper(lst, Null())

    @classmethod
    def _tocons_helper(cls, lst, cons):
        """Helper function for constructor.

        Args:
            lst: list
            cons: clispy.type.sequence.Cons or clispy.type.sequence.Null

        Returns:
            clispy.type.sequence.Cons
        """
        if len(lst) == 0:                 # end of list
            return cons
        else:
            if not isinstance(lst[-1], list):
                return cls._tocons_helper(lst[:-1], Cons(lst[-1], cons))
            else:                         # when an element is nested list
                return cls._tocons_helper(lst[:-1], Cons(cls.tocons(lst[-1]), cons))
