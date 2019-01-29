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


from hashlib import sha1
from weakref import WeakValueDictionary


# ==============================================================================
# Defines meta-classes.
#
#     STANDARD-OBJECT
#     CLASS
#     BUILT-IN-CLASS
#     SYMBOL-CLASS
# ==============================================================================


class StandardObject(type):
    """The standard-object is an instance of standard-class and is a superclass of
    every class that is an instance of standard-class except itself.
    """
    def __new__(mcs, name, bases, class_dict):
        """Instantiates StandardObject.
        """
        # Creates a class object.
        cls = type.__new__(mcs, name, bases, class_dict)

        # object_table manage own objects as weak reference for gc.
        # If objects are needed, access cls.object_registry.
        cls.object_registry = WeakValueDictionary()

        return cls

    def __repr__(cls):
        """The official string representation.
        """
        return "#<STANDARD-OBJECT {0} {{{1:X}}}>".format(cls.__name__, id(cls))

    @classmethod
    def get_instance(mcs, cls, cls_name, *args, **kwargs):
        """Initializes StandardObject. If an instance of StandardObject is already existed
        in object_registry, returns the instance. Otherwise, a new instance is made.
        """
        # Sets class name.
        cls.__name__ = cls_name

        # Sets the seed and the object key for object_registry.
        lst = []
        for arg in args:
            if isinstance(arg, str):
                lst.append(arg)
            elif isinstance(arg, int):
                lst.append(str(arg))
            else:
                lst.extend([str(arg), str(id(arg))])

        seed = '_'.join(lst).encode('utf-8')
        object_key = sha1(seed).hexdigest()

        # Gets a class object.
        if object_key in cls.object_registry:
            return cls.object_registry[object_key]
        else:
            self = object.__new__(cls)  # !!! self makes a reference temporarily for weak reference !!!
            cls.object_registry[object_key] = self
            return self


class Class(StandardObject):
    """The type class represents objects that determine the structure of their instances.
    """
    def __repr__(cls):
        """The official string representation.
        """
        return "#<CLASS {0} {{{1:X}}}>".format(cls.__name__, id(cls))


class BuiltInClass(Class):
    """A built-in class is a class whose instances have restricted capabilities or
    special representations.
    """
    def __repr__(cls):
        """The official string representation.
        """
        return "#<BUILT-IN-CLASS {0} {{{1:X}}}>".format(cls.__name__, id(cls))


# ==============================================================================
# Defines base classes.
#
#     T
#     Nil
#     Symbol
# ==============================================================================


class T(object, metaclass=BuiltInClass):
    """The set of all object. The type T is a supertype of every type,
    including itself. Every object is of type T.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates T. If an instance of T is already existed in object_table,
        return the instance. Otherwise, a new instance is made.
        """
        return BuiltInClass.get_instance(cls, 'BOOLEAN', True)

    def __init__(self, value=True):
        """Initializes an instance of T.
        """
        self.value = value

    def __repr__(self):
        """The official string representation.
        """
        return 'T'

    @classmethod
    def class_of(cls):
        """Returns the class of which the object is a direct instance.
        """
        return cls

    @classmethod
    def type_of(cls):
        """Returns a type specifier for a type that has the objects as an element.
        """
        return Symbol(cls.__name__)


class Nil(T):
    """The set of all object. The type T is a supertype of every type,
    including itself. Every object is of type T.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates Nil. If an instance of Nil is already existed in object_table,
        returns the instance. Otherwise, a new instance is made.
        """
        return BuiltInClass.get_instance(cls, 'NIL', False)

    def __init__(self, value=False):
        """Initializes Nil.
        """
        self.value = value

    def __repr__(self):
        """The official string representation.
        """
        return 'NIL'


class Symbol(T, metaclass=BuiltInClass):
    """Symbols are used for their object identity to name various entities
    in Common Lisp, including (but not limited to) linguistic such as
    variables and functions.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates Symbol. If an instance of Symbol is already existed
        in object_table, returns the instance. Otherwise, a new instance is made.
        """
        return BuiltInClass.get_instance(cls, 'SYMBOL', *args)

    def __init__(self, value):
        """Initializes Symbol.

        Args:
             value: String. It could be converted into uppercase.
        """
        if not isinstance(value, str):
            raise TypeError("The value " + str(value) + " is not of type str")

        if value.isupper():
            self.value = value
        else:
            self.value = '|' + value + '|'

    def __repr__(self):
        """The official string representation.
        """
        return self.value
