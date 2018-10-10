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
        return mcs.get_class('STANDARD-OBJECT', name, bases, class_dict)

    @classmethod
    def get_class(mcs, mcs_name, name, bases, class_dict):
        """A method to initialize StandardObject, Class, BuiltInClass or SymbolClass etc.
        """
        # Sets meta-class name.
        mcs.__name__ = mcs_name

        # Creates a class object.
        cls = type.__new__(mcs, name, bases, class_dict)

        # object_table manage own objects as weak reference for gc.
        # If objects are needed, access cls.object_registry.
        cls.object_registry = WeakValueDictionary()

        return cls

    @classmethod
    def get_instance(mcs, cls, cls_name, *args, **kwargs):
        """Initializes StandardObject. If an instance of StandardObject is already existed
        in object_registry, returns the instance. Otherwise, a new instance is made.
        """
        # Sets class name.
        cls.__name__ = cls_name

        # Sets the seed and the object key for object_registry.
        seed = ('_'.join([str(cls), str(args[0]), str(id(args[0]))])).encode('utf-8')
        object_key = sha1(seed).hexdigest()

        # Gets a class object.
        if object_key in cls.object_registry:
            return cls.object_registry[object_key]
        else:
            self = object.__new__(cls)  # !!! self makes a reference temporarily for weak reference !!!
            cls.object_registry[object_key] = self
            return self

    @classmethod
    def class_of(mcs):
        """Returns the class of which the object is a direct instance.
        """
        return mcs.__class__

    @classmethod
    def type_of(mcs):
        """Returns a type specifier for a type that has the objects as an element.
        """
        return Symbol(mcs.__name__)


class Class(StandardObject):
    """The type class represents objects that determine the structure of their instances.
    """
    def __new__(mcs, name, bases, class_dict):
        """Instantiates Class.
        """
        return mcs.get_class('CLASS', name, bases, class_dict)


class BuiltInClass(Class):
    """A built-in class is a class whose instances have restricted capabilities or
    special representations.
    """
    def __new__(mcs, name, bases, class_dict):
        """Instantiates BuiltInClass.
        """
        return mcs.get_class('BUILT-IN-CLASS', name, bases, class_dict)


class SymbolClass(BuiltInClass):
    """SymbolClass is meta-class of a symbol object in clispy.
    """
    def __new__(mcs, name, bases, class_dict):
        # Gets a class object and sets meta-class name to 'BUILT-IN-CLASS'.
        cls = mcs.get_class('BUILT-IN-CLASS', name, bases, class_dict)

        # object_registry manages own objects as strong reference.
        # If objects are needed, access cls.object_registry.
        cls.object_registry = {}

        return cls

    @classmethod
    def get_instance(mcs, cls, cls_name, *args):
        """Instantiates SymbolClass. If an instance of SymbolClass is already existed
        in object_table, returns the instance. Otherwise, a new instance is made.
        """
        # Sets class name.
        cls.__name__ = cls_name

        # Sets the seed and the object key for object_registry.
        object_key = str(args[0]).upper()

        # Gets a class object.
        if object_key in cls.object_registry:
            return cls.object_registry[object_key]
        else:
            self = object.__new__(cls)
            cls.object_registry[object_key] = self  # Set object into symbol table.
            return self


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
        self._value = value

    def __repr__(self):
        """The official string representation.
        """
        return 'T'

    @property
    def value(self):
        """Getter for self.__value.
        """
        return self._value

    @classmethod
    def class_of(cls):
        """Returns the class of which the object is a direct instance.
        """
        return cls.__class__

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
        self._value = value

    def __repr__(self):
        """The official string representation.
        """
        return 'NIL'


class Symbol(T, metaclass=SymbolClass):
    """Symbols are used for their object identity to name various entities
    in Common Lisp, including (but not limited to) linguistic such as
    variables and functions.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates Symbol. If an instance of Symbol is already existed
        in object_table, returns the instance. Otherwise, a new instance is made.
        """
        return SymbolClass.get_instance(cls, 'SYMBOL', *args)

    def __init__(self, value):
        """Initializes Symbol.

        Args:
             value: String. It could be converted into uppercase.
        """
        if not isinstance(value, str):
            raise TypeError("The value " + str(value) + " is not of type str")
        self._value = value.upper()

    def __repr__(self):
        """The official string representation.
        """
        return self.value
