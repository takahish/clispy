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


class LispObject(type):
    """LispObject is MetaClass of all objects in clispy.
    """
    def __new__(mcs, name, bases, class_dict):
        """Initialize LispObject.
        """
        cls = type.__new__(mcs, name, bases, class_dict)

        # object_table manage own objects as weak reference for gc.
        cls.object_registry = WeakValueDictionary()

        return cls

    @classmethod
    def get_instance(mcs, cls, *args, **kwargs):
        """Initialize LispObject. If an instance of LispObject is already existed
        in object_table, return the instance. Otherwise, an instance is made.
        """
        seed = ('_'.join([str(cls), str(args[0]), str(id(args[0]))])).encode('utf-8')
        object_key = sha1(seed).hexdigest()

        if object_key in cls.object_registry:
            return cls.object_registry[object_key]
        else:
            self = object.__new__(cls)  # self make a reference temporarily.
            cls.object_registry[object_key] = self
            return self

    @classmethod
    def class_of(mcs):
        return mcs.__class__

    @classmethod
    def type_of(mcs):
        return Symbol(mcs.__name__)


class SymbolObject(LispObject):
    """SymbolObject is MetaClass of symbol objects in clispy.
    """
    def __new__(mcs, name, bases, class_dict):
        cls = type.__new__(mcs, name, bases, class_dict)

        # object_table manage own objects as strong reference.
        cls.object_registry = {}

        return cls

    @classmethod
    def get_instance(mcs, cls, *args):
        """Initialize SymbolObject. If an instance of SymbolObject is already existed
        in object_table, return the instance. Otherwise, an instance is made.
        """
        object_key = str(args[0]).upper()  # object key is a value of an instance of Symbol.

        if object_key in cls.object_registry:
            return cls.object_registry[object_key]
        else:
            self = object.__new__(cls)
            cls.object_registry[object_key] = self  # Set object into symbol table.
            return self


class T(object, metaclass=LispObject):
    """The set of all object. The type T is a supertype of every type,
    including itself. Every object is of type T.
    """
    def __new__(cls, *args, **kwargs):
        """Initialize T. If an instance of T is already existed in object_table,
        return the instance. Otherwise, an instance is made.
        """
        cls.__name__ = 'BOOLEAN'
        return LispObject.get_instance(cls, True)

    def __init__(self, value=True):
        """Initialize an instance of T.
        """
        self.__value = value

    @property
    def value(self):
        """Getter for self.__value.
        """
        return self.__value

    def __repr__(self):
        """The official string representation.
        """
        return 'T'

    @classmethod
    def class_of(cls):
        return cls.__class__

    @classmethod
    def type_of(cls):
        """Return a type specifier.
        """
        return Symbol(cls.__name__)


class Nil(T):
    """The set of all object. The type T is a supertype of every type,
    including itself. Every object is of type T.
    """
    def __new__(cls, *args, **kwargs):
        """Initialize Nil. If an instance of Nil is already existed in object_table,
        return the instance. Otherwise, an instance is made.
        """
        cls.__name__ = 'NIL'
        return LispObject.get_instance(cls, False)

    def __init__(self, value=False):
        """Initialize Nil.
        """
        self.__value = value

    @property
    def value(self):
        """Getter for self.__value.
        """
        return self.__value

    def __repr__(self):
        """The official string representation.
        """
        return 'NIL'


class Symbol(T, metaclass=SymbolObject):
    """Symbols are used for their object identity to name various entities
    in Common Lisp, including (but not limited to) linguistic such as
    variables and functions.
    """
    def __new__(cls, *args, **kwargs):
        """Initialize Symbol. If an instance of Symbol is already existed
        in object_table, return the instance. Otherwise, an instance is made.
        """
        cls.__name__ = 'SYMBOL'
        return SymbolObject.get_instance(cls, *args)

    def __init__(self, value):
        """Initialize Symbol.

        Args:
             value: String. It could be converted into uppercase.
        """
        if not isinstance(value, str):
            raise TypeError("The value " + str(value) + " is not of type str")
        self.__value = value.upper()

    @property
    def value(self):
        """Getter for self.__value.
        """
        return self.__value

    def __repr__(self):
        """The official string representation.
        """
        return self.value
