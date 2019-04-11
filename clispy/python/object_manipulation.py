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

from importlib import import_module
from clispy.function import SystemFunction
from clispy.package import PackageManager, assign_helper
from clispy.python import PyObject
from clispy.type import Cons, Null, String, T


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


# ==============================================================================
# Defines object manipulation classes
#
#     ImportModule  Call
# ==============================================================================

class ImportModuleObjectManipulation(ObjectManipulation):
    """Assigns modules to variables like setq special operator.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiates ImportModuleObjectManipulation.
        """
        cls.__name__ = 'IMPORT-MODULE'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of ImportModuleObjectManipulation.
        """
        while forms is not Null():
            symbol, module_name, forms = forms.car, forms.cdr.car, forms.cdr.cdr

            symbol_name = symbol.value
            module = PyObject(import_module(module_name.value))

            # Interns symbol that represents module name into current package.
            PackageManager.intern(String(symbol_name))

            # import-moduel may be used for assignment of both lexical and dynamic variables.
            try:
                var_env.find(symbol_name)[symbol_name] = module
            except LookupError:
                try:
                    # package_name=None means finding an environment from current package.
                    PackageManager.find(symbol_name, package_name=None, status_check=False)[symbol_name] = module
                except LookupError:
                    PackageManager.current_package.env['VARIABLE'][symbol_name] = module

        # the primary value of the last form
        return symbol


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

        # Sets an attribute recursively.
        attr = py_object.value
        for py_attr_name in py_attr.value.split('.'):
            attr = getattr(attr, py_attr_name)

        # If an attribute is callable, executes the method.
        if callable(attr):
            rest_args = args.cdr.cdr
            py_args = []

            while rest_args is not Null():
                py_args.append(rest_args.car.value)
                rest_args = rest_args.cdr

            return PyObject(attr(*py_args))

        else:
            return PyObject(attr)


# ==============================================================================
# Set functions related on object manipulation
# ==============================================================================

assign_helper(symbol_name='IMPORT-MODULE', value=ImportModuleObjectManipulation(), package_name='PYTHON', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='CALL', value=CallObjectManipulation(), package_name='PYTHON', env='FUNCTION', status=':EXTERNAL')
