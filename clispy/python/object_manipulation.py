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
from clispy.type import Cons, Keyword, Null, String, T


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
#     ImportModule  Attribute  Call  Getitem
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


class AttributeObjectManipulation(ObjectManipulation):
    """Get Attribute of python object.
    """
    def __new__(cls, *args, **kwargs):
        """Instaintiates AttributeObjectManipulation.
        """
        cls.__name__ = 'ATTRIBUTE'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of AttributeObjectManipulation.
        """
        args = self.eval_forms(forms, var_env, func_env, macro_env)

        # Sets identifier of python object and attribute.
        py_object = args.car
        py_attr = args.cdr.car

        # Sets an attribute recursively.
        attr = py_object.value
        for py_attr_name in py_attr.value.split('.'):
            attr = getattr(attr, py_attr_name)

        return PyObject(attr)


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
        py_method = args.cdr.car

        # Sets a method recursively.
        method = py_object.value
        for py_method_name in py_method.value.split('.'):
            method = getattr(method, py_method_name)

        # Executes the method.
        rest_args = args.cdr.cdr
        py_args = []
        py_keyword_args = {}

        while rest_args is not Null():
            # Sets keyword aruguments.
            if isinstance(rest_args.car, Keyword):
                # A value of Keyword has ':' at the first character.
                key = rest_args.car.value[1:].lower().replace('-', '_')

                # Next arugment is value.
                rest_args = rest_args.cdr
                value = rest_args.car.value

                # Sets key and value.
                py_keyword_args[key] = value

            else:
                py_args.append(rest_args.car.value)

            rest_args = rest_args.cdr

        return PyObject(method(*py_args, **py_keyword_args))


class GetitemObjectManipulation(ObjectManipulation):
    """Called to implement evaluation of self[key]. For sequence types,
    the accepted keys should be integers and slice objects.
    """
    def __new__(cls, *args, **kwargs):
        """Instaintiates GetitemObjectManipulation.
        """
        cls.__name__ = 'GETITEM'
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        """Behavior of GetitemObjectManipulation.
        """
        args = self.eval_forms(forms, var_env, func_env, macro_env)

        # Sets identifier of python object and rest args.
        py_object = args.car
        rest_args = args.cdr

        # Sets slice objects
        slice_objects = []
        while rest_args is not Null():
            slice_objects.append(rest_args.car.value)
            rest_args = rest_args.cdr

        # If slice_objects has only 1 value, give the value to __getitem__.
        if len(slice_objects) == 1:
            return PyObject(py_object.value.__getitem__(*slice_objects))

        return PyObject(py_object.value.__getitem__(tuple(slice_objects)))


# ==============================================================================
# Set functions related on object manipulation
# ==============================================================================

assign_helper(symbol_name='IMPORT', value=ImportModuleObjectManipulation(), package_name='PYTHON', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='ATTR', value=AttributeObjectManipulation(), package_name='PYTHON', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='CALL', value=CallObjectManipulation(), package_name='PYTHON', env='FUNCTION', status=':EXTERNAL')
assign_helper(symbol_name='ITEM', value=GetitemObjectManipulation(), package_name='PYTHON', env='FUNCTION', status=':EXTERNAL')
