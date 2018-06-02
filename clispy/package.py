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

from clispy.symbol import SymbolTable
from clispy.environment import VariableEnvironment, FunctionEnvironment, MacroEnvironment


class Package(object):
    """Package class for managing symbol and environment.
    """
    def __init__(self):
        """Initialize Package.
        """
        self.__intern = SymbolTable()
        self.__extern = SymbolTable()

        self.__var_env = VariableEnvironment()
        self.__func_env = FunctionEnvironment()
        self.__macro_env = MacroEnvironment()

    @property
    def intern(self):
        """Getter for self.__intern valuable.

        Returns:
            SymbolTable.
        """
        return self.__intern

    @intern.setter
    def intern(self, symbol_table):
        """Setter for self.__intern valuable.

        Args:
            symbol_table: SymbolTable.
        """
        if not isinstance(symbol_table, SymbolTable):
            raise PackageError(str(symbol_table) + " must be clispy.symbol.SymbolTable")
        self.__intern = symbol_table

    @property
    def extern(self):
        """Getter for self.__extern valuable.

        Returns:
            SymbolTable.
        """
        return self.__extern

    @extern.setter
    def extern(self, symbol_table):
        """Setter for self.__extern valuable.

        Args:
            symbol_table: SymbolTable.
        """
        if not isinstance(symbol_table, SymbolTable):
            raise PackageError(str(symbol_table) + " must be clispy.symbol.SymbolTable")
        self.__extern = symbol_table

    @property
    def var_env(self):
        """Getter for self.__var_env valuable.

        Returns:
            VariableEnvironment.
        """
        return self.__var_env

    @var_env.setter
    def var_env(self, var_env):
        """Setter for self.__var_env valuable.

        Args:
            var_env: VariableEnvironment.
        """
        if not isinstance(var_env, VariableEnvironment):
            raise PackageError(str(var_env) + " must be clispy.environment.VariableEnvironment")
        self.__var_env = var_env

    @property
    def func_env(self):
        """Getter for self.__func_env valuable.

        Returns:
            FunctionEnvironment.
        """
        return self.__func_env

    @func_env.setter
    def func_env(self, func_env):
        """Setter for self.__func_env valuable.

        Args:
            func_env: FunctionsEnvironment.
        """
        if not isinstance(func_env, FunctionEnvironment):
            raise PackageError(str(func_env) + " must be clispy.environment.FunctionEnvironment")
        self.__func_env = func_env

    @property
    def macro_env(self):
        """Getter for self.__macro_env valuable.

        Returns:
            MacroEnvironment.
        """
        return self.__macro_env

    @macro_env.setter
    def macro_env(self, macro_env):
        """Setter for self.__macro_env valuable.

        Args:
            macro_env: MacroEnvironment.
        """
        if not isinstance(macro_env, MacroEnvironment):
            raise PackageError(str(macro_env) + " must be clispy.environment.MacroEnvironment")
        self.__macro_env = macro_env


class PackageTable(object):
    """PackageTable class for managing package objects.
    """
    def __init__(self):
        """Initialize PackageTable.
        """
        # Default packages are COMMON-LISP-USER, SYSTEM and PYTHON.
        self.__table = {
            'COMMON-LISP-USER': Package(),
            'KEYWORD': Package(),
            'SYSTEM': Package(),
            'PYTHON': Package()
        }

        # Current space is COMMON-LISP-USER.
        self.__current_space = 'COMMON-LISP-USER'

        # Symbol for common-lisp-user.

    @property
    def current_space(self):
        """Getter for self.__current_space.

        Returns:
            Current space.
        """
        return self.__current_space

    @current_space.setter
    def current_space(self, name):
        """Setter for self.__current_space.

        Args:
            name: String.
        """
        if name not in self.__table:
            raise KeyError(name + " is not existed in package table")
        self.__current_space = name

    def __getitem__(self, name):
        """Literal for getting package from package table.

        Args:
            name: String.
        """
        if name not in self.__table:
            raise KeyError(name + " is not existed in package table")
        return self.__table[name]

    def __setitem__(self, name, package):
        """Literal for setting package into package table.

        Args:
            name: String.
            package: Package.
        """
        if name in self.__table:
            raise PackageError(name + " is already existed in package table")

        if not isinstance(package, Package):
            raise PackageError(str(package) + " must be clispy.package.Package")

        self.__table[name] = package


class PackageError(Exception):
    """PackageError is raised in PackageTable.
    """
    def __init__(self, message):
        """Initialize PackageError
        """
        super().__init__(message)
