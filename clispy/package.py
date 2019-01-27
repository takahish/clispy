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

from clispy.type import BuiltInClass, T, Null, Symbol, Keyword, String


class Environment(dict):
    """A space is a dict of {'var': val} pairs with an outer Namespace.
    """
    def __init__(self, params=(), args=(), outer=None):
        """Initializes namespace with parameters, arguments and outer name space.

        Args:
            params: list.
            args: list.
            outer: clispy.package.Environment.
        """
        # Define outer
        if (outer is not None) and (not isinstance(outer, Environment)):
            raise TypeError('outer must be Environment')
        self.outer = outer

        # Update self
        if isinstance(params, Symbol):
            self.update({params: list(args)})
        else:
            # bind rest parameters for lambda
            if len(args) != len(params):
                raise TypeError('expected %s, given %s, ' % (params, args))
            self.update(zip(params, args))

    def find(self, var):
        """Find the innermost environment where var appears.

        Args:
            var: str. Variable for looking up.

        Returns:
            An environment that have variable. If own space don't have value,
            this method looks up variable from the outer environment.
        """
        if var in self:
            return self
        elif self.outer is None:
            raise LookupError(var)
        else:
            return self.outer.find(var)


class Package(T):
    """Package class for managing name space.
    """
    def __new__(cls, *args, **kwargs):
        """Instantiate Package. If an instance of Package is already existed
        in object_registry, returns the instance. Otherwise, a new instance is made.
        """
        return BuiltInClass.get_instance(cls, 'PACKAGE', *args)

    def __init__(self, package_name):
        """Initialize Package.
        """
        self.package_name = package_name

        # symbol_container.
        # for example, the container is like,
        #     symbol_container = {
        #         symbol_name: [Symbol(symbol_name), Keyword(':INTERNAL'), None]
        #         symbol_name: [Symbol(symbol_name), Keyword(':EXTERNAL'), None]
        #         symbol_name: [Symbol(symbol_name), Keyword(':INHERITED'), package_name]
        #     }
        self.symbol_container = {}

        # Space of environments.
        # There are `VARIABLE`, `FUNCTION` and `MACRO`.
        self.space = {
            'VARIABLE': Environment(),
            'FUNCTION': Environment(),
            'MACRO': Environment()
        }

        # Package use list.
        self.use_list = [self]

    def __repr__(self):
        """The official string representation.
        """
        return "#<PACKAGE " + str(self.package_name) + ">"


class PackageManager(object):
    """PackageManager is static class and managing package objects.
    """
    package_container = {
        'COMMON-LISP': Package('COMMON-LISP'),
        'KEYWORD': Package('KEYWORD'),
        'COMMON-LISP-USER': Package('COMMON-LISP-USER'),
        'PYTHON': Package('PYTHON')
    }

    # Current package is COMMON-LISP.
    current_package = package_container['COMMON-LISP']

    @classmethod
    def find(cls, symbol_designator, env='VARIABLE'):
        """Find the innermost environment where var appears.

        Args:
            symbol_designator: Symbol. a symbol name.
            env: str. a searched environment in package

        Returns:
            An environment that have variable. If own space don't have value,
            this method looks up variable from the outer environment.
        """
        # Get symbol_name.
        symbol_name = symbol_designator.value

        # Split and get symbol_name, package_name and status_check.
        symbol_name, package_name, status_check = cls._split_symbol(symbol_name)

        # Get package. The default is the current package.
        package = cls._get_package(package_name)

        # Get the original package including symbol_name.
        while(True):
            _, status, inherited_package_name = package.symbol_container[symbol_name]
            if status is Keyword(':INHERITED'):
                package = cls._get_package(inherited_package_name)
                _, status, inherited_package_name = package.symbol_container[symbol_name]
            else:
                break

        # Check symbol status.
        if status_check and (status is not Keyword(':EXTERNAL')):
            raise PackageError('The symbol ' + symbol_name+' is not external in the ' + package_name + ' package.')

        return package.space[env].find(symbol_name)

    @classmethod
    def find_symbol(cls, symbol_designator, package_designator=None):
        """find_symbol locates a symbol whose name is symbol_designator in a package. If a symbol named
        symbol_designator is found in package, directly or by inheritance, the symbol found is returned
        as the first value; the second value is as follows:

        :INTERNAL
            If the symbol is present in package as an internal symbol.
        :EXTERNAL
            If the symbol is present in package as an external symbol.
        :INHERITED
            If the symbol is inherited by package through use-package, but is not present in package.

        If no such symbol is accessible in package, both values are nil.

        Args:
            symbol_designator: String. a symbol name.
            package_designator: str. a package name.

        Returns:
            (Symbol, Keyword)
        """
        # Get symbol_name and package_name.
        symbol_name = symbol_designator.value
        package_name = cls._get_package_name(package_designator)

        # Get package. The default is the current package.
        package = cls._get_package(package_name)

        # Extract symbol status included by package.symbol_container.
        if symbol_name in package.symbol_container.keys():
            symbol, status, _ = package.symbol_container[symbol_name]
            return symbol, status
        else:
            return Null(), Null()

    @classmethod
    def intern(cls, symbol_designator, package_designator=None):
        """The first value returned by intern, symbol_designator, is the symbol that was found or created.
        The meaning of the secondary value, status, is as follows:

        :INTERNAL
            The symbol was found and is present in package as an internal symbol.
        :EXTERNAL
            The symbol was found and is present as an external symbol.
        :INHERITED'
            The symbol was found and is inherited via use-package (which implies that the symbol is internal).
        nil
            No pre-existing symbol was found, so one was created.

        Args:
            symbol_designator: String. a symbol name.
            package_designator: String. a package name.

        Returns:
            (Symbol, Keyword)
        """
        # Get symbol_name and package_name.
        symbol_name = symbol_designator.value
        package_name = cls._get_package_name(package_designator)

        # Get package. The default is the current package.
        package = cls._get_package(package_name)

        # Check whether symbol already exists or not.
        # If symbol is not exist, Add symbol to package.symbol_container.
        if symbol_name in package.symbol_container.keys():
            symbol, status, _ = package.symbol_container[symbol_name]
            return symbol, status
        else:
            symbol = Symbol(symbol_name)
            status = Keyword(':INTERNAL')
            package.symbol_container[symbol_name] = [symbol, status, None]
            return symbol, status

    @classmethod
    def export(cls, symbol_designator, package_designator=None):
        """Export makes one or more symbols that are accessible in package (whether directly
        or by inheritance) be external symbols of that package.

        Args:
            symbol_designator: Symbol. a symbol name.
            package_designator: Symbol. a package name.

        Returns:
            T
        """
        # Get symbol_name and package_name.
        symbol_name = symbol_designator.value
        package_name = cls._get_package_name(package_designator)

        # Get package. The default is the current package.
        package = cls._get_package(package_name)

        # Change symbol symbol status to `:EXTERNAL`.
        package.symbol_container[symbol_name][1] = Keyword(':EXTERNAL')

        return T()

    @classmethod
    def import_(cls, symbol_designator, package_designator=None):
        """Import adds symbol or symbols to the internals of package, checking for name conflicts
        with existing symbols either present in package or accessible to it. Once the symbols have
        been imported, they may be referenced in the importing package without the use of a package
        prefix when using the Lisp reader.

        Args:
            symbol_designator: Symbol. a symbol name.
            package_designator: Symbol. a package name that imports symbol represented symbol_name.

        Retruns:
            T
        """
        # Get symbol_name and package_name.
        symbol_name = symbol_designator.value
        package_name = cls._get_package_name(package_designator)

        # Split and get symbol_name, base_package_name and status_check.
        symbol_name, base_package_name, status_check = cls._split_symbol(symbol_name)

        # Get base package instance. The default is the current package.
        base_package = cls._get_package(base_package_name)

        # Get package instance. The default is the current package.
        package = cls._get_package(package_name)

        # Check symbol status.
        _, status, _ = base_package.symbol_container[symbol_name]
        if status_check and (status is not Keyword(':EXTERNAL')):
            raise PackageError('The symbol ' + symbol_name+' is not external in the ' + package_name + ' package.')

        # Import symbol
        package.symbol_container[symbol_name] = [Symbol(symbol_name), Keyword(':INTERNAL'), None]
        try:
            package.space['VARIABLE'][symbol_name] = base_package.space['VARIABLE'][symbol_name]
        except KeyError:
            try:
                package.space['FUNCTION'][symbol_name] = base_package.space['FUNCTION'][symbol_name]
            except KeyError:
                package.space['MACRO'][symbol_name] = base_package.space['MACRO'][symbol_name]

        return T()

    @classmethod
    def in_package(cls, package_designator):
        """Causes the the package named by name to become the current package---that is,
        the value of *package*. If no such package already exists, an error of package-error
        is signaled.

        Args:
            package_designator: Symbol. a package name.

        Returns:
            Package. a current package.
        """
        # Get package_name.
        package_name = package_designator.value

        # Set current package to package shown by package_name.
        cls.current_package = cls.package_container[package_name]

        # Variable *PACKAGE* in COMMON-LISP package is set to a current package.
        common_lisp_package = cls.package_container['COMMON-LISP']
        common_lisp_package.space['VARIABLE']['*PACKAGE*'] = cls.current_package

        return cls.current_package

    @classmethod
    def use_package(cls, package_designator_to_use, package_designator=None):
        """use-package causes package to inherit all the external symbols of packages-to-use.
        The inherited symbols become accessible as internal symbols of package.

        packages-to-use are added to the use list of package if they are not there already.
        All external symbols in packages-to-use become accessible in package as internal
        symbols. use-package does not cause any new symbols to be present in package but
        only makes them accessible by inheritance.

        Args:
            package_designator_to_use: Symbol. a designator for a list of package designators.
            package_designator: Symbol. a package designator. The default is the current package.

        Returns:
            T.
        """
        # Get symbol_name and package_name.
        package_name_to_use = package_designator_to_use.value
        package_name = cls._get_package_name(package_designator)

        # Get package instance. The default is the current package.
        package_to_use = cls._get_package(package_name_to_use)
        package = cls._get_package(package_name)

        for symbol_name, symbol_value in package_to_use.symbol_container.items():
            symbol, status, _ = symbol_value

            # Check status of symbol included by package_to_use.symbol_container.
            if status is not Keyword(':EXTERNAL'):
                continue

            # Add symbol status included by package.symbol_container.
            package.symbol_container[symbol_name] = [symbol, Keyword(':INHERITED'), package_name_to_use]

        # Add package_to_use to package.use_list
        if not package_to_use in  package.use_list:
            package.use_list.append(package_to_use)

        return T()

    @classmethod
    def _get_package_name(cls, package_designator):
        """Extract package name from package_designator. If package_designator is None,
        it returns None. Otherwise it returns package_name included package_designator.

        Args:
            package_designator: Symbol or String. a package name.

        Returns:
            package_name.
        """
        if package_designator is None:
            return None
        else:
            return package_designator.value

    @classmethod
    def _get_package(cls, package_name):
        """Extract package shown by package_name. The default is the current package.

        Args:
            package_name: str. a package name.

        Returns:
            Package.
        """
        # Return Package that shown by package_name.
        # The default is the current package.
        if package_name is None:
            return cls.current_package
        else:
            return cls.package_container[package_name]

    @classmethod
    def _split_symbol(cls, symbol_name):
        """Split symbol_name to symbol_name itself, package_name. At the same time,
        status_check that represents whether checking status is needed or not is returned.

        For examples:
            'COMMON-LISP::CAR'  -> 'CAR', 'COMMON-LISP', False
            'COMMON-LISP:CAR'   -> 'CAR', 'COMMON-LISP', True
            'CAR'               -> 'CAR', None, False
            'KEYWORD::INTERNAL' -> ':INTERNAL', 'KEYWORD', False
            'KEYWORD:INTERNAL'  -> ':INTERNAL', 'KEYWORD', False
            ':INTERNAL'         -> ':INTERNAL', 'KEYWORD', False

        Args:
            symbol_name: str. a symbol name.

        Retruns:
            symbol_name that have no package_name, package_name and status_check.
        """

        if '::' in symbol_name:
            # Split symbol_name to package_name and symbol_name
            package_name, symbol_name = (value for value in symbol_name.split('::'))
            status_check = False
            # When package_name is 'KEYWORD'.
            if package_name == 'KEYWORD':
                symbol_name = ':' + symbol_name
        elif ':' in symbol_name and symbol_name.index(':') != 0:
            # Split symbol_name to package_name and symbol_name
            package_name, symbol_name = (value for value in symbol_name.split(':'))
            status_check = True
            # When package_name is 'KEYWORD'.
            if package_name == 'KEYWORD':
                symbol_name = ':' + symbol_name
                status_check = False
        elif ':' in symbol_name and symbol_name.index(':') == 0:
            # When symbol_name is 'KEYWORD' and doesn't have package_name.
            package_name = 'KEYWORD'
            status_check = False
        else:
            # When symbol_name doesn't have.
            package_name = None
            status_check = False

        return symbol_name, package_name, status_check


class PackageError(Exception):
    """PackageError is raised in PackageTable.
    """
    def __init__(self, message):
        """Initialize PackageError.
        """
        super().__init__(message)


# ==============================================================================
# Helper functions
# ==============================================================================

def assign_helper(symbol_name, value, package_name, env='VARIABLE', status=':INTERNAL'):
    """Assign helper function. Other modules use this function to assign value.

    Args:
        symbol_name: str. A symbol name.
        value: mix. An assigned value.
        package_name: str. A package name.
        env: str. An environment is selected from 'VARIABLE', 'FUNCTION' or 'MACRO'.
        status: str. A status is selected from ':INTERNAL' or ':EXTERNAL'
    """
    PackageManager.intern(String(symbol_name), String(package_name))
    PackageManager.package_container[package_name].space[env][symbol_name] = value
    if status == ':EXTERNAL':
        PackageManager.export(Symbol(symbol_name), Symbol(package_name))

def use_package_helper(package_name_to_use, package_name):
    """Import helper function. Other modules use this function to import symbol from original package.

    Args:
        symbol_name: str. A symbol name that include package (for example `COMMON-LISP::CAR`).
        package_name: str. A package name that import symbol indicated by a symbol name.
    """
    PackageManager.use_package(Symbol(package_name_to_use), Symbol(package_name))


# ==============================================================================
# Set values related on this module.
# ==============================================================================

# KEYWORD package
assign_helper(symbol_name=':INTERNAL', value=Keyword(':INTERNAL'), package_name='KEYWORD', env='VARIABLE', status=':EXTERNAL')
assign_helper(symbol_name=':EXTERNAL', value=Keyword(':EXTERNAL'), package_name='KEYWORD', env='VARIABLE', status=':EXTERNAL')
assign_helper(symbol_name=':INHERITED', value=Keyword(':INHERITED'), package_name='KEYWORD', env='VARIABLE', status=':EXTERNAL')

# COMMON-LISP package
assign_helper(symbol_name='T', value=T(), package_name='COMMON-LISP', env='VARIABLE', status=':EXTERNAL')
assign_helper(symbol_name='NIL', value=Null(), package_name='COMMON-LISP', env='VARIABLE', status=':EXTERNAL')
assign_helper(symbol_name='*PACKAGE*', value=PackageManager.current_package, package_name='COMMON-LISP', env='VARIABLE', status=':EXTERNAL')

# COMMON-LISP-USER package
use_package_helper(package_name_to_use='COMMON-LISP', package_name='COMMON-LISP-USER')
