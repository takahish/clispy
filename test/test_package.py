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

import unittest
from clispy.type import BuiltInClass, T, Null, Symbol, Keyword, String
from clispy.package import Environment, Package, PackageManager


class EnvironmentUnitTestCase(unittest.TestCase):
    def testEnvironment(self):
        env = Environment()

        # Check instance.
        self.assertIsInstance(env, Environment)

    def testEnvironment_find(self):
        env = Environment()

        # Add key-value pair for testing `find` method.
        env['KEY'] = 'VAL'

        # `find` method must return self.
        self.assertEqual(env.find('KEY'), env)

    def testEnvironment_find_value(self):
        env = Environment()

        # Add key-value pair for testing `find` method.
        env['KEY'] = 'VAL'

        # `find` method must return self and we can get value for key.
        self.assertEqual(env.find('KEY')['KEY'], 'VAL')

    def testEnvironment_find_outer(self):
        # Set outer environment.
        outer = Environment()
        env = Environment(outer=outer)

        # Add key-value pari to outer environment.
        env.outer['OUTER_KEY'] = ['OUTER_VAL']

        # `find` method must retrun outer environment.
        self.assertEqual(env.find('OUTER_KEY'), outer)

    def testEnvironment_find_outer_value(self):
        # Set outer environment.
        outer = Environment()
        env = Environment(outer=outer)

        # Add key-value pari to outer environment.
        env.outer['OUTER_KEY'] = 'OUTER_VAL'

        # `find` method must retrun outer environment and we can get outer value for outer key.
        self.assertEqual(env.find('OUTER_KEY')['OUTER_KEY'], 'OUTER_VAL')

    def testEnvironment_find_raise_exception(self):
        env = Environment()

        # Raise exception if key-value pair does not exist.
        self.assertRaises(LookupError, env.find, 'KEY')

    def testEnvironment_set_params(self):
        # Make instant with params and args.
        env = Environment(params=['KEY_1', 'KEY_2'], args=['VAL_1', 'VAL_2'])

        # `find` method must return self and we can get values for keys.
        self.assertEqual(env.find('KEY_1')['KEY_1'], 'VAL_1')
        self.assertEqual(env.find('KEY_2')['KEY_2'], 'VAL_2')

    def testEnvironment_set_params_raise_exception(self):
        # Raise exception when length of params and args is deffirerent.
        self.assertRaises(TypeError, Environment, ['KEY_1', 'KEY_2'], ['VAL_1'])

    def testEnvironment_extend(self):
        env = Environment()

        # A environment that have itself as an outer environment.
        extended_env = env.extend()

        # An outer environment must be itself.
        self.assertTrue(extended_env.outer is env)


class PackageTestCase(unittest.TestCase):
    def testPackage(self):
        # Make instance with package_name 'COMMON-LISP'.
        pkg = Package(package_name='COMMON-LISP')

        # Check instance.
        self.assertIsInstance(pkg, Package)

    def testPackageObject(self):
        # Make instance of two 'A' package and one 'B' package.
        a = Package('A')
        b = Package('B')

        # Check objects.
        self.assertFalse(a is b)

    def testPackage_symbol_container(self):
        # Make instance with package_name 'COMMON-LISP'.
        pkg = Package(package_name='COMMON-LISP')

        # Check instance.
        self.assertIsInstance(pkg.symbol_container, dict)

    def testPackage_space(self):
        # Make instance with package_name 'COMMON-LISP'.
        pkg = Package(package_name='COMMON-LISP')

        # The space have three environments ('VARIABLE', 'FUNCTION' and 'MACRO')
        self.assertEqual(len(pkg.env), 3)
        self.assertIsInstance(pkg.env['VARIABLE'], Environment)
        self.assertIsInstance(pkg.env['FUNCTION'], Environment)
        self.assertIsInstance(pkg.env['MACRO'], Environment)

    def testPackage_repr(self):
        # Make instance with package_name 'COMMON-LISP'.
        pkg = Package(package_name='COMMON-LISP')

        # Check official representation.
        self.assertEqual(str(pkg), '#<PACKAGE COMMON-LISP>')


class PackageManagerTestCase(unittest.TestCase):
    def testPackageManager_package_container(self):
        # PackageManager is static class.
        # `package_container` has four Package objects as default.
        self.assertEqual(len(PackageManager.package_container), 4)
        self.assertIsInstance(PackageManager.package_container['COMMON-LISP'], Package)
        self.assertIsInstance(PackageManager.package_container['KEYWORD'], Package)
        self.assertIsInstance(PackageManager.package_container['COMMON-LISP-USER'], Package)
        self.assertIsInstance(PackageManager.package_container['PYTHON'], Package)

    def testPackageManager_current_package(self):
        # PackageManager.current_package is 'COMMON-LISP-USER' as default.
        self.assertIsInstance(PackageManager.current_package, Package)
        self.assertTrue(PackageManager.current_package is PackageManager.package_container['COMMON-LISP-USER'])

    def testPackageManager_get_package_name(self):
        # PackageManager.get_package_name returns package name represented by package_designator.

        # IF package_designator is None, it returns None.
        self.assertTrue(PackageManager.get_package_name(package_designator=None) is None)

        # Then package_designator is String or Symbol, it returns package name.
        self.assertTrue(PackageManager.get_package_name(String('COMMON-LISP')) is 'COMMON-LISP')
        self.assertTrue(PackageManager.get_package_name(Symbol('COMMON-LISP')) is 'COMMON-LISP')

    def testPackageManager_get_package(self):
        # PackageManager.get_package returns package represented by package_name.
        # If package_name is None, it returns PackageManager.current_package.
        self.assertTrue(PackageManager.get_package(package_name=None) is PackageManager.current_package)

        # Check four packages included in PackageManager.package_container as defualt.
        self.assertTrue(PackageManager.get_package(package_name='COMMON-LISP') is PackageManager.package_container['COMMON-LISP'])
        self.assertTrue(PackageManager.get_package(package_name='KEYWORD') is PackageManager.package_container['KEYWORD'])
        self.assertTrue(PackageManager.get_package(package_name='COMMON-LISP-USER') is PackageManager.package_container['COMMON-LISP-USER'])
        self.assertTrue(PackageManager.get_package(package_name='PYTHON') is PackageManager.package_container['PYTHON'])

        # Raise an exception of KeyError if package_name dose not exist in keys of
        # PackageMnager.package_container.
        self.assertRaises(KeyError, PackageManager.get_package, 'NOT-EXIST')

    def testPackageManager_get_package_name_from_nickname(self):
        # PackageManager.get_package returns package represented by nickname.
        self.assertTrue(PackageManager.get_package(package_name='CL') is PackageManager.package_container['COMMON-LISP'])
        self.assertTrue(PackageManager.get_package(package_name='CL-USER') is PackageManager.package_container['COMMON-LISP-USER'])
        self.assertTrue(PackageManager.get_package(package_name='PY') is PackageManager.package_container['PYTHON'])

    def testPackageManager_split_symbol_name(self):
        # PackageManager.split_symbol_name splits symbol_name including package_name
        # and returns status_check indicating whether it needs status check or not.

        # When symbol_name is not KEYWORD,

        symbol_name, package_name, status_check = PackageManager.split_symbol_name('COMMON-LISP::CAR')
        self.assertEqual(symbol_name, 'CAR')
        self.assertEqual(package_name, 'COMMON-LISP')
        self.assertFalse(status_check)

        symbol_name, package_name, status_check = PackageManager.split_symbol_name('COMMON-LISP:CAR')
        self.assertEqual(symbol_name, 'CAR')
        self.assertEqual(package_name, 'COMMON-LISP')
        self.assertTrue(status_check)

        symbol_name, package_name, status_check = PackageManager.split_symbol_name('CAR')
        self.assertEqual(symbol_name, 'CAR')
        self.assertEqual(package_name, None)
        self.assertFalse(status_check)

        # When symbol_name is KEYWORD,

        sybmol_name, package_name, status_check = PackageManager.split_symbol_name('KEYWORD::EXTERNAL')
        self.assertEqual(sybmol_name, ':EXTERNAL')
        self.assertEqual(package_name, 'KEYWORD')
        self.assertFalse(status_check)

        sybmol_name, package_name, status_check = PackageManager.split_symbol_name('KEYWORD:EXTERNAL')
        self.assertEqual(sybmol_name, ':EXTERNAL')
        self.assertEqual(package_name, 'KEYWORD')
        self.assertFalse(status_check)

        sybmol_name, package_name, status_check = PackageManager.split_symbol_name(':EXTERNAL')
        self.assertEqual(sybmol_name, ':EXTERNAL')
        self.assertEqual(package_name, 'KEYWORD')
        self.assertFalse(status_check)

    def testPackageManager_find(self):
        # PackageManger.find results an environment that have value assigned by symbol.
        # The package that have the environment is shown by package_name.

        # symbol_name, package_name, status_check is given by PackageManger.split_symbol.

        # Checks an environment using *PACKAGE* in current package.

        symbol_name, package_name, status_check = PackageManager.split_symbol_name('COMMON-LISP::*PACKAGE*')
        env = PackageManager.find(symbol_name, package_name, status_check)
        self.assertIsInstance(env, Environment)
        self.assertTrue('*PACKAGE*' in env)

        symbol_name, package_name, status_check = PackageManager.split_symbol_name('COMMON-LISP:*PACKAGE*')
        env = PackageManager.find(symbol_name, package_name, status_check)
        self.assertIsInstance(env, Environment)
        self.assertTrue('*PACKAGE*' in env)

        symbol_name, package_name, status_check = PackageManager.split_symbol_name('*PACKAGE*')
        env = PackageManager.find(symbol_name, package_name, status_check)
        self.assertIsInstance(env, Environment)
        self.assertTrue('*PACKAGE*' in env)

        # Checks an environment using *PACKAGE* in another package.

        symbol_name, package_name, status_check = PackageManager.split_symbol_name('COMMON-LISP-USER::*PACKAGE*')
        env = PackageManager.find(symbol_name, package_name, status_check)
        self.assertIsInstance(env, Environment)
        self.assertTrue('*PACKAGE*' in env)

        symbol_name, package_name, status_check = PackageManager.split_symbol_name('COMMON-LISP-USER:*PACKAGE*')
        env = PackageManager.find(symbol_name, package_name, status_check)
        self.assertIsInstance(env, Environment)
        self.assertTrue('*PACKAGE*' in env)

    def testPackageManager_intern(self):
        # PackageManager.intern interns symbol_designator to Package.symbol_container.

        # The package_designator argument is omitted,
        # symbol_designator is interned to PackageManager.current_package.symbol_container.
        symbol, status = PackageManager.intern(symbol_designator=String('TEST-INTERN'))

        self.assertTrue(symbol is Symbol('TEST-INTERN'))
        self.assertTrue(status is Null())
        self.assertTrue('TEST-INTERN' in PackageManager.current_package.symbol_container.keys())
        self.assertEqual(PackageManager.current_package.symbol_container['TEST-INTERN'], [Symbol('TEST-INTERN'), Keyword(':INTERNAL'), None])

        # The package_designator argument is supplied to the specified package.
        symbol, status = PackageManager.intern(symbol_designator=String('INTERN-WITH-PACKAGE'), package_designator=String('COMMON-LISP-USER'))

        self.assertTrue(symbol is Symbol('INTERN-WITH-PACKAGE'))
        self.assertTrue(status is Null())
        self.assertTrue('INTERN-WITH-PACKAGE' in PackageManager.package_container['COMMON-LISP-USER'].symbol_container.keys())
        self.assertEqual(PackageManager.package_container['COMMON-LISP-USER'].symbol_container['INTERN-WITH-PACKAGE'], [Symbol('INTERN-WITH-PACKAGE'), Keyword(':INTERNAL'), None])

    def testPackageManager_find_symbol(self):
        # PackageManager.find_symbol returns symbol represented by symbol_designator and status.

        # The package_designator argument is omitted,
        # PackageManager.current_package.symbol_container is checked.
        # FIND-SYMBOL is interned to PackageManager.current_package in advance.
        PackageManager.intern(symbol_designator=String('FIND-SYMBOL'))
        symbol, status = PackageManager.find_symbol(symbol_designator=String('FIND-SYMBOL'))

        self.assertTrue(symbol is Symbol('FIND-SYMBOL'))
        self.assertTrue(status, Keyword(':INTERNAL'))

        # If symbol_designator dose not exist in PackageManager.current_package,
        # it must return Null object as symbol and status.
        symbol, status = PackageManager.find_symbol(symbol_designator=String('FIND-SYMBOL-NOT-EXIST'))

        self.assertTrue(symbol is Null())
        self.assertTrue(status is Null())

        # The package_designator argument is supplied to the specified package.
        # FIND-SYMBOL-WITH-PACKAGE is interned to COMMON-LISP-USER package in advance.
        PackageManager.intern(symbol_designator=String('FIND-SYMBOL-WITH-PACKAGE'), package_designator=String('COMMON-LISP-USER'))
        symbol, status = PackageManager.find_symbol(symbol_designator=String('FIND-SYMBOL-WITH-PACKAGE'), package_designator=String('COMMON-LISP-USER'))

        self.assertTrue(symbol is Symbol('FIND-SYMBOL-WITH-PACKAGE'))
        self.assertTrue(status is Keyword(':INTERNAL'))

        # If symbol_designator dose not exist in the specified package,
        # it must return Null object as symbol and status.
        symbol, status = PackageManager.find_symbol(symbol_designator=String('FIND-SYMBOL-WITH-PACKAGE-NOT-EXIST'), package_designator=String('COMMON-LISP-USER'))

        self.assertTrue(symbol is Null())
        self.assertTrue(status is Null())

    def testPackageManager_export(self):
        # PackageManager.export changes symbol status in symbol container to :EXTERNAL.

        # The package_designator argument is omitted,
        # status in PackageManager.current_package.symbol_container is changed.
        # EXPORT is interned to PackageManager.current_package in advance.
        PackageManager.intern(symbol_designator=String('EXPORT'))
        PackageManager.export(symbol_designator=Symbol('EXPORT'))
        symbol, status = PackageManager.find_symbol(symbol_designator=String('EXPORT'))

        self.assertTrue(symbol is Symbol('EXPORT'))
        self.assertTrue(status is Keyword(':EXTERNAL'))

        # The package_designator argument is supplied to the specified package.
        # EXPORT-WITH-PACKAGE is interned to COMMON-LISP-USER pacakge in advance.
        PackageManager.intern(symbol_designator=String('EXPORT-WITH-PACKAGE'), package_designator=String('COMMON-LISP-USER'))
        PackageManager.export(symbol_designator=Symbol('EXPORT-WITH-PACKAGE'), package_designator=Symbol('COMMON-LISP-USER'))
        symbol, status = PackageManager.find_symbol(symbol_designator=String('EXPORT-WITH-PACKAGE'), package_designator=String('COMMON-LISP-USER'))

        self.assertTrue(symbol is Symbol('EXPORT-WITH-PACKAGE'))
        self.assertTrue(status is Keyword(':EXTERNAL'))

    def testPackageManager_import(self):
        # PackageManager.import_ imports symbol to symbol container of the specific package.

        # The package_name argument is omitted,
        # the method imports symbol to PackageManager.current_package.symbol_container.

        # IMPORT is interned to PakageManager.package_container['COMMON-LISP-USER'].
        PackageManager.intern(symbol_designator=String('IMPORT'), package_designator=String('COMMON-LISP-USER'))
        # Add value to the variable space.
        PackageManager.package_container['COMMON-LISP-USER'].env['VARIABLE']['IMPORT'] = String('VALUE')

        PackageManager.import_(symbol_designator=Symbol('COMMON-LISP-USER::IMPORT'))
        symbol, status = PackageManager.find_symbol(symbol_designator=String('IMPORT'))

        self.assertTrue(symbol is Symbol('IMPORT'))
        self.assertTrue(status is Keyword(':INTERNAL'))
        self.assertTrue(PackageManager.current_package.env['VARIABLE']['IMPORT'] is String('VALUE'))

        # The package_designator argument is supplied to the specified package.

        # IMPORT-WITH-PACKAGE is interned to COMMON-LISP package.
        PackageManager.intern(symbol_designator=String('IMPORT-WITH-PACKAGE'), package_designator=String('COMMON-LISP'))
        # Add value to the variable space.
        PackageManager.package_container['COMMON-LISP'].env['VARIABLE']['IMPORT-WITH-PACKAGE'] = String('VALUE')

        PackageManager.import_(symbol_designator=Symbol('COMMON-LISP::IMPORT-WITH-PACKAGE'), package_designator=Symbol('COMMON-LISP-USER'))
        symbol, status = PackageManager.find_symbol(symbol_designator=String('IMPORT-WITH-PACKAGE'), package_designator=String('COMMON-LISP-USER'))

        self.assertTrue(symbol is Symbol('IMPORT-WITH-PACKAGE'))
        self.assertTrue(status is Keyword(':INTERNAL'))
        self.assertTrue(PackageManager.package_container['COMMON-LISP-USER'].env['VARIABLE']['IMPORT-WITH-PACKAGE'] is String('VALUE'))

    def testPackageManager_in_package(self):
        # PackageManager.in_package changes PackageManager.current_package to package_designator.

        # PackageManager.current_package is chenged to COMMON-LISP-USER.
        PackageManager.in_package(package_designator=Symbol('COMMON-LISP-USER'))

        # Check PackageManager.current_package and *PACKAGE* variable in COMMON-LISP package.
        self.assertTrue(PackageManager.current_package is PackageManager.package_container['COMMON-LISP-USER'])
        self.assertTrue(PackageManager.package_container['COMMON-LISP'].env['VARIABLE']['*PACKAGE*'] is PackageManager.current_package)

        # PackageManager.current_package is chenged to COMMON-LISP as default for other tests.
        PackageManager.in_package(package_designator=Symbol('COMMON-LISP'))

    def testPackageManager_use_package(self):
        # PackageManager.use_package inherited symbol in package_designator_to_use.

        # The package_designator argument is omitted,
        # symbol is inherited, and interned to PackageManager.current_package.symbol_container.
        # USE-PACKAGE-INTERNAL and USE-PACKAGE-EXTERNAL are interned to COMMON-LISP-USER.
        PackageManager.intern(symbol_designator=String('USE-PACKAGE-INTERNAL'), package_designator=String('COMMON-LISP-USER'))
        PackageManager.intern(symbol_designator=String('USE-PACKAGE-EXTERNAL'), package_designator=String('COMMON-LISP-USER'))

        # Only status of USE-PACKAGE-EXTERNAL is changed to :EXTERNAL.
        PackageManager.export(symbol_designator=Symbol('USE-PACKAGE-EXTERNAL'), package_designator=Symbol('COMMON-LISP-USER'))

        PackageManager.use_package(package_designator_to_use=Symbol('COMMON-LISP-USER'))

        # When status is :EXTERNAL, the symbol is inherited.
        symbol, status = PackageManager.find_symbol(symbol_designator=String('USE-PACKAGE-EXTERNAL'))
        self.assertTrue(symbol is Symbol('USE-PACKAGE-EXTERNAL'))
        self.assertTrue(status is Keyword(':INHERITED'))

        # When statis is not :EXTERNAL, the symbol is not inherited.
        symbol, status = PackageManager.find_symbol(symbol_designator=String('USE-PACKAGE-INTERNAL'))
        self.assertTrue(symbol is Null())
        self.assertTrue(status is Null())

        # The package_designator argument is supplied to the specified package.
        # USE-PACKAGE-INTERNAL-WITH-PACKAGE and USE-PACKAGE-EXTERNAL-WITH-PACKAGE are interned to COMMON-LISP.
        PackageManager.intern(String('USE-PACKAGE-INTERNAL-WITH-PACKAGE'))
        PackageManager.intern(String('USE-PACKAGE-EXTERNAL-WITH-PACKAGE'))

        # Only status of USE-PACKAGE-EXTERNAL-WITH-PACKAGE is changed to :EXTERNAL.
        PackageManager.export(Symbol('USE-PACKAGE-EXTERNAL-WITH-PACKAGE'))

        PackageManager.use_package(package_designator_to_use=Symbol('COMMON-LISP'), package_designator=Symbol('COMMON-LISP-USER'))

        # When status is :EXTERNAL, the symbol is inherited.
        symbol, status = PackageManager.find_symbol(symbol_designator=String('USE-PACKAGE-EXTERNAL-WITH-PACKAGE'), package_designator=String('COMMON-LISP-USER'))
        self.assertTrue(symbol is Symbol('USE-PACKAGE-EXTERNAL-WITH-PACKAGE'))
        self.assertTrue(status is Keyword(':INHERITED'))

        # When statis is not :EXTERNAL, the symbol is not inherited.
        symbol, status = PackageManager.find_symbol(symbol_designator=String('USE-PACKAGE-INTERNAL-WITH-PACKAGE'), package_designator=String('COMMON-LISP-USER'))
        self.assertTrue(symbol is Null())
        self.assertTrue(status is Null())
