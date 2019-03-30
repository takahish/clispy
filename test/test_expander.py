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
from clispy.expander import Expander
from clispy.package import PackageManager
from clispy.parser import Parser
from clispy.type import SingleFloat


class ExpanderUnitTestCase(unittest.TestCase):
    def testExpanderUnitTestCase_expand_atom(self):
        # Sets atom.
        forms = Parser.parse('3.14')

        # Expand forms.
        forms = Expander.expand(
            forms,
            PackageManager.current_package.env['VARIABLE'],
            PackageManager.current_package.env['FUNCTION'],
            PackageManager.current_package.env['MACRO']
        )

        # Checks forms.
        self.assertEqual(forms, SingleFloat(3.14))
