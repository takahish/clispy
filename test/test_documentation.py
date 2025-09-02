import os
import tempfile
import unittest

# Use a temporary database for tests
os.environ['CLISPY_META_DB'] = os.path.join(tempfile.gettempdir(), 'clispy_test_meta.db')

from clispy.meta_db import MetaDB
from clispy.runtime.documentation import (
    DocumentationSystemFunction,
    SetfDocumentationSystemFunction,
)
from clispy.parser import Parser
from clispy.package import PackageManager
from clispy.type import Null, String
from clispy.expander import Expander
from clispy.evaluator import Evaluator


class DocumentationTestCase(unittest.TestCase):
    def setUp(self):
        # Clear database before each test
        db = MetaDB.get()
        with db.tx() as cx:
            cx.execute('DELETE FROM docs')
            cx.execute('DELETE FROM entities')
            cx.execute('DELETE FROM history')
        self.var_env = PackageManager.current_package.env['VARIABLE']
        self.func_env = PackageManager.current_package.env['FUNCTION']
        self.macro_env = PackageManager.current_package.env['MACRO']
        self.doc_fn = DocumentationSystemFunction()
        self.setf_fn = SetfDocumentationSystemFunction()

    def test_round_trip(self):
        forms = Parser.parse("('FOO 'FUNCTION)")
        self.assertEqual(self.doc_fn(forms, self.var_env, self.func_env, self.macro_env), Null())
        forms = Parser.parse('(SETF (DOCUMENTATION \'FOO \'FUNCTION) "doc")')
        forms = Expander.expand(forms, self.var_env, self.func_env, self.macro_env)
        Evaluator.eval(forms, self.var_env, self.func_env, self.macro_env)
        forms = Parser.parse("('FOO 'FUNCTION)")
        self.assertEqual(self.doc_fn(forms, self.var_env, self.func_env, self.macro_env), String('doc'))
        forms = Parser.parse("(NIL 'FOO 'FUNCTION)")
        self.setf_fn(forms, self.var_env, self.func_env, self.macro_env)
        forms = Parser.parse("('FOO 'FUNCTION)")
        self.assertEqual(self.doc_fn(forms, self.var_env, self.func_env, self.macro_env), Null())

    def test_doc_type_independent(self):
        forms = Parser.parse('("fdoc" \'BAR \'FUNCTION)')
        self.setf_fn(forms, self.var_env, self.func_env, self.macro_env)
        forms = Parser.parse('("vdoc" \'BAR \'VARIABLE)')
        self.setf_fn(forms, self.var_env, self.func_env, self.macro_env)
        forms = Parser.parse("('BAR 'FUNCTION)")
        self.assertEqual(self.doc_fn(forms, self.var_env, self.func_env, self.macro_env), String('fdoc'))
        forms = Parser.parse("('BAR 'VARIABLE)")
        self.assertEqual(self.doc_fn(forms, self.var_env, self.func_env, self.macro_env), String('vdoc'))

    def test_package_qualified(self):
        forms = Parser.parse('("pkg" \'CL-USER::BAZ \'FUNCTION)')
        self.setf_fn(forms, self.var_env, self.func_env, self.macro_env)
        forms = Parser.parse("('CL-USER::BAZ 'FUNCTION)")
        self.assertEqual(self.doc_fn(forms, self.var_env, self.func_env, self.macro_env), String('pkg'))

    def test_missing_returns_nil(self):
        forms = Parser.parse("('NO-SUCH 'FUNCTION)")
        self.assertEqual(self.doc_fn(forms, self.var_env, self.func_env, self.macro_env), Null())


if __name__ == '__main__':
    unittest.main()
