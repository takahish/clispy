from clispy.function import SystemFunction
from clispy.package import PackageManager, assign_helper
from clispy.type import Null, String, Symbol
from clispy.meta_db import MetaDB

ALLOWED_DOC_TYPES = {"FUNCTION", "VARIABLE", "TYPE", "STRUCTURE", "CLASS", "SETF"}

def _normalize_symbol(sym):
    name, package, _ = PackageManager.split_symbol_name(sym.value)
    if package is None:
        package = PackageManager.current_package.package_name
    return package, name

def _normalize_doc_type(sym):
    if not isinstance(sym, Symbol):
        return None
    doc_type = sym.value.upper()
    if doc_type not in ALLOWED_DOC_TYPES:
        return None
    return doc_type


class DocumentationSystemFunction(SystemFunction):
    def __new__(cls, *args, **kwargs):
        cls.__name__ = "DOCUMENTATION"
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        args = self.eval_forms(forms, var_env, func_env, macro_env)
        thing = args.car
        doc_type_sym = args.cdr.car
        if not isinstance(thing, Symbol):
            return Null()
        doc_type = _normalize_doc_type(doc_type_sym)
        if doc_type is None:
            return Null()
        package, name = _normalize_symbol(thing)
        db = MetaDB.get()
        content = db.get_doc("symbol", package, name, doc_type)
        if content is None:
            return Null()
        return String(content)


class SetfDocumentationSystemFunction(SystemFunction):
    """Update documentation strings in the metadata DB.

    TODO: Convert SELF-DOCUMENTATION to a standard generic function when
    CLOS becomes available.
    """

    def __new__(cls, *args, **kwargs):
        cls.__name__ = "SETF-DOCUMENTATION"
        return object.__new__(cls)

    def __call__(self, forms, var_env, func_env, macro_env):
        args = self.eval_forms(forms, var_env, func_env, macro_env)
        new_doc = args.car
        thing = args.cdr.car
        doc_type_sym = args.cdr.cdr.car
        if not isinstance(thing, Symbol):
            return new_doc
        doc_type = _normalize_doc_type(doc_type_sym)
        if doc_type is None:
            return new_doc
        package, name = _normalize_symbol(thing)
        db = MetaDB.get()
        if isinstance(new_doc, Null):
            db.delete_doc("symbol", package, name, doc_type)
            return Null()
        else:
            db.set_doc("symbol", package, name, doc_type, new_doc.value)
            return new_doc


assign_helper(
    symbol_name="DOCUMENTATION",
    value=DocumentationSystemFunction(),
    package_name="COMMON-LISP",
    env="FUNCTION",
    status=":EXTERNAL",
)
assign_helper(
    symbol_name="SETF-DOCUMENTATION",
    value=SetfDocumentationSystemFunction(),
    package_name="COMMON-LISP",
    env="FUNCTION",
    status=":EXTERNAL",
)
