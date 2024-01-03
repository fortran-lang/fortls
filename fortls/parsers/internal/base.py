from __future__ import annotations

from fortls.constants import BASE_TYPE_ID
from fortls.helper_functions import fortran_md


# Fortran object classes
class FortranObj:
    def __init__(self):
        self.vis: int = 0
        self.def_vis: int = 0
        self.doc_str: str = None
        self.parent = None
        self.eline: int = -1
        self.implicit_vars = None

    def set_default_vis(self, new_vis: int):
        self.def_vis = new_vis

    def set_visibility(self, new_vis: int):
        self.vis = new_vis

    def set_parent(self, parent_obj):
        self.parent = parent_obj

    def add_doc(self, doc_str: str):
        self.doc_str = doc_str

    def update_fqsn(self, enc_scope=None):
        return None

    def end(self, line_number: int):
        self.eline = line_number

    def resolve_inherit(self, obj_tree, inherit_version):
        return None

    def require_inherit(self):
        return False

    def resolve_link(self, obj_tree):
        return None

    def require_link(self):
        return False

    def get_type(self, no_link=False):
        return BASE_TYPE_ID

    def get_type_obj(self, obj_tree):
        return None

    def get_desc(self):
        return "unknown"

    def get_snippet(self, name_replace=None, drop_arg=-1):
        return None, None

    def get_documentation(self):
        return self.doc_str

    def get_hover(self, long=False, drop_arg=-1) -> tuple[str | None, str | None]:
        return None, None

    def get_hover_md(self, long=False, drop_arg=-1) -> str:
        msg, docs = self.get_hover(long, drop_arg)
        return fortran_md(msg, docs)

    def get_signature(self, drop_arg=-1):
        return None, None, None

    def get_interface(self, name_replace=None, drop_arg=-1, change_strings=None):
        return None

    def get_children(self, public_only=False):
        return []

    def get_ancestors(self):
        return []

    def get_diagnostics(self):
        return []

    def get_implicit(self):
        if self.parent is None:
            return self.implicit_vars
        parent_implicit = self.parent.get_implicit()
        if (self.implicit_vars is not None) or (parent_implicit is None):
            return self.implicit_vars
        return parent_implicit

    def get_actions(self, sline, eline):
        return None

    def is_optional(self):
        return False

    def is_mod_scope(self):
        return False

    def is_callable(self):
        return False

    def is_external_int(self):
        return False

    def is_abstract(self):
        return False

    def req_named_end(self):
        return False

    def check_valid_parent(self):
        return True

    def check_definition(self, obj_tree, known_types: dict = None, interface=False):
        if known_types is None:
            known_types = {}
        return None, known_types
