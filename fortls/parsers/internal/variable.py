from __future__ import annotations

from typing import TYPE_CHECKING

from fortls.constants import CLASS_TYPE_ID, KEYWORD_ID_DICT, VAR_TYPE_ID, FRegex
from fortls.helper_functions import fortran_md, get_keywords, get_paren_substring

from .base import FortranObj
from .diagnostics import Diagnostic
from .utilities import find_in_scope, find_in_workspace

if TYPE_CHECKING:
    from .ast import FortranAST
    from .imports import Import
    from .use import Use


class Variable(FortranObj):
    def __init__(
        self,
        file_ast: FortranAST,
        line_number: int,
        name: str,
        var_desc: str,
        keywords: list,
        keyword_info: dict = None,
        kind: str | None = None,
        link_obj=None,
    ):
        super().__init__()
        if keyword_info is None:
            keyword_info = {}
        self.file_ast: FortranAST = file_ast
        self.sline: int = line_number
        self.eline: int = line_number
        self.name: str = name
        self.desc: str = var_desc
        self.keywords: list = keywords
        self.keyword_info: dict = keyword_info
        self.kind: str | None = kind
        self.children: list = []
        self.use: list[Use | Import] = []
        self.link_obj = None
        self.type_obj = None
        self.is_const: bool = False
        self.is_external: bool = False
        self.param_val: str = None
        self.link_name: str = None
        self.callable: bool = FRegex.CLASS_VAR.match(self.get_desc(True)) is not None
        self.FQSN: str = self.name.lower()
        if link_obj is not None:
            self.link_name = link_obj.lower()
        if file_ast.enc_scope_name is not None:
            self.FQSN = f"{file_ast.enc_scope_name.lower()}::{self.name.lower()}"
        if self.keywords.count(KEYWORD_ID_DICT["public"]) > 0:
            self.vis = 1
        if self.keywords.count(KEYWORD_ID_DICT["private"]) > 0:
            self.vis = -1
        if self.keywords.count(KEYWORD_ID_DICT["parameter"]) > 0:
            self.is_const = True
        if (
            self.keywords.count(KEYWORD_ID_DICT["external"]) > 0
            or self.desc.lower() == "external"
        ):
            self.is_external = True

    def update_fqsn(self, enc_scope=None):
        if enc_scope is not None:
            self.FQSN = f"{enc_scope.lower()}::{self.name.lower()}"
        else:
            self.FQSN = self.name.lower()
        for child in self.children:
            child.update_fqsn(self.FQSN)

    def resolve_link(self, obj_tree):
        self.link_obj = None
        if self.link_name is None:
            return
        if self.parent is not None:
            link_obj = find_in_scope(self.parent, self.link_name, obj_tree)
            if link_obj is not None:
                self.link_obj = link_obj

    def require_link(self):
        return self.link_name is not None

    def get_type(self, no_link=False):
        if (not no_link) and (self.link_obj is not None):
            return self.link_obj.get_type()
        # Normal variable
        return VAR_TYPE_ID

    def get_desc(self, no_link=False):
        if not no_link and self.link_obj is not None:
            return self.link_obj.get_desc()
        # Normal variable
        return self.desc + self.kind if self.kind else self.desc

    def get_type_obj(self, obj_tree):
        if self.link_obj is not None:
            return self.link_obj.get_type_obj(obj_tree)
        if (self.type_obj is None) and (self.parent is not None):
            type_name = get_paren_substring(self.get_desc(no_link=True))
            if type_name is not None:
                search_scope = self.parent
                if search_scope.get_type() == CLASS_TYPE_ID:
                    search_scope = search_scope.parent
                if search_scope is not None:
                    type_name = type_name.strip().lower()
                    type_obj = find_in_scope(search_scope, type_name, obj_tree)
                    if type_obj is not None:
                        self.type_obj = type_obj
        return self.type_obj

    # XXX: unused delete or use for associate blocks
    def set_dim(self, dim_str):
        if KEYWORD_ID_DICT["dimension"] not in self.keywords:
            self.keywords.append(KEYWORD_ID_DICT["dimension"])
            self.keyword_info["dimension"] = dim_str
            self.keywords.sort()

    def get_snippet(self, name_replace=None, drop_arg=-1):
        name = name_replace if name_replace is not None else self.name
        if self.link_obj is not None:
            return self.link_obj.get_snippet(name, drop_arg)
        # Normal variable
        return None, None

    def get_hover(self, long=False, drop_arg=-1) -> tuple[str, str]:
        doc_str = self.get_documentation()
        # In associated blocks we need to fetch the desc and keywords of the
        # linked object
        hover_str = ", ".join([self.get_desc()] + self.get_keywords())
        # If this is not a preprocessor variable, we can append the variable name
        if not hover_str.startswith("#"):
            hover_str += f" :: {self.name}"
        if self.is_parameter() and self.param_val:
            hover_str += f" = {self.param_val}"
        return hover_str, doc_str

    def get_hover_md(self, long=False, drop_arg=-1):
        return fortran_md(*self.get_hover(long, drop_arg))

    def get_keywords(self):
        # TODO: if local keywords are set they should take precedence over link_obj
        # Alternatively, I could do a dictionary merge with local variables
        # having precedence by default and use a flag to override?
        if self.link_obj is not None:
            return get_keywords(self.link_obj.keywords, self.link_obj.keyword_info)
        return get_keywords(self.keywords, self.keyword_info)

    def is_optional(self):
        return self.keywords.count(KEYWORD_ID_DICT["optional"]) > 0

    def is_callable(self):
        return self.callable

    def is_parameter(self):
        return self.is_const

    def set_parameter_val(self, val: str):
        self.param_val = val

    def set_external_attr(self):
        self.keywords.append(KEYWORD_ID_DICT["external"])
        self.is_external = True

    def check_definition(self, obj_tree, known_types=None, interface=False):
        if known_types is None:
            known_types = {}
        # Check for type definition in scope
        type_match = FRegex.DEF_KIND.match(self.get_desc(no_link=True))
        if type_match is not None:
            var_type = type_match.group(1).strip().lower()
            if var_type == "procedure":
                return None, known_types
            desc_obj_name = type_match.group(2).strip().lower()
            if desc_obj_name not in known_types:
                type_def = find_in_scope(
                    self.parent,
                    desc_obj_name,
                    obj_tree,
                    interface=interface,
                )
                if type_def is None:
                    self._check_definition_type_def(
                        obj_tree, desc_obj_name, known_types, type_match
                    )
                else:
                    known_types[desc_obj_name] = (0, type_def)
            type_info = known_types[desc_obj_name]
            if type_info is not None and type_info[0] == 1:
                if interface:
                    out_diag = Diagnostic(
                        self.sline - 1,
                        message=f'Object "{desc_obj_name}" not imported in interface',
                        severity=1,
                        find_word=desc_obj_name,
                    )
                else:
                    out_diag = Diagnostic(
                        self.sline - 1,
                        message=f'Object "{desc_obj_name}" not found in scope',
                        severity=1,
                        find_word=desc_obj_name,
                    )
                    type_def = type_info[1]
                    out_diag.add_related(
                        path=type_def.file_ast.path,
                        line=type_def.sline - 1,
                        message="Possible object",
                    )
                return out_diag, known_types
        return None, known_types

    def _check_definition_type_def(
        self, obj_tree, desc_obj_name, known_types, type_match
    ):
        type_defs = find_in_workspace(
            obj_tree,
            desc_obj_name,
            filter_public=True,
            exact_match=True,
        )
        known_types[desc_obj_name] = None
        var_type = type_match.group(1).strip().lower()
        filter_id = VAR_TYPE_ID
        if var_type in ["class", "type"]:
            filter_id = CLASS_TYPE_ID
        for type_def in type_defs:
            if type_def.get_type() == filter_id:
                known_types[desc_obj_name] = (1, type_def)
                break
