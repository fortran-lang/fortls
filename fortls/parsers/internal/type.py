from __future__ import annotations

import copy
from typing import TYPE_CHECKING

from fortls.constants import BLOCK_TYPE_ID, CLASS_TYPE_ID, KEYWORD_ID_DICT
from fortls.json_templates import range_json
from fortls.jsonrpc import path_to_uri

from .diagnostics import Diagnostic
from .scope import Scope
from .utilities import find_in_scope

if TYPE_CHECKING:
    from .ast import FortranAST


class Type(Scope):
    def __init__(
        self,
        file_ast: FortranAST,
        line_number: int,
        name: str,
        keywords: list,
    ):
        super().__init__(file_ast, line_number, name, keywords)
        self.in_children: list = []
        self.inherit = None
        self.inherit_var = None
        self.inherit_tmp = None
        self.inherit_version = -1
        self.abstract = self.keywords.count(KEYWORD_ID_DICT["abstract"]) > 0
        if self.keywords.count(KEYWORD_ID_DICT["public"]) > 0:
            self.vis = 1
        if self.keywords.count(KEYWORD_ID_DICT["private"]) > 0:
            self.vis = -1

    def get_type(self, no_link=False):
        return CLASS_TYPE_ID

    def get_desc(self):
        return "TYPE"

    def get_children(self, public_only=False):
        tmp_list = copy.copy(self.children)
        tmp_list.extend(self.in_children)
        return tmp_list

    def resolve_inherit(self, obj_tree, inherit_version):
        if (self.inherit is None) or (self.inherit_version == inherit_version):
            return
        self.inherit_version = inherit_version
        self.inherit_var = find_in_scope(self.parent, self.inherit, obj_tree)
        if self.inherit_var is not None:
            self._resolve_inherit_parent(obj_tree, inherit_version)

    def _resolve_inherit_parent(self, obj_tree, inherit_version):
        # Resolve parent inheritance while avoiding circular recursion
        self.inherit_tmp = self.inherit
        self.inherit = None
        self.inherit_var.resolve_inherit(obj_tree, inherit_version)
        self.inherit = self.inherit_tmp
        self.inherit_tmp = None
        # Get current fields
        child_names = [child.name.lower() for child in self.children]
        # Import for parent objects
        self.in_children = []
        for child in self.inherit_var.get_children():
            if child.name.lower() not in child_names:
                self.in_children.append(child)

    def require_inherit(self):
        return True

    def get_overridden(self, field_name):
        ret_list = []
        field_name = field_name.lower()
        for child in self.children:
            if field_name == child.name.lower():
                ret_list.append(child)
                break
        if self.inherit_var is not None:
            ret_list += self.inherit_var.get_overridden(field_name)
        return ret_list

    def check_valid_parent(self):
        if self.parent is None:
            return False
        parent_type = self.parent.get_type()
        return parent_type != CLASS_TYPE_ID and parent_type < BLOCK_TYPE_ID

    def get_diagnostics(self):
        errors = []
        for in_child in self.in_children:
            if (not self.abstract) and (
                in_child.keywords.count(KEYWORD_ID_DICT["deferred"]) > 0
            ):
                new_diag = Diagnostic(
                    self.eline - 1,
                    f'Deferred procedure "{in_child.name}" not implemented',
                    severity=1,
                )
                new_diag.add_related(
                    path=in_child.file_ast.path,
                    line=in_child.sline - 1,
                    message="Inherited procedure declaration",
                )
                errors.append(new_diag)
        return errors

    def get_actions(self, sline, eline):
        actions = []
        edits = []
        line_number = self.eline - 1
        if (line_number < sline) or (line_number > eline):
            return actions
        if self.contains_start is None:
            edits.append(
                {
                    **range_json(line_number, 0, line_number, 0),
                    "newText": "CONTAINS\n",
                }
            )
        #
        diagnostics = []
        has_edits = False
        file_uri = path_to_uri(self.file_ast.path)
        for in_child in self.in_children:
            if in_child.keywords.count(KEYWORD_ID_DICT["deferred"]) > 0:
                # Get interface
                interface_string = in_child.get_interface(
                    name_replace=in_child.name,
                    change_strings=(
                        f"class({in_child.parent.name})",
                        f"CLASS({self.name})",
                    ),
                )
                if interface_string is None:
                    continue
                interface_path, interface_edits = self.parent.add_subroutine(
                    interface_string, no_contains=has_edits
                )
                if interface_path != self.file_ast.path:
                    continue
                edits.append(
                    {
                        **range_json(line_number, 0, line_number, 0),
                        "newText": "  PROCEDURE :: {0} => {0}\n".format(in_child.name),
                    }
                )
                edits += interface_edits
                new_diag = Diagnostic(
                    line_number,
                    f'Deferred procedure "{in_child.name}" not implemented',
                    severity=1,
                )
                new_diag.add_related(
                    path=in_child.file_ast.path,
                    line=in_child.sline - 1,
                    message="Inherited procedure declaration",
                )
                diagnostics.append(new_diag)
                has_edits = True
        #
        if has_edits:
            actions = [
                {
                    "title": "Implement deferred procedures",
                    "kind": "quickfix",
                    "edit": {"changes": {file_uri: edits}},
                    "diagnostics": diagnostics,
                }
            ]
        return actions

    def get_hover(self, long=False, drop_arg=-1) -> tuple[str, str]:
        keywords = [self.get_desc()]
        if self.abstract:
            keywords.append("ABSTRACT")
        if self.inherit:
            keywords.append(f"EXTENDS({self.inherit})")
        decl = ", ".join(keywords)
        hover = f"{decl} :: {self.name}"
        doc_str = self.get_documentation()
        return hover, doc_str
