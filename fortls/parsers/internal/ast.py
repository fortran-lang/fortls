from __future__ import annotations

import os
import re
from re import Pattern

from fortls.ftypes import IncludeInfo
from fortls.json_templates import diagnostic_json

from .diagnostics import Diagnostic
from .imports import Import, ImportTypes
from .interface import Interface
from .program import Program
from .scope import Scope
from .use import Use
from .variable import Variable


class FortranAST:
    def __init__(self, file_obj=None):
        self.file = file_obj
        self.path: str | None = file_obj.path if file_obj is not None else None
        self.global_dict: dict = {}
        self.scope_list: list = []
        self.variable_list: list = []
        self.public_list: list = []
        self.private_list: list = []
        self.scope_stack: list = []
        self.end_stack: list = []
        self.pp_if: list = []
        self.include_statements: list = []
        self.end_errors: list = []
        self.parse_errors: list = []
        self.inherit_objs: list = []
        self.linkable_objs: list = []
        self.external_objs: list = []
        self.none_scope = None
        self.inc_scope = None
        self.current_scope = None
        self.end_scope_regex: Pattern | None = None
        self.enc_scope_name: str | None = None
        self.last_obj = None
        self.pending_doc: str | None = None

    def create_none_scope(self):
        """Create empty scope to hold non-module contained items"""
        if self.none_scope is not None:
            raise ValueError
        self.none_scope = Program(self, 1, "main")
        self.add_scope(
            self.none_scope, re.compile(r"[ ]*END[ ]*PROGRAM", re.I), exportable=False
        )

    def get_enc_scope_name(self):
        """Get current enclosing scope name"""
        return None if self.current_scope is None else self.current_scope.FQSN

    def add_scope(
        self,
        new_scope: Scope,
        end_scope_regex: Pattern[str],
        exportable: bool = True,
        req_container: bool = False,
    ):
        self.scope_list.append(new_scope)
        if new_scope.require_inherit():
            self.inherit_objs.append(new_scope)
        if new_scope.require_link():
            self.linkable_objs.append(new_scope)
        if self.current_scope is None:
            if req_container:
                self.create_none_scope()
                new_scope.FQSN = f"{self.none_scope.FQSN}::{new_scope.name.lower()}"
                self.current_scope.add_child(new_scope)
                self.scope_stack.append(self.current_scope)
            elif exportable:
                self.global_dict[new_scope.FQSN] = new_scope
        else:
            self.current_scope.add_child(new_scope)
            self.scope_stack.append(self.current_scope)
        if self.end_scope_regex is not None:
            self.end_stack.append(self.end_scope_regex)
        self.current_scope = new_scope
        self.end_scope_regex = end_scope_regex
        self.enc_scope_name = self.get_enc_scope_name()
        self.last_obj = new_scope
        if self.pending_doc is not None:
            self.last_obj.add_doc(self.pending_doc)
            self.pending_doc = None

    def end_scope(self, line_number: int, check: bool = True):
        if (
            (self.current_scope is None) or (self.current_scope is self.none_scope)
        ) and check:
            self.end_errors.append([-1, line_number])
            return
        self.current_scope.end(line_number)
        if len(self.scope_stack) > 0:
            self.current_scope = self.scope_stack.pop()
        else:
            self.current_scope = None
        if len(self.end_stack) > 0:
            self.end_scope_regex = self.end_stack.pop()
        else:
            self.end_scope_regex = None
        self.enc_scope_name = self.get_enc_scope_name()

    def add_variable(self, new_var: Variable):
        if self.current_scope is None:
            self.create_none_scope()
            new_var.FQSN = f"{self.none_scope.FQSN}::{new_var.name.lower()}"
        self.current_scope.add_child(new_var)
        self.variable_list.append(new_var)
        if new_var.is_external:
            self.external_objs.append(new_var)
        if new_var.require_link():
            self.linkable_objs.append(new_var)
        self.last_obj = new_var
        if self.pending_doc is not None:
            self.last_obj.add_doc(self.pending_doc)
            self.pending_doc = None

    def add_int_member(self, key):
        self.current_scope.add_member(key)

    def add_private(self, name: str):
        self.private_list.append(f"{self.enc_scope_name}::{name}")

    def add_public(self, name: str):
        self.public_list.append(f"{self.enc_scope_name}::{name}")

    def add_use(self, use_mod: Use | Import):
        if self.current_scope is None:
            self.create_none_scope()
        self.current_scope.add_use(use_mod)

    def add_include(self, path: str, line_number: int):
        self.include_statements.append(IncludeInfo(line_number, path, None, []))

    def add_doc(self, doc_string: str, forward: bool = False):
        if not doc_string:
            return
        if forward:
            self.pending_doc = doc_string
        elif self.last_obj is not None:
            self.last_obj.add_doc(doc_string)

    def add_error(self, msg: str, sev: int, ln: int, sch: int, ech: int = None):
        """Add a Diagnostic error, encountered during parsing, for a range
        in the document.

        Parameters
        ----------
        msg : str
            Error message
        sev : int
            Severity, Error, Warning, Notification
        ln : int
            Line number
        sch : int
            Start character
        ech : int
            End character
        """
        # Convert from Editor line numbers 1-base index to LSP index which is 0-based
        self.parse_errors.append(diagnostic_json(ln - 1, sch, ln - 1, ech, msg, sev))

    def start_ppif(self, line_number: int):
        self.pp_if.append([line_number - 1, -1])

    def end_ppif(self, line_number: int):
        if len(self.pp_if) > 0:
            self.pp_if[-1][1] = line_number - 1

    def get_scopes(self, line_number: int | None = None):
        """Get a list of all the scopes present in the line number provided.

        Parameters
        ----------
        line_number : int, optional
            Document line number, if None return all document scopes, by default None

        Returns
        -------
        Variable,Type,Function,Subroutine,Module,Program,Interface,BlockData
            A list of scopes
        """
        if line_number is None:
            return self.scope_list
        scope_list = []
        for scope in self.scope_list:
            if not scope.sline <= line_number <= scope.eline:
                continue
            if type(scope.parent) is Interface:
                for use_stmnt in scope.use:
                    if type(use_stmnt) is not Import:
                        continue
                    # Exclude the parent and all other scopes
                    if use_stmnt.import_type == ImportTypes.NONE:
                        return [scope]
            scope_list.append(scope)
            scope_list.extend(iter(scope.get_ancestors()))
        if scope_list or self.none_scope is None:
            return scope_list
        return [self.none_scope]

    def get_inner_scope(self, line_number: int):
        scope_sline = -1
        curr_scope = None
        for scope in self.scope_list:
            if scope.sline > scope_sline and scope.sline <= line_number <= scope.eline:
                curr_scope = scope
                scope_sline = scope.sline
        if (curr_scope is None) and (self.none_scope is not None):
            return self.none_scope
        return curr_scope

    def get_object(self, FQSN: str):
        def find_child_by_name(parent, name):
            for child in parent.children:
                if child.name == name:
                    return child
                if child.name.startswith("#GEN_INT"):
                    found = next(
                        (
                            int_child
                            for int_child in child.get_children()
                            if int_child.name == name
                        ),
                        None,
                    )
                    if found:
                        return found
            return None

        parts = FQSN.split("::")
        current = self.global_dict.get(parts[0])

        # Look for non-exportable scopes
        if current is None:
            current = next(
                (scope for scope in self.scope_list if scope.FQSN == parts[0]), None
            )
            if current is None:
                return None

        for part in parts[1:]:
            current = find_child_by_name(current, part)
            if current is None:
                return None

        return current

    def resolve_includes(self, workspace, path: str | None = None):
        file_dir = os.path.dirname(self.path)
        for inc in self.include_statements:
            file_path = os.path.normpath(os.path.join(file_dir, inc.path))
            if path and path != file_path:
                continue
            parent_scope = self.get_inner_scope(inc.line_number)
            added_entities = inc.scope_objs
            if file_path in workspace:
                include_file = workspace[file_path]
                include_ast = include_file.ast
                inc.file = include_file
                if include_ast.none_scope:
                    if include_ast.inc_scope is None:
                        include_ast.inc_scope = include_ast.none_scope
                    # Remove old objects
                    for obj in added_entities:
                        parent_scope.children.remove(obj)
                    added_entities = []
                    for child in include_ast.inc_scope.children:
                        added_entities.append(child)
                        if parent_scope is not None:
                            parent_scope.add_child(child)
                            child.update_fqsn(parent_scope.FQSN)
                    include_ast.none_scope = parent_scope
                    inc.scope_objs = added_entities

    def resolve_links(self, obj_tree, link_version):
        for inherit_obj in self.inherit_objs:
            inherit_obj.resolve_inherit(obj_tree, inherit_version=link_version)
        for linkable_obj in self.linkable_objs:
            linkable_obj.resolve_link(obj_tree)

    def close_file(self, line_number: int):
        # Close open scopes
        while self.current_scope is not None:
            self.end_scope(line_number, check=False)
        # Close and delist none_scope
        if self.none_scope is not None:
            self.none_scope.end(line_number)
            self.scope_list.remove(self.none_scope)
        # Tasks to be done when file parsing is finished
        for private_name in self.private_list:
            obj = self.get_object(private_name)
            if obj is not None:
                obj.set_visibility(-1)
        for public_name in self.public_list:
            obj = self.get_object(public_name)
            if obj is not None:
                obj.set_visibility(1)

    def check_file(self, obj_tree):
        errors = []
        tmp_list = self.scope_list[:]  # shallow copy
        if self.none_scope is not None:
            tmp_list += [self.none_scope]
        for error in self.end_errors:
            if error[0] >= 0:
                message = f"Unexpected end of scope at line {error[0]}"
            else:
                message = "Unexpected end statement: No open scopes"
            errors.append(Diagnostic(error[1] - 1, message=message, severity=1))
        for scope in tmp_list:
            if not scope.check_valid_parent():
                errors.append(
                    Diagnostic(
                        scope.sline - 1,
                        message=f'Invalid parent for "{scope.get_desc()}" declaration',
                        severity=1,
                    )
                )
            errors += scope.check_use(obj_tree)
            errors += scope.check_definitions(obj_tree)
            errors += scope.get_diagnostics()
        return errors, self.parse_errors
