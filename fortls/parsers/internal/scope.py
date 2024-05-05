from __future__ import annotations

import copy
from typing import TYPE_CHECKING
from typing import Type as T

from fortls.constants import (
    BLOCK_TYPE_ID,
    FUNCTION_TYPE_ID,
    INTERFACE_TYPE_ID,
    MODULE_TYPE_ID,
    SUBMODULE_TYPE_ID,
    SUBROUTINE_TYPE_ID,
)
from fortls.json_templates import range_json

from .base import FortranObj
from .diagnostics import Diagnostic
from .imports import Import
from .utilities import find_in_scope

if TYPE_CHECKING:
    from .ast import FortranAST
    from .use import Use


class Scope(FortranObj):
    def __init__(
        self,
        file_ast: FortranAST,
        line_number: int,
        name: str,
        keywords: list = None,
    ):
        super().__init__()
        if keywords is None:
            keywords = []
        self.file_ast: FortranAST = file_ast
        self.sline: int = line_number
        self.eline: int = line_number
        self.name: str = name
        self.children: list[T[Scope]] = []
        self.members: list = []
        self.use: list[Use | Import] = []
        self.keywords: list = keywords
        self.inherit = None
        self.parent = None
        self.contains_start = None
        self.implicit_line = None
        self.FQSN: str = self.name.lower()
        if file_ast.enc_scope_name is not None:
            self.FQSN = f"{file_ast.enc_scope_name.lower()}::{self.name.lower()}"

    def copy_from(self, copy_source: Scope):
        # Pass the reference, we don't want shallow copy since that would still
        # result into 2 versions of attributes between copy_source and self
        for k, v in copy_source.__dict__.items():
            setattr(self, k, v)

    def add_use(self, use_mod: Use | Import):
        self.use.append(use_mod)

    def set_inherit(self, inherit_type):
        self.inherit = inherit_type

    def set_parent(self, parent_obj):
        self.parent = parent_obj

    def set_implicit(self, implicit_flag, line_number):
        self.implicit_vars = implicit_flag
        self.implicit_line = line_number

    def mark_contains(self, line_number):
        if self.contains_start is not None:
            raise ValueError
        self.contains_start = line_number

    def add_child(self, child):
        self.children.append(child)
        child.set_parent(self)

    def update_fqsn(self, enc_scope=None):
        if enc_scope is not None:
            self.FQSN = f"{enc_scope.lower()}::{self.name.lower()}"
        else:
            self.FQSN = self.name.lower()
        for child in self.children:
            child.update_fqsn(self.FQSN)

    def add_member(self, member):
        self.members.append(member)

    def get_children(self, public_only=False) -> list[T[FortranObj]]:
        if not public_only:
            return copy.copy(self.children)
        pub_children = []
        for child in self.children:
            if (child.vis < 0) or ((self.def_vis < 0) and (child.vis <= 0)):
                continue
            if child.name.startswith("#GEN_INT"):
                pub_children.append(child)
                continue
            pub_children.append(child)
        return pub_children

    def check_definitions(self, obj_tree) -> list[Diagnostic]:
        """Check for definition errors in scope"""
        fqsn_dict: dict[str, int] = {}
        errors: list[Diagnostic] = []
        known_types: dict[str, FortranObj] = {}

        for child in self.children:
            # Skip masking/double checks for interfaces
            if child.get_type() == INTERFACE_TYPE_ID:
                continue
            # Check other variables in current scope
            if child.FQSN in fqsn_dict:
                if child.sline < fqsn_dict[child.FQSN]:
                    fqsn_dict[child.FQSN] = child.sline - 1
            else:
                fqsn_dict[child.FQSN] = child.sline - 1

        contains_line = -1
        if self.get_type() in (
            MODULE_TYPE_ID,
            SUBMODULE_TYPE_ID,
            SUBROUTINE_TYPE_ID,
            FUNCTION_TYPE_ID,
        ):
            contains_line = (
                self.contains_start if self.contains_start is not None else self.eline
            )
        # Detect interface definitions
        is_interface = (
            self.parent is not None
            and self.parent.get_type() == INTERFACE_TYPE_ID
            and not self.is_mod_scope()
        )

        for child in self.children:
            if child.name.startswith("#"):
                continue
            line_number = child.sline - 1
            # Check for type definition in scope
            def_error, known_types = child.check_definition(
                obj_tree, known_types=known_types, interface=is_interface
            )
            if def_error is not None:
                errors.append(def_error)
            # Detect contains errors
            if contains_line >= child.sline and child.get_type(no_link=True) in (
                SUBROUTINE_TYPE_ID,
                FUNCTION_TYPE_ID,
            ):
                new_diag = Diagnostic(
                    line_number,
                    message="Subroutine/Function definition before CONTAINS statement",
                    severity=1,
                )
                errors.append(new_diag)
            # Skip masking/double checks for interfaces and members
            if (
                self.get_type() == INTERFACE_TYPE_ID
                or child.get_type() == INTERFACE_TYPE_ID
            ):
                continue
            # Check other variables in current scope
            if child.FQSN in fqsn_dict and line_number > fqsn_dict[child.FQSN]:
                new_diag = Diagnostic(
                    line_number,
                    message=f'Variable "{child.name}" declared twice in scope',
                    severity=1,
                    find_word=child.name,
                )
                new_diag.add_related(
                    path=self.file_ast.path,
                    line=fqsn_dict[child.FQSN],
                    message="First declaration",
                )
                errors.append(new_diag)
                continue
            # Check for masking from parent scope in subroutines, functions, and blocks
            if self.parent is not None and self.get_type() in (
                SUBROUTINE_TYPE_ID,
                FUNCTION_TYPE_ID,
                BLOCK_TYPE_ID,
            ):
                parent_var = find_in_scope(self.parent, child.name, obj_tree)
                if parent_var is not None:
                    # Ignore if function return variable
                    if (
                        self.get_type() == FUNCTION_TYPE_ID
                        and parent_var.FQSN == self.FQSN
                    ):
                        continue

                    new_diag = Diagnostic(
                        line_number,
                        message=(
                            f'Variable "{child.name}" masks variable in parent scope'
                        ),
                        severity=2,
                        find_word=child.name,
                    )
                    new_diag.add_related(
                        path=parent_var.file_ast.path,
                        line=parent_var.sline - 1,
                        message="First declaration",
                    )
                    errors.append(new_diag)

        return errors

    def check_use(self, obj_tree):
        errors = []
        last_use_line = -1
        for use_stmnt in self.use:
            last_use_line = max(last_use_line, use_stmnt.line_number)
            if type(use_stmnt) is Import:
                if (self.parent is None) or (
                    self.parent.get_type() != INTERFACE_TYPE_ID
                ):
                    new_diag = Diagnostic(
                        use_stmnt.line_number - 1,
                        message="IMPORT statement outside of interface",
                        severity=1,
                    )
                    errors.append(new_diag)
                continue
            if use_stmnt.mod_name not in obj_tree:
                new_diag = Diagnostic(
                    use_stmnt.line_number - 1,
                    message=f'Module "{use_stmnt.mod_name}" not found in project',
                    severity=3,
                    find_word=use_stmnt.mod_name,
                )
                errors.append(new_diag)
        if (self.implicit_line is not None) and (last_use_line >= self.implicit_line):
            new_diag = Diagnostic(
                self.implicit_line - 1,
                message="USE statements after IMPLICIT statement",
                severity=1,
                find_word="IMPLICIT",
            )
            errors.append(new_diag)
        return errors

    def add_subroutine(self, interface_string, no_contains=False):
        edits = []
        line_number = self.eline - 1
        if (self.contains_start is None) and (not no_contains):
            first_sub_line = line_number
            for child in self.children:
                if child.get_type() in (SUBROUTINE_TYPE_ID, FUNCTION_TYPE_ID):
                    first_sub_line = min(first_sub_line, child.sline - 1)
            edits.append(
                {
                    **range_json(first_sub_line, 0, first_sub_line, 0),
                    "newText": "CONTAINS\n",
                }
            )
        edits.append(
            {
                **range_json(line_number, 0, line_number, 0),
                "newText": interface_string + "\n",
            }
        )
        return self.file_ast.path, edits
