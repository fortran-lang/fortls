from __future__ import annotations

import copy
import os
import re
from dataclasses import replace
from typing import Pattern

from fortls.constants import (
    ASSOC_TYPE_ID,
    BASE_TYPE_ID,
    BLOCK_TYPE_ID,
    CLASS_TYPE_ID,
    DO_TYPE_ID,
    ENUM_TYPE_ID,
    FUNCTION_TYPE_ID,
    IF_TYPE_ID,
    INTERFACE_TYPE_ID,
    KEYWORD_ID_DICT,
    METH_TYPE_ID,
    MODULE_TYPE_ID,
    SELECT_TYPE_ID,
    SUBMODULE_TYPE_ID,
    SUBROUTINE_TYPE_ID,
    VAR_TYPE_ID,
    WHERE_TYPE_ID,
    FRegex,
)
from fortls.ftypes import INCLUDE_info, USE_info
from fortls.helper_functions import get_keywords, get_paren_substring, get_var_stack
from fortls.json_templates import diagnostic_json, location_json, range_json
from fortls.jsonrpc import path_to_uri


def get_use_tree(
    scope: fortran_scope,
    use_dict: dict[str, USE_info],
    obj_tree: dict,
    only_list: list[str] = None,
    rename_map: dict[str, str] = None,
    curr_path: list[str] = None,
):
    def intersect_only(use_stmnt):
        tmp_list = []
        tmp_map = rename_map.copy()
        for val1 in only_list:
            mapped1 = tmp_map.get(val1, val1)
            if mapped1 in use_stmnt.only_list:
                tmp_list.append(val1)
                new_rename = use_stmnt.rename_map.get(mapped1, None)
                if new_rename is not None:
                    tmp_map[val1] = new_rename
            else:
                tmp_map.pop(val1, None)
        return tmp_list, tmp_map

    if only_list is None:
        only_list = []
    if rename_map is None:
        rename_map = {}
    if curr_path is None:
        curr_path = []

    # Detect and break circular references
    if scope.FQSN in curr_path:
        return use_dict
    new_path = curr_path + [scope.FQSN]
    # Add recursively
    for use_stmnt in scope.use:
        if use_stmnt.mod_name not in obj_tree:
            continue
        # Intersect parent and current ONLY list and renaming
        if len(only_list) == 0:
            merged_use_list = use_stmnt.only_list[:]
            merged_rename = use_stmnt.rename_map.copy()
        elif len(use_stmnt.only_list) == 0:
            merged_use_list = only_list[:]
            merged_rename = rename_map.copy()
        else:
            merged_use_list, merged_rename = intersect_only(use_stmnt)
            if len(merged_use_list) == 0:
                continue
        # Update ONLY list and renaming for current module
        use_dict_mod = use_dict.get(use_stmnt.mod_name)
        if use_dict_mod is not None:
            old_len = len(use_dict_mod.only_list)
            if (old_len > 0) and (len(merged_use_list) > 0):
                only_len = old_len
                for only_name in merged_use_list:
                    use_dict_mod.only_list.add(only_name)
                    if len(use_dict_mod.only_list) != only_len:
                        only_len = len(use_dict_mod.only_list)
                        new_rename = merged_rename.get(only_name, None)
                        if new_rename is not None:
                            use_dict[use_stmnt.mod_name] = replace(
                                use_dict_mod, rename_map=merged_rename
                            )
            else:
                use_dict[use_stmnt.mod_name] = USE_info(use_stmnt.mod_name, set(), {})
            # Skip if we have already visited module with the same only list
            if old_len == len(use_dict_mod.only_list):
                continue
        else:
            use_dict[use_stmnt.mod_name] = USE_info(
                use_stmnt.mod_name, set(merged_use_list), merged_rename
            )
        # Descend USE tree
        use_dict = get_use_tree(
            obj_tree[use_stmnt.mod_name][0],
            use_dict,
            obj_tree,
            merged_use_list,
            merged_rename,
            new_path,
        )
    return use_dict


def find_in_scope(
    scope: fortran_scope,
    var_name: str,
    obj_tree: dict,
    interface: bool = False,
    local_only: bool = False,
    var_line_number: int = None,
):
    def check_scope(
        local_scope: fortran_scope,
        var_name_lower: str,
        filter_public: bool = False,
        var_line_number: int = None,
    ):
        for child in local_scope.get_children():
            if child.name.startswith("#GEN_INT"):
                tmp_var = check_scope(child, var_name_lower, filter_public)
                if tmp_var is not None:
                    return tmp_var
            if filter_public:
                if (child.vis < 0) or ((local_scope.def_vis < 0) and (child.vis <= 0)):
                    continue
            if child.name.lower() == var_name_lower:
                # For functions with an implicit result() variable the name
                # of the function is used. If we are hovering over the function
                # definition, we do not want the implicit result() to be returned.
                # If scope is from a function and child's name is same as functions name
                # and start of scope i.e. function definition is equal to the request ln
                # then we are need to skip this child
                if (
                    isinstance(local_scope, fortran_function)
                    and local_scope.name.lower() == child.name.lower()
                    and var_line_number in (local_scope.sline, local_scope.eline)
                ):
                    return None

                return child
        return None

    #
    var_name_lower = var_name.lower()
    # Check local scope
    if scope is None:
        return None
    tmp_var = check_scope(scope, var_name_lower, var_line_number=var_line_number)
    if local_only or (tmp_var is not None):
        return tmp_var
    # Check INCLUDE statements
    if scope.file_ast.include_statements:
        strip_str = var_name.replace('"', "")
        strip_str = strip_str.replace("'", "")
        for inc in scope.file_ast.include_statements:
            if strip_str == inc.path:
                if inc.file is None:
                    return None
                return fortran_include(inc.file.ast, inc.line_number, inc.path)

    # Setup USE search
    use_dict = get_use_tree(scope, {}, obj_tree)
    # Look in found use modules
    for use_mod, use_info in use_dict.items():
        use_scope = obj_tree[use_mod][0]
        # Module name is request
        if use_mod.lower() == var_name_lower:
            return use_scope
        # Filter children by only_list
        if len(use_info.only_list) > 0:
            if var_name_lower not in use_info.only_list:
                continue
        mod_name = use_info.rename_map.get(var_name_lower, var_name_lower)
        tmp_var = check_scope(use_scope, mod_name, filter_public=True)
        if tmp_var is not None:
            return tmp_var
    # Only search local and imported names for interfaces
    if interface:
        in_import = False
        for use_stmnt in scope.use:
            if use_stmnt.mod_name.startswith("#import"):
                if var_name_lower in use_stmnt.only_list:
                    in_import = True
                    break
        if not in_import:
            return None
    # Check parent scopes
    if scope.parent is not None:
        tmp_var = find_in_scope(scope.parent, var_name, obj_tree)
        if tmp_var is not None:
            return tmp_var
    # Check ancestor scopes
    for ancestor in scope.get_ancestors():
        tmp_var = find_in_scope(ancestor, var_name, obj_tree)
        if tmp_var is not None:
            return tmp_var
    return None


def find_in_workspace(
    obj_tree: dict, query: str, filter_public: bool = False, exact_match: bool = False
):
    def add_children(mod_obj, query: str):
        tmp_list = []
        for child_obj in mod_obj.get_children(filter_public):
            if child_obj.name.lower().find(query) >= 0:
                tmp_list.append(child_obj)
        return tmp_list

    matching_symbols = []
    query = query.lower()
    for (_, obj_packed) in obj_tree.items():
        top_obj = obj_packed[0]
        top_uri = obj_packed[1]
        if top_uri is not None:
            if top_obj.name.lower().find(query) > -1:
                matching_symbols.append(top_obj)
            if top_obj.get_type() == MODULE_TYPE_ID:
                matching_symbols += add_children(top_obj, query)
    if exact_match:
        filtered_symbols = []
        n = len(query)
        for symbol in matching_symbols:
            if len(symbol.name) == n:
                filtered_symbols.append(symbol)
        matching_symbols = filtered_symbols
    return matching_symbols


def climb_type_tree(var_stack, curr_scope: fortran_scope, obj_tree: dict):
    """Walk up user-defined type sequence to determine final field type"""
    # Find base variable in current scope
    iVar = 0
    var_name = var_stack[iVar].strip().lower()
    var_obj = find_in_scope(curr_scope, var_name, obj_tree)
    if var_obj is None:
        return None
    # Search for type, then next variable in stack and so on
    for _ in range(30):
        # Find variable type object
        type_obj = var_obj.get_type_obj(obj_tree)
        # Return if not found
        if type_obj is None:
            return None
        # Go to next variable in stack and exit if done
        iVar += 1
        if iVar == len(var_stack) - 1:
            break
        # Find next variable by name in type
        var_name = var_stack[iVar].strip().lower()
        var_obj = find_in_scope(type_obj, var_name, obj_tree, local_only=True)
        # Return if not found
        if var_obj is None:
            return None
    else:
        raise KeyError
    return type_obj


# Helper classes
class USE_line:
    def __init__(
        self,
        mod_name: str,
        line_number: int,
        only_list: list = None,
        rename_map: dict = None,
    ):
        self.mod_name: str = mod_name.lower()
        self.line_number: int = line_number
        if only_list is not None:
            self.only_list: list = [only.lower() for only in only_list]
        if rename_map is not None:
            self.rename_map: dict = {
                key.lower(): value.lower() for key, value in rename_map.items()
            }


class fortran_diagnostic:
    def __init__(
        self, sline: int, message: str, severity: int = 1, find_word: str = None
    ):
        self.sline: int = sline
        self.message: str = message
        self.severity: int = severity
        self.find_word: str = find_word
        self.has_related: bool = False
        self.related_path = None
        self.related_line = None
        self.related_message = None

    def add_related(self, path: str, line: int, message: str):
        self.has_related = True
        self.related_path = path
        self.related_line = line
        self.related_message = message

    def build(self, file_obj):
        schar = echar = 0
        if self.find_word is not None:
            self.sline, obj_range = file_obj.find_word_in_code_line(
                self.sline, self.find_word
            )
            if obj_range.start >= 0:
                schar = obj_range.start
                echar = obj_range.end
        diag = diagnostic_json(
            self.sline, schar, self.sline, echar, self.message, self.severity
        )
        if self.has_related:
            diag["relatedInformation"] = [
                {
                    **location_json(
                        path_to_uri(self.related_path), self.related_line, 0
                    ),
                    "message": self.related_message,
                }
            ]
        return diag


# Fortran object classes
class fortran_obj:
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

    @staticmethod
    def get_placeholders(arg_list: list[str]):
        place_holders = []
        for i, arg in enumerate(arg_list):
            opt_split = arg.split("=")
            if len(opt_split) > 1:
                place_holders.append(f"{opt_split[0]}=${{{i+1}:{opt_split[1]}}}")
            else:
                place_holders.append(f"${{{i+1}:{arg}}}")
        arg_str = f"({', '.join(arg_list)})"
        arg_snip = f"({', '.join(place_holders)})"
        return arg_str, arg_snip

    def get_documentation(self):
        return self.doc_str

    def get_hover(self, long=False, include_doc=True, drop_arg=-1):
        return None, False

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
        else:
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


class fortran_scope(fortran_obj):
    def __init__(self, file_ast, line_number: int, name: str, keywords: list = None):
        super().__init__()
        if keywords is None:
            keywords = []
        self.file_ast: fortran_ast = file_ast
        self.sline: int = line_number
        self.eline: int = line_number
        self.name: str = name
        self.children: list = []
        self.members: list = []
        self.use: list[USE_line] = []
        self.keywords: list = keywords
        self.inherit = None
        self.parent = None
        self.contains_start = None
        self.implicit_line = None
        self.FQSN: str = self.name.lower()
        if file_ast.enc_scope_name is not None:
            self.FQSN = file_ast.enc_scope_name.lower() + "::" + self.name.lower()

    def copy_from(self, copy_source: fortran_scope):
        # Pass the reference, we don't want shallow copy since that would still
        # result into 2 versions of attributes between copy_source and self
        for k, v in copy_source.__dict__.items():
            setattr(self, k, v)

    def add_use(
        self, use_mod, line_number, only_list: list = None, rename_map: dict = None
    ):
        if only_list is None:
            only_list = []
        if rename_map is None:
            rename_map = {}
        self.use.append(USE_line(use_mod, line_number, only_list, rename_map))

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
            self.FQSN = enc_scope.lower() + "::" + self.name.lower()
        else:
            self.FQSN = self.name.lower()
        for child in self.children:
            child.update_fqsn(self.FQSN)

    def add_member(self, member):
        self.members.append(member)

    def get_children(self, public_only=False):
        if public_only:
            pub_children = []
            for child in self.children:
                if (child.vis < 0) or ((self.def_vis < 0) and (child.vis <= 0)):
                    continue
                if child.name.startswith("#GEN_INT"):
                    pub_children.append(child)
                    continue
                pub_children.append(child)
            return pub_children
        else:
            return copy.copy(self.children)

    def check_definitions(self, obj_tree):
        """Check for definition errors in scope"""
        FQSN_dict = {}
        for child in self.children:
            # Skip masking/double checks for interfaces
            if child.get_type() == INTERFACE_TYPE_ID:
                continue
            # Check other variables in current scope
            if child.FQSN in FQSN_dict:
                if child.sline < FQSN_dict[child.FQSN]:
                    FQSN_dict[child.FQSN] = child.sline - 1
            else:
                FQSN_dict[child.FQSN] = child.sline - 1
        #
        contains_line = -1
        after_contains_list = (SUBROUTINE_TYPE_ID, FUNCTION_TYPE_ID)
        if self.get_type() in (
            MODULE_TYPE_ID,
            SUBMODULE_TYPE_ID,
            SUBROUTINE_TYPE_ID,
            FUNCTION_TYPE_ID,
        ):
            if self.contains_start is None:
                contains_line = self.eline
            else:
                contains_line = self.contains_start
        # Detect interface definitions
        is_interface = False
        if (
            (self.parent is not None)
            and (self.parent.get_type() == INTERFACE_TYPE_ID)
            and (not self.is_mod_scope())
        ):
            is_interface = True
        errors = []
        known_types = {}
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
            if (contains_line >= child.sline) and (
                child.get_type(no_link=True) in after_contains_list
            ):
                new_diag = fortran_diagnostic(
                    line_number,
                    message="Subroutine/Function definition before CONTAINS statement",
                    severity=1,
                )
                errors.append(new_diag)
            # Skip masking/double checks for interfaces and members
            if (self.get_type() == INTERFACE_TYPE_ID) or (
                child.get_type() == INTERFACE_TYPE_ID
            ):
                continue
            # Check other variables in current scope
            if child.FQSN in FQSN_dict:
                if line_number > FQSN_dict[child.FQSN]:
                    new_diag = fortran_diagnostic(
                        line_number,
                        message=f'Variable "{child.name}" declared twice in scope',
                        severity=1,
                        find_word=child.name,
                    )
                    new_diag.add_related(
                        path=self.file_ast.path,
                        line=FQSN_dict[child.FQSN],
                        message="First declaration",
                    )
                    errors.append(new_diag)
                    continue
            # Check for masking from parent scope in subroutines, functions, and blocks
            if (self.parent is not None) and (
                self.get_type() in (SUBROUTINE_TYPE_ID, FUNCTION_TYPE_ID, BLOCK_TYPE_ID)
            ):
                parent_var = find_in_scope(self.parent, child.name, obj_tree)
                if parent_var is not None:
                    # Ignore if function return variable
                    if (self.get_type() == FUNCTION_TYPE_ID) and (
                        parent_var.FQSN == self.FQSN
                    ):
                        continue
                    new_diag = fortran_diagnostic(
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
            if use_stmnt.mod_name.startswith("#import"):
                if (self.parent is None) or (
                    self.parent.get_type() != INTERFACE_TYPE_ID
                ):
                    new_diag = fortran_diagnostic(
                        use_stmnt.line_number - 1,
                        message="IMPORT statement outside of interface",
                        severity=1,
                    )
                    errors.append(new_diag)
                continue
            if use_stmnt.mod_name not in obj_tree:
                new_diag = fortran_diagnostic(
                    use_stmnt.line_number - 1,
                    message=f'Module "{use_stmnt.mod_name}" not found in project',
                    severity=3,
                    find_word=use_stmnt.mod_name,
                )
                errors.append(new_diag)
        if (self.implicit_line is not None) and (last_use_line >= self.implicit_line):
            new_diag = fortran_diagnostic(
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


class fortran_module(fortran_scope):
    def get_type(self, no_link=False):
        return MODULE_TYPE_ID

    def get_desc(self):
        return "MODULE"

    def check_valid_parent(self):
        if self.parent is not None:
            return False
        return True


class fortran_include(fortran_scope):
    def get_desc(self):
        return "INCLUDE"


class fortran_program(fortran_module):
    def get_desc(self):
        return "PROGRAM"


class fortran_submodule(fortran_module):
    def __init__(
        self,
        file_ast: fortran_ast,
        line_number: int,
        name: str,
        ancestor_name: str = None,
    ):
        super().__init__(file_ast, line_number, name)
        self.ancestor_name = ancestor_name
        self.ancestor_obj = None

    def get_type(self, no_link=False):
        return SUBMODULE_TYPE_ID

    def get_desc(self):
        return "SUBMODULE"

    def get_ancestors(self):
        if self.ancestor_obj is not None:
            great_ancestors = self.ancestor_obj.get_ancestors()
            if great_ancestors is not None:
                return [self.ancestor_obj] + great_ancestors
            return [self.ancestor_obj]
        return []

    def resolve_inherit(self, obj_tree, inherit_version):
        if self.ancestor_name is None:
            return
        if self.ancestor_name in obj_tree:
            self.ancestor_obj = obj_tree[self.ancestor_name][0]

    def require_inherit(self):
        return True

    def resolve_link(self, obj_tree):
        # Link subroutine/function implementations to prototypes
        if self.ancestor_obj is None:
            return
        # Grab ancestor interface definitions (function/subroutine only)
        ancestor_interfaces = []
        for child in self.ancestor_obj.children:
            if child.get_type() == INTERFACE_TYPE_ID:
                for prototype in child.children:
                    prototype_type = prototype.get_type()
                    if (
                        prototype_type
                        in (SUBROUTINE_TYPE_ID, FUNCTION_TYPE_ID, BASE_TYPE_ID)
                    ) and prototype.is_mod_scope():
                        ancestor_interfaces.append(prototype)
        # Match interface definitions to implementations
        for prototype in ancestor_interfaces:
            for (i, child) in enumerate(self.children):
                if child.name.lower() == prototype.name.lower():
                    # Create correct object for interface
                    if child.get_type() == BASE_TYPE_ID:
                        child_old = child
                        if prototype.get_type() == SUBROUTINE_TYPE_ID:
                            child = fortran_subroutine(
                                child_old.file_ast, child_old.sline, child_old.name
                            )
                        elif prototype.get_type() == FUNCTION_TYPE_ID:
                            child = fortran_function(
                                child_old.file_ast, child_old.sline, child_old.name
                            )
                        child.copy_from(child_old)
                        # Replace in child and scope lists
                        self.children[i] = child
                        for (j, file_scope) in enumerate(child.file_ast.scope_list):
                            if file_scope is child_old:
                                child.file_ast.scope_list[j] = child
                    if child.get_type() == prototype.get_type():
                        prototype.resolve_link(obj_tree)
                        child.copy_interface(prototype)
                        break

    def require_link(self):
        return True


class fortran_subroutine(fortran_scope):
    def __init__(
        self,
        file_ast: fortran_ast,
        line_number: int,
        name: str,
        args: str = "",
        mod_flag: bool = False,
        keywords: list = None,
    ):
        super().__init__(file_ast, line_number, name, keywords)
        self.args: str = args.replace(" ", "")
        self.args_snip: str = self.args
        self.arg_objs: list = []
        self.in_children: list = []
        self.missing_args: list = []
        self.mod_scope: bool = mod_flag

    def is_mod_scope(self):
        return self.mod_scope

    def is_callable(self):
        return True

    def copy_interface(self, copy_source: fortran_subroutine) -> list[str]:
        # Copy arguments
        self.args = copy_source.args
        self.args_snip = copy_source.args_snip
        self.arg_objs = copy_source.arg_objs
        # Get current fields
        child_names = []
        for child in self.children:
            child_names.append(child.name.lower())
        # Import arg_objs from copy object
        self.in_children = []
        for child in copy_source.arg_objs:
            if child is None:
                continue
            if child.name.lower() not in child_names:
                self.in_children.append(child)
        return child_names

    def get_children(self, public_only=False):
        tmp_list = copy.copy(self.children)
        tmp_list.extend(self.in_children)
        return tmp_list

    def resolve_arg_link(self, obj_tree):
        if (self.args == "") or (len(self.in_children) > 0):
            return
        arg_list = self.args.replace(" ", "").split(",")
        arg_list_lower = self.args.lower().replace(" ", "").split(",")
        self.arg_objs = [None] * len(arg_list)
        # check_objs = copy.copy(self.children)
        # for child in self.children:
        #     if child.is_external_int():
        #         check_objs += child.get_children()
        self.missing_args = []
        for child in self.children:
            ind = -1
            for (i, arg) in enumerate(arg_list_lower):
                if arg == child.name.lower():
                    ind = i
                    break
                # If an argument is part of an interface block go through the
                # block's children i.e. functions and subroutines to see if one matches
                elif child.name.lower().startswith("#gen_int"):
                    for sub_child in child.children:
                        if arg == sub_child.name:
                            self.arg_objs[i] = sub_child
                            break

            if ind < 0:
                if child.keywords.count(KEYWORD_ID_DICT["intent"]) > 0:
                    self.missing_args.append(child)
            else:
                self.arg_objs[ind] = child
                if child.is_optional():
                    arg_list[ind] = f"{arg_list[ind]}={arg_list[ind]}"
        self.args_snip = ",".join(arg_list)

    def resolve_link(self, obj_tree):
        self.resolve_arg_link(obj_tree)

    def require_link(self):
        return True

    def get_type(self, no_link=False):
        return SUBROUTINE_TYPE_ID

    def get_snippet(self, name_replace=None, drop_arg=-1):
        arg_list = self.args_snip.split(",")
        if (drop_arg >= 0) and (drop_arg < len(arg_list)):
            del arg_list[drop_arg]
        arg_snip = None
        if len(arg_list) > 0:
            arg_str, arg_snip = self.get_placeholders(arg_list)
        else:
            arg_str = "()"
        name = self.name
        if name_replace is not None:
            name = name_replace
        snippet = None
        if arg_snip is not None:
            snippet = name + arg_snip
        return name + arg_str, snippet

    def get_desc(self):
        return "SUBROUTINE"

    def get_hover(self, long=False, include_doc=True, drop_arg=-1):
        sub_sig, _ = self.get_snippet(drop_arg=drop_arg)
        keyword_list = get_keywords(self.keywords)
        keyword_list.append(f"{self.get_desc()} ")
        hover_array = [" ".join(keyword_list) + sub_sig]
        hover_array = self.get_docs_full(hover_array, long, include_doc, drop_arg)
        return "\n ".join(hover_array), long

    def get_docs_full(
        self, hover_array: list[str], long=False, include_doc=True, drop_arg=-1
    ):
        doc_str = self.get_documentation()
        if include_doc and doc_str is not None:
            hover_array[0] += "\n" + doc_str
        if long:
            for i, arg_obj in enumerate(self.arg_objs):
                if arg_obj is None or i == drop_arg:
                    continue
                arg_doc, _ = arg_obj.get_hover(include_doc=False)
                hover_array.append(f"{arg_doc} :: {arg_obj.name}")
                doc_str = arg_obj.get_documentation()
                if include_doc and (doc_str is not None):
                    hover_array += doc_str.splitlines()
        return hover_array

    def get_signature(self, drop_arg=-1):
        arg_sigs = []
        arg_list = self.args.split(",")
        for (i, arg_obj) in enumerate(self.arg_objs):
            if i == drop_arg:
                continue
            if arg_obj is None:
                arg_sigs.append({"label": arg_list[i]})
            else:
                if arg_obj.is_optional():
                    label = f"{arg_obj.name.lower()}={arg_obj.name.lower()}"
                else:
                    label = arg_obj.name.lower()
                arg_sigs.append(
                    {"label": label, "documentation": arg_obj.get_hover()[0]}
                )
        call_sig, _ = self.get_snippet()
        return call_sig, self.get_documentation(), arg_sigs

    def get_interface_array(
        self, keywords: list[str], signature: str, change_arg=-1, change_strings=None
    ):
        interface_array = [" ".join(keywords) + signature]
        for i, arg_obj in enumerate(self.arg_objs):
            if arg_obj is None:
                return None
            arg_doc, _ = arg_obj.get_hover(include_doc=False)
            if i == change_arg:
                i0 = arg_doc.lower().find(change_strings[0].lower())
                if i0 >= 0:
                    i1 = i0 + len(change_strings[0])
                    arg_doc = arg_doc[:i0] + change_strings[1] + arg_doc[i1:]
            interface_array.append(f"{arg_doc} :: {arg_obj.name}")
        return interface_array

    def get_interface(self, name_replace=None, change_arg=-1, change_strings=None):
        sub_sig, _ = self.get_snippet(name_replace=name_replace)
        keyword_list = get_keywords(self.keywords)
        keyword_list.append("SUBROUTINE ")
        interface_array = self.get_interface_array(
            keyword_list, sub_sig, change_arg, change_strings
        )
        name = self.name
        if name_replace is not None:
            name = name_replace
        interface_array.append(f"END SUBROUTINE {name}")
        return "\n".join(interface_array)

    def check_valid_parent(self):
        if self.parent is not None:
            parent_type = self.parent.get_type()
            if (parent_type == CLASS_TYPE_ID) or (parent_type >= BLOCK_TYPE_ID):
                return False
        return True

    def get_diagnostics(self):
        errors = []
        for missing_obj in self.missing_args:
            new_diag = fortran_diagnostic(
                missing_obj.sline - 1,
                f'Variable "{missing_obj.name}" with INTENT keyword not found in'
                " argument list",
                severity=1,
                find_word=missing_obj.name,
            )
            errors.append(new_diag)
        implicit_flag = self.get_implicit()
        if (implicit_flag is None) or implicit_flag:
            return errors
        arg_list = self.args.replace(" ", "").split(",")
        for (i, arg_obj) in enumerate(self.arg_objs):
            if arg_obj is None:
                arg_name = arg_list[i].strip()
                new_diag = fortran_diagnostic(
                    self.sline - 1,
                    f'No matching declaration found for argument "{arg_name}"',
                    severity=1,
                    find_word=arg_name,
                )
                errors.append(new_diag)
        return errors


class fortran_function(fortran_subroutine):
    def __init__(
        self,
        file_ast: fortran_ast,
        line_number: int,
        name: str,
        args: str = "",
        mod_flag: bool = False,
        keywords: list = None,
        result_type: str = None,
        result_name: str = None,
    ):
        super().__init__(file_ast, line_number, name, args, mod_flag, keywords)
        self.args: str = args.replace(" ", "").lower()
        self.args_snip: str = self.args
        self.arg_objs: list = []
        self.in_children: list = []
        self.missing_args: list = []
        self.mod_scope: bool = mod_flag
        self.result_name: str = result_name
        self.result_type: str = result_type
        self.result_obj: fortran_var = None
        # Set the implicit result() name to be the function name
        if self.result_name is None:
            self.result_name = self.name

    def copy_interface(self, copy_source: fortran_function):
        # Call the parent class method
        child_names = super().copy_interface(copy_source)
        # Return specific options
        self.result_name = copy_source.result_name
        self.result_type = copy_source.result_type
        self.result_obj = copy_source.result_obj
        if copy_source.result_obj is not None:
            if copy_source.result_obj.name.lower() not in child_names:
                self.in_children.append(copy_source.result_obj)

    def resolve_link(self, obj_tree):
        self.resolve_arg_link(obj_tree)
        result_var_lower = self.result_name.lower()
        for child in self.children:
            if child.name.lower() == result_var_lower:
                self.result_obj = child
                # Update result value and type
                self.result_name = child.name
                self.result_type = child.get_desc()

    def get_type(self, no_link=False):
        return FUNCTION_TYPE_ID

    def get_desc(self):
        if self.result_type:
            return self.result_type + " FUNCTION"
        return "FUNCTION"

    def is_callable(self):
        return False

    def get_hover(
        self, long: bool = False, include_doc: bool = True, drop_arg: int = -1
    ) -> tuple[str, bool]:
        """Construct the hover message for a FUNCTION.
        Two forms are produced here the `long` i.e. the normal for hover requests

        [MODIFIERS] FUNCTION NAME([ARGS]) RESULT(RESULT_VAR)
          TYPE, [ARG_MODIFIERS] :: [ARGS]
          TYPE, [RESULT_MODIFIERS] :: RESULT_VAR

        note: intrinsic functions will display slightly different,
        `RESULT_VAR` and its `TYPE` might not always be present

        short form, used when functions are arguments in functions and subroutines:

        FUNCTION NAME([ARGS]) :: ARG_LIST_NAME

        Parameters
        ----------
        long : bool, optional
            toggle between long and short hover results, by default False
        include_doc : bool, optional
            if to include any documentation, by default True
        drop_arg : int, optional
            Ignore argument at position `drop_arg` in the argument list, by default -1

        Returns
        -------
        tuple[str, bool]
            String representative of the hover message and the `long` flag used
        """
        fun_sig, _ = self.get_snippet(drop_arg=drop_arg)
        # short hover messages do not include the result()
        fun_sig += f" RESULT({self.result_name})" if long else ""
        keyword_list = get_keywords(self.keywords)
        keyword_list.append("FUNCTION")

        hover_array = [f"{' '.join(keyword_list)} {fun_sig}"]
        hover_array = self.get_docs_full(hover_array, long, include_doc, drop_arg)
        # Only append the return value if using long form
        if self.result_obj and long:
            arg_doc, _ = self.result_obj.get_hover(include_doc=False)
            hover_array.append(f"{arg_doc} :: {self.result_obj.name}")
        # intrinsic functions, where the return type is missing but can be inferred
        elif self.result_type and long:
            # prepend type to function signature
            hover_array[0] = f"{self.result_type} {hover_array[0]}"
        return "\n ".join(hover_array), long

    def get_interface(self, name_replace=None, change_arg=-1, change_strings=None):
        fun_sig, _ = self.get_snippet(name_replace=name_replace)
        fun_sig += f" RESULT({self.result_name})"
        # XXX:
        keyword_list = []
        if self.result_type:
            keyword_list.append(self.result_type)
        keyword_list += get_keywords(self.keywords)
        keyword_list.append("FUNCTION ")

        interface_array = self.get_interface_array(
            keyword_list, fun_sig, change_arg, change_strings
        )
        if self.result_obj is not None:
            arg_doc, _ = self.result_obj.get_hover(include_doc=False)
            interface_array.append(f"{arg_doc} :: {self.result_obj.name}")
        name = self.name
        if name_replace is not None:
            name = name_replace
        interface_array.append(f"END FUNCTION {name}")
        return "\n".join(interface_array)


class fortran_type(fortran_scope):
    def __init__(
        self, file_ast: fortran_ast, line_number: int, name: str, keywords: list
    ):
        super().__init__(file_ast, line_number, name, keywords)
        #
        self.in_children: list = []
        self.inherit = None
        self.inherit_var = None
        self.inherit_tmp = None
        self.inherit_version = -1
        if keywords.count(KEYWORD_ID_DICT["abstract"]) > 0:
            self.abstract = True
        else:
            self.abstract = False
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
            # Resolve parent inheritance while avoiding circular recursion
            self.inherit_tmp = self.inherit
            self.inherit = None
            self.inherit_var.resolve_inherit(obj_tree, inherit_version)
            self.inherit = self.inherit_tmp
            self.inherit_tmp = None
            # Get current fields
            child_names = []
            for child in self.children:
                child_names.append(child.name.lower())
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
        else:
            parent_type = self.parent.get_type()
            if (parent_type == CLASS_TYPE_ID) or (parent_type >= BLOCK_TYPE_ID):
                return False
        return True

    def get_diagnostics(self):
        errors = []
        for in_child in self.in_children:
            if (not self.abstract) and (
                in_child.keywords.count(KEYWORD_ID_DICT["deferred"]) > 0
            ):
                new_diag = fortran_diagnostic(
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
                new_diag = fortran_diagnostic(
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


class fortran_block(fortran_scope):
    def __init__(self, file_ast: fortran_ast, line_number: int, name: str):
        super().__init__(file_ast, line_number, name)

    def get_type(self, no_link=False):
        return BLOCK_TYPE_ID

    def get_desc(self):
        return "BLOCK"

    def get_children(self, public_only=False):
        return copy.copy(self.children)

    def req_named_end(self):
        return True


class fortran_do(fortran_block):
    def __init__(self, file_ast: fortran_ast, line_number: int, name: str):
        super().__init__(file_ast, line_number, name)

    def get_type(self, no_link=False):
        return DO_TYPE_ID

    def get_desc(self):
        return "DO"


class fortran_where(fortran_block):
    def __init__(self, file_ast: fortran_ast, line_number: int, name: str):
        super().__init__(file_ast, line_number, name)

    def get_type(self, no_link=False):
        return WHERE_TYPE_ID

    def get_desc(self):
        return "WHERE"


class fortran_if(fortran_block):
    def __init__(self, file_ast: fortran_ast, line_number: int, name: str):
        super().__init__(file_ast, line_number, name)

    def get_type(self, no_link=False):
        return IF_TYPE_ID

    def get_desc(self):
        return "IF"


class fortran_associate(fortran_block):
    def __init__(self, file_ast: fortran_ast, line_number: int, name: str):
        super().__init__(file_ast, line_number, name)
        self.assoc_links = []

    def get_type(self, no_link=False):
        return ASSOC_TYPE_ID

    def get_desc(self):
        return "ASSOCIATE"

    def create_binding_variable(self, file_ast, line_number, bound_name, link_var):
        new_var = fortran_var(file_ast, line_number, bound_name, "UNKNOWN", [])
        self.assoc_links.append([new_var, bound_name, link_var])
        return new_var

    def resolve_link(self, obj_tree):
        for assoc_link in self.assoc_links:
            var_stack = get_var_stack(assoc_link[2])
            if len(var_stack) > 1:
                type_scope = climb_type_tree(var_stack, self, obj_tree)
                if type_scope is None:
                    continue
                var_obj = find_in_scope(type_scope, var_stack[-1], obj_tree)
                if var_obj is not None:
                    assoc_link[0].link_obj = var_obj
            else:
                var_obj = find_in_scope(self, assoc_link[2], obj_tree)
                if var_obj is not None:
                    assoc_link[0].link_obj = var_obj

    def require_link(self):
        return True


class fortran_enum(fortran_block):
    def __init__(self, file_ast: fortran_ast, line_number: int, name: str):
        super().__init__(file_ast, line_number, name)

    def get_type(self, no_link=False):
        return ENUM_TYPE_ID

    def get_desc(self):
        return "ENUM"


class fortran_select(fortran_block):
    def __init__(self, file_ast: fortran_ast, line_number: int, name: str, select_info):
        super().__init__(file_ast, line_number, name)
        self.select_type = select_info.type
        self.binding_name = None
        self.bound_var = None
        self.binding_type = None
        if self.select_type == 2:
            binding_split = select_info.binding.split("=>")
            if len(binding_split) == 1:
                self.bound_var = binding_split[0].strip()
            elif len(binding_split) == 2:
                self.binding_name = binding_split[0].strip()
                self.bound_var = binding_split[1].strip()
        elif self.select_type == 3:
            self.binding_type = select_info.binding
        # Close previous "TYPE IS" region if open
        if (
            (file_ast.current_scope is not None)
            and (file_ast.current_scope.get_type() == SELECT_TYPE_ID)
            and file_ast.current_scope.is_type_region()
        ):
            file_ast.end_scope(line_number)

    def get_type(self, no_link=False):
        return SELECT_TYPE_ID

    def get_desc(self):
        return "SELECT"

    def is_type_binding(self):
        return self.select_type == 2

    def is_type_region(self):
        return (self.select_type == 3) or (self.select_type == 4)

    def create_binding_variable(self, file_ast, line_number, var_desc, case_type):
        if self.parent.get_type() != SELECT_TYPE_ID:
            return None
        binding_name = None
        bound_var = None
        if (self.parent is not None) and self.parent.is_type_binding():
            binding_name = self.parent.binding_name
            bound_var = self.parent.bound_var
        # Check for default case
        if (binding_name is not None) and (case_type != 4):
            bound_var = None
        # Create variable
        if binding_name is not None:
            return fortran_var(
                file_ast, line_number, binding_name, var_desc, [], link_obj=bound_var
            )
        elif (binding_name is None) and (bound_var is not None):
            return fortran_var(file_ast, line_number, bound_var, var_desc, [])
        return None


class fortran_int(fortran_scope):
    def __init__(
        self,
        file_ast: fortran_ast,
        line_number: int,
        name: str,
        abstract: bool = False,
    ):
        super().__init__(file_ast, line_number, name)
        self.mems = []
        self.abstract = abstract
        self.external = name.startswith("#GEN_INT") and (not abstract)

    def get_type(self, no_link=False):
        return INTERFACE_TYPE_ID

    def get_desc(self):
        return "INTERFACE"

    def is_callable(self):
        return True

    def is_external_int(self):
        return self.external

    def is_abstract(self):
        return self.abstract

    def resolve_link(self, obj_tree):
        if self.parent is None:
            return
        self.mems = []
        for member in self.members:
            mem_obj = find_in_scope(self.parent, member, obj_tree)
            if mem_obj is not None:
                self.mems.append(mem_obj)

    def require_link(self):
        return True


class fortran_var(fortran_obj):
    def __init__(
        self,
        file_ast: fortran_ast,
        line_number: int,
        name: str,
        var_desc: str,
        keywords: list,
        keyword_info: dict = None,
        link_obj=None,
    ):
        super().__init__()
        if keyword_info is None:
            keyword_info = {}
        self.file_ast: fortran_ast = file_ast
        self.sline: int = line_number
        self.eline: int = line_number
        self.name: str = name
        self.desc: str = var_desc
        self.keywords: list = keywords
        self.keyword_info: dict = keyword_info
        self.callable: bool = FRegex.CLASS_VAR.match(var_desc) is not None
        self.children: list = []
        self.use: list[USE_line] = []
        self.link_obj = None
        self.type_obj = None
        self.is_const: bool = False
        self.is_external: bool = False
        self.param_val: str = None
        self.link_name: str = None
        self.FQSN: str = self.name.lower()
        if link_obj is not None:
            self.link_name = link_obj.lower()
        if file_ast.enc_scope_name is not None:
            self.FQSN = file_ast.enc_scope_name.lower() + "::" + self.name.lower()
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
            self.FQSN = enc_scope.lower() + "::" + self.name.lower()
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

    def get_desc(self):
        if self.link_obj is not None:
            return self.link_obj.get_desc()
        # Normal variable
        return self.desc

    def get_type_obj(self, obj_tree):
        if self.link_obj is not None:
            return self.link_obj.get_type_obj(obj_tree)
        if (self.type_obj is None) and (self.parent is not None):
            type_name = get_paren_substring(self.desc)
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

    def set_dim(self, dim_str):
        if KEYWORD_ID_DICT["dimension"] not in self.keywords:
            self.keywords.append(KEYWORD_ID_DICT["dimension"])
            self.keyword_info["dimension"] = dim_str
            self.keywords.sort()

    def get_snippet(self, name_replace=None, drop_arg=-1):
        name = self.name
        if name_replace is not None:
            name = name_replace
        if self.link_obj is not None:
            return self.link_obj.get_snippet(name, drop_arg)
        # Normal variable
        return None, None

    def get_hover(self, long=False, include_doc=True, drop_arg=-1):
        doc_str = self.get_documentation()
        hover_str = ", ".join(
            [self.desc] + get_keywords(self.keywords, self.keyword_info)
        )
        # TODO: at this stage we can mae this lowercase
        # Add parameter value in the output
        if self.is_parameter() and self.param_val:
            hover_str += f" :: {self.name} = {self.param_val}"
        if include_doc and (doc_str is not None):
            hover_str += "\n {0}".format("\n ".join(doc_str.splitlines()))
        return hover_str, True

    def is_optional(self):
        if self.keywords.count(KEYWORD_ID_DICT["optional"]) > 0:
            return True
        else:
            return False

    def is_callable(self):
        return self.callable

    def is_parameter(self):
        return self.is_const

    def set_parameter_val(self, val: str):
        self.param_val = val

    def set_external_attr(self):
        self.keywords.append(KEYWORD_ID_DICT["external"])
        self.is_external = True

    def check_definition(self, obj_tree, known_types={}, interface=False):
        # Check for type definition in scope
        type_match = FRegex.DEF_KIND.match(self.desc)
        if type_match is not None:
            var_type = type_match.group(1).strip().lower()
            if var_type == "procedure":
                return None, known_types
            desc_obj_name = type_match.group(2).strip().lower()
            if desc_obj_name not in known_types:
                type_def = find_in_scope(
                    self.parent, desc_obj_name, obj_tree, interface=interface
                )
                if type_def is None:
                    type_defs = find_in_workspace(
                        obj_tree, desc_obj_name, filter_public=True, exact_match=True
                    )
                    known_types[desc_obj_name] = None
                    var_type = type_match.group(1).strip().lower()
                    filter_id = VAR_TYPE_ID
                    if (var_type == "class") or (var_type == "type"):
                        filter_id = CLASS_TYPE_ID
                    for type_def in type_defs:
                        if type_def.get_type() == filter_id:
                            known_types[desc_obj_name] = (1, type_def)
                            break
                else:
                    known_types[desc_obj_name] = (0, type_def)
            type_info = known_types[desc_obj_name]
            if type_info is not None:
                if type_info[0] == 1:
                    if interface:
                        out_diag = fortran_diagnostic(
                            self.sline - 1,
                            message=(
                                f'Object "{desc_obj_name}" not imported in interface'
                            ),
                            severity=1,
                            find_word=desc_obj_name,
                        )
                    else:
                        out_diag = fortran_diagnostic(
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


class fortran_meth(fortran_var):
    def __init__(
        self,
        file_ast: fortran_ast,
        line_number: int,
        name: str,
        var_desc: str,
        keywords: list,
        keyword_info: dict,
        link_obj=None,
    ):
        super().__init__(
            file_ast, line_number, name, var_desc, keywords, keyword_info, link_obj
        )
        self.drop_arg: int = -1
        self.pass_name: str = keyword_info.get("pass")
        if link_obj is None:
            self.link_name = get_paren_substring(var_desc.lower())

    def set_parent(self, parent_obj):
        self.parent = parent_obj
        if self.parent.get_type() == CLASS_TYPE_ID:
            if self.keywords.count(KEYWORD_ID_DICT["nopass"]) == 0:
                self.drop_arg = 0
            if (
                (self.parent.contains_start is not None)
                and (self.sline > self.parent.contains_start)
                and (self.link_name is None)
            ):
                self.link_name = self.name.lower()

    def get_snippet(self, name_replace=None, drop_arg=-1):
        if self.link_obj is not None:
            if name_replace is None:
                name = self.name
            else:
                name = name_replace
            return self.link_obj.get_snippet(name, self.drop_arg)
        return None, None

    def get_type(self, no_link=False):
        if (not no_link) and (self.link_obj is not None):
            return self.link_obj.get_type()
        # Generic
        return METH_TYPE_ID

    def get_documentation(self):
        if (self.link_obj is not None) and (self.doc_str is None):
            return self.link_obj.get_documentation()
        return self.doc_str

    def get_hover(self, long=False, include_doc=True, drop_arg=-1):
        doc_str = self.get_documentation()
        if long:
            if self.link_obj is None:
                sub_sig, _ = self.get_snippet()
                hover_str = f"{self.get_desc()} {sub_sig}"
                if include_doc and (doc_str is not None):
                    hover_str += f"\n{doc_str}"
            else:
                link_hover, _ = self.link_obj.get_hover(
                    long=True, include_doc=include_doc, drop_arg=self.drop_arg
                )
                hover_split = link_hover.splitlines()
                call_sig = hover_split[0]
                paren_start = call_sig.rfind("(")
                link_name_len = len(self.link_obj.name)
                call_sig = (
                    call_sig[: paren_start - link_name_len]
                    + self.name
                    + call_sig[paren_start:]
                )
                hover_split = hover_split[1:]
                if include_doc and (self.doc_str is not None):
                    # Replace linked docs with current object's docs
                    if (len(hover_split) > 0) and (hover_split[0].count("!!") > 0):
                        for (i, hover_line) in enumerate(hover_split):
                            if hover_line.count("!!") == 0:
                                hover_split = hover_split[i:]
                                break
                        else:  # All lines are docs
                            hover_split = []
                    hover_split = [self.doc_str] + hover_split
                hover_str = "\n".join([call_sig] + hover_split)
            return hover_str, True
        else:
            hover_str = ", ".join([self.desc] + get_keywords(self.keywords))
            if include_doc and (doc_str is not None):
                hover_str += f"\n{doc_str}"
            return hover_str, True

    def get_signature(self, drop_arg=-1):
        if self.link_obj is not None:
            call_sig, _ = self.get_snippet()
            _, _, arg_sigs = self.link_obj.get_signature(self.drop_arg)
            return call_sig, self.get_documentation(), arg_sigs
        return None, None, None

    def get_interface(self, name_replace=None, change_arg=-1, change_strings=None):
        if self.link_obj is not None:
            return self.link_obj.get_interface(
                name_replace, self.drop_arg, change_strings
            )
        return None

    def resolve_link(self, obj_tree):
        if self.link_name is None:
            return
        if self.parent is not None:
            if self.parent.get_type() == CLASS_TYPE_ID:
                link_obj = find_in_scope(self.parent.parent, self.link_name, obj_tree)
            else:
                link_obj = find_in_scope(self.parent, self.link_name, obj_tree)
            if link_obj is not None:
                self.link_obj = link_obj
                if self.pass_name is not None:
                    self.pass_name = self.pass_name.lower()
                    for i, arg in enumerate(link_obj.args_snip.split(",")):
                        if arg.lower() == self.pass_name:
                            self.drop_arg = i
                            break

    def is_callable(self):
        return True

    def check_definition(self, obj_tree, known_types={}, interface=False):
        return None, known_types


class fortran_ast:
    def __init__(self, file_obj=None):
        self.file = file_obj
        self.path: str = None
        if file_obj is not None:
            self.path = file_obj.path
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
        self.END_SCOPE_REGEX: Pattern = None
        self.enc_scope_name: str = None
        self.last_obj = None
        self.pending_doc: str = None

    def create_none_scope(self):
        """Create empty scope to hold non-module contained items"""
        if self.none_scope is not None:
            raise ValueError
        self.none_scope = fortran_program(self, 1, "main")
        self.add_scope(
            self.none_scope, re.compile(r"[ ]*END[ ]*PROGRAM", re.I), exportable=False
        )

    def get_enc_scope_name(self):
        """Get current enclosing scope name"""
        if self.current_scope is None:
            return None
        return self.current_scope.FQSN

    def add_scope(
        self,
        new_scope: fortran_scope,
        END_SCOPE_REGEX: Pattern[str],
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
                new_scope.FQSN = self.none_scope.FQSN + "::" + new_scope.name.lower()
                self.current_scope.add_child(new_scope)
                self.scope_stack.append(self.current_scope)
            else:
                if exportable:
                    self.global_dict[new_scope.FQSN] = new_scope
        else:
            self.current_scope.add_child(new_scope)
            self.scope_stack.append(self.current_scope)
        if self.END_SCOPE_REGEX is not None:
            self.end_stack.append(self.END_SCOPE_REGEX)
        self.current_scope = new_scope
        self.END_SCOPE_REGEX = END_SCOPE_REGEX
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
            self.END_SCOPE_REGEX = self.end_stack.pop()
        else:
            self.END_SCOPE_REGEX = None
        self.enc_scope_name = self.get_enc_scope_name()

    def add_variable(self, new_var: fortran_var):
        if self.current_scope is None:
            self.create_none_scope()
            new_var.FQSN = self.none_scope.FQSN + "::" + new_var.name.lower()
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
        self.private_list.append(self.enc_scope_name + "::" + name)

    def add_public(self, name: str):
        self.public_list.append(self.enc_scope_name + "::" + name)

    def add_use(
        self,
        mod_word: str,
        line_number: int,
        only_list: list = [],
        rename_map: dict = {},
    ):
        if self.current_scope is None:
            self.create_none_scope()
        self.current_scope.add_use(mod_word, line_number, only_list, rename_map)

    def add_include(self, path: str, line_number: int):
        self.include_statements.append(INCLUDE_info(line_number, path, None, []))

    def add_doc(self, doc_string: str, forward: bool = False):
        if doc_string == "":
            return
        if forward:
            self.pending_doc = doc_string
        else:
            if self.last_obj is not None:
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

    def end_ppif(self, line_number):
        if len(self.pp_if) > 0:
            self.pp_if[-1][1] = line_number - 1

    def get_scopes(self, line_number: int = None):
        if line_number is None:
            return self.scope_list
        scope_list = []
        for scope in self.scope_list:
            if (line_number >= scope.sline) and (line_number <= scope.eline):
                scope_list.append(scope)
                for ancestor in scope.get_ancestors():
                    scope_list.append(ancestor)
        if (len(scope_list) == 0) and (self.none_scope is not None):
            return [self.none_scope]
        return scope_list

    def get_inner_scope(self, line_number: int):
        scope_sline = -1
        curr_scope = None
        for scope in self.scope_list:
            if scope.sline > scope_sline:
                if (line_number >= scope.sline) and (line_number <= scope.eline):
                    curr_scope = scope
                    scope_sline = scope.sline
        if (curr_scope is None) and (self.none_scope is not None):
            return self.none_scope
        return curr_scope

    def get_object(self, FQSN: str):
        FQSN_split = FQSN.split("::")
        curr_obj = self.global_dict.get(FQSN_split[0])
        if curr_obj is None:
            # Look for non-exportable scopes
            for scope in self.scope_list:
                if FQSN_split[0] == scope.FQSN:
                    curr_obj = scope
                    break
        if curr_obj is None:
            return None
        if len(FQSN_split) > 1:
            for name in FQSN_split[1:]:
                next_obj = None
                for child in curr_obj.children:
                    if child.name.startswith("#GEN_INT"):
                        for int_child in child.get_children():
                            if int_child.name == name:
                                next_obj = int_child
                                break
                        if next_obj is not None:
                            break
                    if child.name == name:
                        next_obj = child
                        break
                if next_obj is None:
                    return None
                curr_obj = next_obj
        return curr_obj

    def resolve_includes(self, workspace, path: str = None):
        file_dir = os.path.dirname(self.path)
        for inc in self.include_statements:
            file_path = os.path.normpath(os.path.join(file_dir, inc.path))
            if path and not (path == file_path):
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
            errors.append(fortran_diagnostic(error[1] - 1, message=message, severity=1))
        for scope in tmp_list:
            if not scope.check_valid_parent():
                errors.append(
                    fortran_diagnostic(
                        scope.sline - 1,
                        message=f'Invalid parent for "{scope.get_desc()}" declaration',
                        severity=1,
                    )
                )
            errors += scope.check_use(obj_tree)
            errors += scope.check_definitions(obj_tree)
            errors += scope.get_diagnostics()
        return errors, self.parse_errors
