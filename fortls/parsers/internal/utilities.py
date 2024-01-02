from __future__ import annotations

import contextlib
from typing import TYPE_CHECKING

from fortls.constants import MODULE_TYPE_ID

from .imports import Import, ImportTypes
from .use import Use

if TYPE_CHECKING:
    from .scope import Scope


def get_use_tree(
    scope: Scope,
    use_dict: dict[str, Use | Import],
    obj_tree: dict,
    only_list: list[str] = None,
    rename_map: dict[str, str] = None,
    curr_path: list[str] = None,
):
    if only_list is None:
        only_list = set()
    if rename_map is None:
        rename_map = {}
    if curr_path is None:
        curr_path = []

    def intersect_only(use_stmnt: Use | Import):
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

    # Detect and break circular references
    if scope.FQSN in curr_path:
        return use_dict
    new_path = curr_path + [scope.FQSN]
    # Add recursively
    for use_stmnt in scope.use:
        # if use_stmnt.mod_name not in obj_tree:
        if type(use_stmnt) is Use and use_stmnt.mod_name not in obj_tree:
            continue
        # Escape any IMPORT, NONE statements
        if type(use_stmnt) is Import and use_stmnt.import_type is ImportTypes.NONE:
            continue
        # Intersect parent and current ONLY list and renaming
        if not only_list:
            merged_use_list = use_stmnt.only_list.copy()
            merged_rename = use_stmnt.rename_map.copy()
        elif len(use_stmnt.only_list) == 0:
            merged_use_list = only_list.copy()
            merged_rename = rename_map.copy()
        else:
            merged_use_list, merged_rename = intersect_only(use_stmnt)
            if len(merged_use_list) == 0:
                continue
        # Update ONLY list and renaming for current module
        # If you have
        # USE MOD, ONLY: A
        # USE MOD, ONLY: B
        # or
        # IMPORT VAR
        # IMPORT VAR2
        use_dict_mod = use_dict.get(use_stmnt.mod_name)
        if use_dict_mod is not None:
            old_len = len(use_dict_mod.only_list)
            if old_len > 0 and merged_use_list:
                only_len = old_len
                for only_name in merged_use_list:
                    use_dict_mod.only_list.add(only_name)
                    if len(use_dict_mod.only_list) == only_len:
                        continue
                    only_len = len(use_dict_mod.only_list)
                    new_rename = merged_rename.get(only_name)
                    if new_rename is None:
                        continue
                    use_dict_mod.rename_map = merged_rename
                    use_dict[use_stmnt.mod_name] = use_dict_mod
            else:
                use_dict[use_stmnt.mod_name] = Use(use_stmnt.mod_name)
            # Skip if we have already visited module with the same only list
            if old_len == len(use_dict_mod.only_list):
                continue
        else:
            if type(use_stmnt) is Use:
                use_dict[use_stmnt.mod_name] = Use(
                    mod_name=use_stmnt.mod_name,
                    only_list=set(merged_use_list),
                    rename_map=merged_rename,
                )
            elif type(use_stmnt) is Import:
                use_dict[use_stmnt.mod_name] = Import(
                    name=use_stmnt.mod_name,
                    import_type=use_stmnt.import_type,
                    only_list=set(merged_use_list),
                    rename_map=merged_rename,
                )
                with contextlib.suppress(AttributeError):
                    use_dict[use_stmnt.mod_name].scope = scope.parent.parent
        # Do not descent the IMPORT tree, because it does not exist
        if type(use_stmnt) is Import:
            continue
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
    scope: Scope,
    var_name: str,
    obj_tree: dict,
    interface: bool = False,
    local_only: bool = False,
    var_line_number: int = None,
):
    from .include import Include

    def check_scope(
        local_scope: Scope,
        var_name_lower: str,
        filter_public: bool = False,
        var_line_number: int = None,
    ):
        from .function import Function

        for child in local_scope.get_children():
            if child.name.startswith("#GEN_INT"):
                tmp_var = check_scope(child, var_name_lower, filter_public)
                if tmp_var is not None:
                    return tmp_var
            is_private = child.vis < 0 or (local_scope.def_vis < 0 and child.vis <= 0)
            if filter_public and is_private:
                continue
            if child.name.lower() == var_name_lower:
                # For functions with an implicit result() variable the name
                # of the function is used. If we are hovering over the function
                # definition, we do not want the implicit result() to be returned.
                # If scope is from a function and child's name is same as functions name
                # and start of scope i.e. function definition is equal to the request ln
                # then we are need to skip this child
                if (
                    isinstance(local_scope, Function)
                    and local_scope.name.lower() == child.name.lower()
                    and var_line_number in (local_scope.sline, local_scope.eline)
                ):
                    return None

                return child
        return None

    def check_import_scope(scope: Scope, var_name_lower: str):
        for use_stmnt in scope.use:
            if type(use_stmnt) is not Import:
                continue
            if use_stmnt.import_type == ImportTypes.ONLY:
                # Check if name is in only list
                if var_name_lower in use_stmnt.only_list:
                    return ImportTypes.ONLY
            # Get the parent scope
            elif use_stmnt.import_type == ImportTypes.ALL:
                return ImportTypes.ALL
            # Skip looking for parent scope
            elif use_stmnt.import_type == ImportTypes.NONE:
                return ImportTypes.NONE
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
                return Include(inc.file.ast, inc.line_number, inc.path)

    # Setup USE search
    use_dict = get_use_tree(scope, {}, obj_tree)
    # Look in found use modules
    for use_mod, use_info in use_dict.items():
        # If use_mod is Import then it will not exist in the obj_tree
        if type(use_info) is Import:
            continue
        use_scope = obj_tree[use_mod][0]
        # Module name is request
        if use_mod.lower() == var_name_lower:
            return use_scope
        # Filter children by only_list
        if len(use_info.only_list) > 0 and var_name_lower not in use_info.only_list:
            continue
        mod_name = use_info.rename_map.get(var_name_lower, var_name_lower)
        tmp_var = check_scope(use_scope, mod_name, filter_public=True)
        if tmp_var is not None:
            return tmp_var
    # Only search local and imported names for interfaces
    import_type = ImportTypes.DEFAULT
    if interface:
        import_type = check_import_scope(scope, var_name_lower)
        if import_type is None:
            return None
    # Check parent scopes
    if scope.parent is not None and import_type != ImportTypes.NONE:
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
    for _, obj_packed in obj_tree.items():
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


def climb_type_tree(var_stack, curr_scope: Scope, obj_tree: dict):
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
