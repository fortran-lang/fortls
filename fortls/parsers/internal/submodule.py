from __future__ import annotations

from typing import TYPE_CHECKING
from typing import Type as T

from fortls.constants import (
    BASE_TYPE_ID,
    FUNCTION_TYPE_ID,
    INTERFACE_TYPE_ID,
    SUBMODULE_TYPE_ID,
    SUBROUTINE_TYPE_ID,
)

from .function import Function
from .module import Module
from .subroutine import Subroutine

if TYPE_CHECKING:
    from .ast import FortranAST
    from .interface import Interface
    from .scope import Scope


class Submodule(Module):
    def __init__(
        self,
        file_ast: FortranAST,
        line_number: int,
        name: str,
        ancestor_name: str = "",
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
        if not self.ancestor_name:
            return
        if self.ancestor_name in obj_tree:
            self.ancestor_obj = obj_tree[self.ancestor_name][0]

    def require_inherit(self):
        return True

    def resolve_link(self, obj_tree):
        def get_ancestor_interfaces(
            ancestor_children: list[Scope],
        ) -> list[T[Interface]]:
            interfaces = []
            for child in ancestor_children:
                if child.get_type() != INTERFACE_TYPE_ID:
                    continue
                for interface in child.children:
                    interface_type = interface.get_type()
                    if (
                        interface_type
                        in (SUBROUTINE_TYPE_ID, FUNCTION_TYPE_ID, BASE_TYPE_ID)
                    ) and interface.is_mod_scope():
                        interfaces.append(interface)
            return interfaces

        def create_child_from_prototype(child: Scope, interface: Interface):
            if interface.get_type() == SUBROUTINE_TYPE_ID:
                return Subroutine(child.file_ast, child.sline, child.name)
            elif interface.get_type() == FUNCTION_TYPE_ID:
                return Function(child.file_ast, child.sline, child.name)
            else:
                raise ValueError(f"Unsupported interface type: {interface.get_type()}")

        def replace_child_in_scope_list(child: Scope, child_old: Scope):
            for i, file_scope in enumerate(child.file_ast.scope_list):
                if file_scope is child_old:
                    child.file_ast.scope_list[i] = child
            return child

        # Link subroutine/function implementations to prototypes
        if self.ancestor_obj is None:
            return

        ancestor_interfaces = get_ancestor_interfaces(self.ancestor_obj.children)
        # Match interface definitions to implementations
        for interface in ancestor_interfaces:
            for i, child in enumerate(self.children):
                if child.name.lower() != interface.name.lower():
                    continue

                if child.get_type() == BASE_TYPE_ID:
                    child_old = child
                    child = create_child_from_prototype(child_old, interface)
                    child.copy_from(child_old)
                    self.children[i] = child
                    child = replace_child_in_scope_list(child, child_old)

                if child.get_type() == interface.get_type():
                    interface.link_obj = child
                    interface.resolve_link(obj_tree)
                    child.copy_interface(interface)
                    break

    def require_link(self):
        return True
