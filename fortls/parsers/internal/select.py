from __future__ import annotations

from typing import TYPE_CHECKING

from fortls.constants import SELECT_TYPE_ID

from .block import Block
from .variable import Variable

if TYPE_CHECKING:
    from .ast import FortranAST


class Select(Block):
    def __init__(
        self,
        file_ast: FortranAST,
        line_number: int,
        name: str,
        select_info,
    ):
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
        return self.select_type in [3, 4]

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
            return Variable(
                file_ast, line_number, binding_name, var_desc, [], link_obj=bound_var
            )
        elif bound_var is not None:
            return Variable(file_ast, line_number, bound_var, var_desc, [])
        return None
