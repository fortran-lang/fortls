from __future__ import annotations

from typing import TYPE_CHECKING

from fortls.constants import INTERFACE_TYPE_ID

from .scope import Scope
from .utilities import find_in_scope

if TYPE_CHECKING:
    from .ast import FortranAST


class Interface(Scope):
    def __init__(
        self,
        file_ast: FortranAST,
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
