from __future__ import annotations

import copy
from typing import TYPE_CHECKING

from fortls.constants import BLOCK_TYPE_ID

from .scope import Scope

if TYPE_CHECKING:
    from .ast import FortranAST


class Block(Scope):
    def __init__(self, file_ast: FortranAST, line_number: int, name: str):
        super().__init__(file_ast, line_number, name)

    def get_type(self, no_link=False):
        return BLOCK_TYPE_ID

    def get_desc(self):
        return "BLOCK"

    def get_children(self, public_only=False):
        return copy.copy(self.children)

    def req_named_end(self):
        return True
