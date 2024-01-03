from __future__ import annotations

from typing import TYPE_CHECKING

from fortls.constants import WHERE_TYPE_ID

from .block import Block

if TYPE_CHECKING:
    from .ast import FortranAST


class Where(Block):
    def __init__(self, file_ast: FortranAST, line_number: int, name: str):
        super().__init__(file_ast, line_number, name)

    def get_type(self, no_link=False):
        return WHERE_TYPE_ID

    def get_desc(self):
        return "WHERE"
