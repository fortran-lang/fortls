from __future__ import annotations

from typing import TYPE_CHECKING

from .use import Use

if TYPE_CHECKING:
    from .module import Module
    from .scope import Scope


class ImportTypes:
    DEFAULT = -1
    NONE = 0
    ALL = 1
    ONLY = 2


class Import(Use):
    """AST node for IMPORT statement"""

    def __init__(
        self,
        name: str,
        import_type: ImportTypes = ImportTypes.DEFAULT,
        only_list: set[str] = None,
        rename_map: dict[str, str] = None,
        line_number: int = 0,
    ):
        if only_list is None:
            only_list = set()
        if rename_map is None:
            rename_map = {}
        super().__init__(name, only_list, rename_map, line_number)
        self.import_type = import_type
        self._scope: Scope | Module | None = None

    @property
    def scope(self):
        """Parent scope of IMPORT statement i.e. parent of the interface"""
        return self._scope

    @scope.setter
    def scope(self, scope: Scope):
        self._scope = scope
