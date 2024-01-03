from __future__ import annotations


# Helper classes
class Use:
    """AST node for USE statement"""

    def __init__(
        self,
        mod_name: str,
        only_list: set[str] = None,
        rename_map: dict[str, str] = None,
        line_number: int = 0,
    ):
        if only_list is None:
            only_list = set()
        if rename_map is None:
            rename_map = {}
        self.mod_name: str = mod_name.lower()
        self._line_no: int = line_number
        self.only_list: set[str] = only_list
        self.rename_map: dict[str, str] = rename_map
        if only_list:
            self.only_list: set[str] = {only.lower() for only in only_list}
        if rename_map:
            self.rename_map = {k.lower(): v.lower() for k, v in rename_map.items()}

    @property
    def line_number(self):
        return self._line_no

    @line_number.setter
    def line_number(self, line_number: int):
        self._line_no = line_number

    def rename(self, only_list: list[str] = None):
        """Rename ONLY:, statements"""
        if only_list is None:
            only_list = []
        if not only_list:
            only_list = self.only_list
        return [self.rename_map.get(only_name, only_name) for only_name in only_list]
