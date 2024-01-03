from __future__ import annotations

from fortls.constants import MODULE_TYPE_ID

from .scope import Scope


class Module(Scope):
    def get_type(self, no_link=False):
        return MODULE_TYPE_ID

    def get_desc(self):
        return "MODULE"

    def get_hover(self, long=False, drop_arg=-1) -> tuple[str, str]:
        hover = f"{self.get_desc()} {self.name}"
        doc_str = self.get_documentation()
        return hover, doc_str

    def check_valid_parent(self) -> bool:
        return self.parent is None
