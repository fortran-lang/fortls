from __future__ import annotations

from .scope import Scope


class Include(Scope):
    def get_desc(self):
        return "INCLUDE"
