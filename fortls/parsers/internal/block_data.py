from __future__ import annotations

from .module import Module


class BlockData(Module):
    """BLOCK DATA program unit. It holds COMMON blocks and DATA statements."""

    def get_desc(self) -> str:
        return "BLOCK DATA"
