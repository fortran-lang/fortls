from __future__ import annotations

from fortls.json_templates import diagnostic_json, location_json
from fortls.jsonrpc import path_to_uri


class Diagnostic:
    def __init__(
        self, sline: int, message: str, severity: int = 1, find_word: str = None
    ):
        self.sline: int = sline
        self.message: str = message
        self.severity: int = severity
        self.find_word: str = find_word
        self.has_related: bool = False
        self.related_path = None
        self.related_line = None
        self.related_message = None

    def add_related(self, path: str, line: int, message: str):
        self.has_related = True
        self.related_path = path
        self.related_line = line
        self.related_message = message

    def build(self, file_obj):
        schar = echar = 0
        if self.find_word is not None:
            self.sline, obj_range = file_obj.find_word_in_code_line(
                self.sline, self.find_word
            )
            if obj_range.start >= 0:
                schar = obj_range.start
                echar = obj_range.end
        diag = diagnostic_json(
            self.sline, schar, self.sline, echar, self.message, self.severity
        )
        if self.has_related:
            diag["relatedInformation"] = [
                {
                    **location_json(
                        path_to_uri(self.related_path), self.related_line, 0
                    ),
                    "message": self.related_message,
                }
            ]
        return diag
