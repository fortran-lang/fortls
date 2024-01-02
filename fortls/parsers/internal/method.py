from __future__ import annotations

from typing import TYPE_CHECKING

from fortls.constants import CLASS_TYPE_ID, KEYWORD_ID_DICT, METH_TYPE_ID
from fortls.helper_functions import get_paren_substring

from .utilities import find_in_scope
from .variable import Variable

if TYPE_CHECKING:
    from .ast import FortranAST


class Method(Variable):  # i.e. TypeBound procedure
    def __init__(
        self,
        file_ast: FortranAST,
        line_number: int,
        name: str,
        var_desc: str,
        keywords: list,
        keyword_info: dict,
        proc_ptr: str = "",  # procedure pointer e.g. `foo` in `procedure(foo)`
        link_obj=None,
    ):
        super().__init__(
            file_ast,
            line_number,
            name,
            var_desc,
            keywords,
            keyword_info,
            kind=proc_ptr,
            link_obj=link_obj,
        )
        self.drop_arg: int = -1
        self.pass_name: str = keyword_info.get("pass")
        if link_obj is None:
            self.link_name = get_paren_substring(self.get_desc(True).lower())

    def set_parent(self, parent_obj):
        self.parent = parent_obj
        if self.parent.get_type() == CLASS_TYPE_ID:
            if self.keywords.count(KEYWORD_ID_DICT["nopass"]) == 0:
                self.drop_arg = 0
            if (
                (self.parent.contains_start is not None)
                and (self.sline > self.parent.contains_start)
                and (self.link_name is None)
            ):
                self.link_name = self.name.lower()

    def get_snippet(self, name_replace=None, drop_arg=-1):
        if self.link_obj is not None:
            name = self.name if name_replace is None else name_replace
            return self.link_obj.get_snippet(name, self.drop_arg)
        return None, None

    def get_type(self, no_link=False):
        if (not no_link) and (self.link_obj is not None):
            return self.link_obj.get_type()
        # Generic
        return METH_TYPE_ID

    def get_documentation(self):
        if (self.link_obj is not None) and (self.doc_str is None):
            return self.link_obj.get_documentation()
        return self.doc_str

    def get_hover(self, long=False, drop_arg=-1) -> tuple[str, str]:
        docs = self.get_documentation()
        # Long hover message
        if self.link_obj is None:
            sub_sig, _ = self.get_snippet()
            hover_str = f"{self.get_desc()} {sub_sig}"
        else:
            link_msg, link_docs = self.link_obj.get_hover(
                long=True, drop_arg=self.drop_arg
            )
            # Replace the name of the linked object with the name of this object
            hover_str = link_msg.replace(self.link_obj.name, self.name, 1)
            if isinstance(link_docs, str):
                # Get just the docstring of the link, if any, no args
                link_doc_top = self.link_obj.get_documentation()
                # Replace the linked objects topmost documentation with the
                # documentation of the procedure pointer if one is present
                if link_doc_top is not None:
                    docs = link_docs.replace(link_doc_top, docs, 1)
                # If no top docstring is present at the linked object but there
                # are docstrings for the arguments, add them to the end of the
                # documentation for this object
                elif link_docs:
                    if docs is None:
                        docs = ""
                    docs += "  \n" + link_docs
        return hover_str, docs

    def get_signature(self, drop_arg=-1):
        if self.link_obj is not None:
            call_sig, _ = self.get_snippet()
            _, _, arg_sigs = self.link_obj.get_signature(self.drop_arg)
            return call_sig, self.get_documentation(), arg_sigs
        return None, None, None

    def get_interface(self, name_replace=None, drop_arg=-1, change_strings=None):
        if self.link_obj is not None:
            return self.link_obj.get_interface(
                name_replace, self.drop_arg, change_strings
            )
        return None

    def resolve_link(self, obj_tree):
        if self.link_name is None:
            return
        if self.parent is not None:
            if self.parent.get_type() == CLASS_TYPE_ID:
                link_obj = find_in_scope(self.parent.parent, self.link_name, obj_tree)
            else:
                link_obj = find_in_scope(self.parent, self.link_name, obj_tree)
            if link_obj is not None:
                self.link_obj = link_obj
                if self.pass_name is not None:
                    self.pass_name = self.pass_name.lower()
                    for i, arg in enumerate(link_obj.args_snip.split(",")):
                        if arg.lower() == self.pass_name:
                            self.drop_arg = i
                            break

    def is_callable(self):
        return True

    def check_definition(self, obj_tree, known_types=None, interface=False):
        if known_types is None:
            known_types = {}
        return None, known_types
