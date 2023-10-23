from __future__ import annotations

import copy
from typing import TYPE_CHECKING

from fortls.constants import (
    BLOCK_TYPE_ID,
    CLASS_TYPE_ID,
    KEYWORD_ID_DICT,
    SUBROUTINE_TYPE_ID,
)
from fortls.helper_functions import fortran_md, get_keywords, get_placeholders

from .diagnostics import Diagnostic
from .scope import Scope

if TYPE_CHECKING:
    from .ast import FortranAST
    from .function import Function


class Subroutine(Scope):
    def __init__(
        self,
        file_ast: FortranAST,
        line_number: int,
        name: str,
        args: str = "",
        mod_flag: bool = False,
        keywords: list = None,
    ):
        super().__init__(file_ast, line_number, name, keywords)
        self.args: str = args.replace(" ", "")
        self.args_snip: str = self.args
        self.arg_objs: list = []
        self.in_children: list = []
        self.missing_args: list = []
        self.mod_scope: bool = mod_flag
        self.link_obj: Subroutine | Function | None = None

    def is_mod_scope(self):
        return self.mod_scope

    def is_callable(self):
        return True

    def copy_interface(self, copy_source: Subroutine) -> list[str]:
        # Copy arguments
        self.args = copy_source.args
        self.args_snip = copy_source.args_snip
        self.arg_objs = copy_source.arg_objs
        # Get current fields
        child_names = [child.name.lower() for child in self.children]
        # Import arg_objs from copy object
        self.in_children = []
        for child in copy_source.arg_objs:
            if child is None:
                continue
            if child.name.lower() not in child_names:
                self.in_children.append(child)
        return child_names

    def get_children(self, public_only=False):
        tmp_list = copy.copy(self.children)
        tmp_list.extend(self.in_children)
        return tmp_list

    def resolve_arg_link(self, obj_tree):
        if (self.args == "") or (len(self.in_children) > 0):
            return
        arg_list = self.args.replace(" ", "").split(",")
        arg_list_lower = self.args.lower().replace(" ", "").split(",")
        self.arg_objs = [None] * len(arg_list)
        # check_objs = copy.copy(self.children)
        # for child in self.children:
        #     if child.is_external_int():
        #         check_objs += child.get_children()
        self.missing_args = []
        for child in self.children:
            ind = -1
            for i, arg in enumerate(arg_list_lower):
                if arg == child.name.lower():
                    ind = i
                    break
                # If an argument is part of an interface block go through the
                # block's children i.e. functions and subroutines to see if one matches
                elif child.name.lower().startswith("#gen_int"):
                    for sub_child in child.children:
                        if arg == sub_child.name:
                            self.arg_objs[i] = sub_child
                            break

            if ind < 0:
                if child.keywords.count(KEYWORD_ID_DICT["intent"]) > 0:
                    self.missing_args.append(child)
            else:
                self.arg_objs[ind] = child
                if child.is_optional():
                    arg_list[ind] = f"{arg_list[ind]}={arg_list[ind]}"
        self.args_snip = ",".join(arg_list)

    def resolve_link(self, obj_tree):
        self.resolve_arg_link(obj_tree)

    def require_link(self):
        return True

    def get_type(self, no_link=False):
        return SUBROUTINE_TYPE_ID

    def get_snippet(self, name_replace=None, drop_arg=-1):
        arg_list = self.args_snip.split(",")
        if (drop_arg >= 0) and (drop_arg < len(arg_list)):
            del arg_list[drop_arg]
        arg_snip = None
        if len(arg_list) > 0:
            arg_str, arg_snip = get_placeholders(arg_list)
        else:
            arg_str = "()"
        name = name_replace if name_replace is not None else self.name
        snippet = name + arg_snip if arg_snip is not None else None
        return name + arg_str, snippet

    def get_desc(self):
        return "SUBROUTINE"

    def get_hover(self, long=False, drop_arg=-1):
        sub_sig, _ = self.get_snippet(drop_arg=drop_arg)
        keyword_list = get_keywords(self.keywords)
        keyword_list.append(f"{self.get_desc()} ")
        hover_array = [" ".join(keyword_list) + sub_sig]
        hover_array, docs = self.get_docs_full(hover_array, long, drop_arg)
        return "\n ".join(hover_array), "   \n".join(docs)

    def get_hover_md(self, long=False, drop_arg=-1):
        return fortran_md(*self.get_hover(long, drop_arg))

    def get_docs_full(
        self, hover_array: list[str], long=False, drop_arg=-1
    ) -> tuple[list[str], list[str]]:
        """Construct the full documentation with the code signature and the
        documentation string + the documentation of any arguments.

        Parameters
        ----------
        hover_array : list[str]
            The list of strings to append the documentation to.
        long : bool, optional
            Whether or not to fetch the docs of the arguments, by default False
        drop_arg : int, optional
            Whether or not to drop certain arguments from the results, by default -1

        Returns
        -------
        tuple[list[str], list[str]]
            Tuple containing the Fortran signature that should be in code blocks
            and the documentation string that should be in normal Markdown.
        """
        doc_strs: list[str] = []
        doc_str = self.get_documentation()
        if doc_str is not None:
            doc_strs.append(doc_str)
        if long:
            has_args = True
            for i, arg_obj in enumerate(self.arg_objs):
                if arg_obj is None or i == drop_arg:
                    continue
                arg, doc_str = arg_obj.get_hover()
                hover_array.append(arg)
                if doc_str:  # If doc_str is not None or ""
                    if has_args:
                        doc_strs.append("\n**Parameters:**  ")
                        has_args = False
                    # stripping prevents multiple \n characters from the parser
                    doc_strs.append(f"`{arg_obj.name}` {doc_str}".strip())
        return hover_array, doc_strs

    def get_signature(self, drop_arg=-1):
        arg_sigs = []
        arg_list = self.args.split(",")
        for i, arg_obj in enumerate(self.arg_objs):
            if i == drop_arg:
                continue
            if arg_obj is None:
                arg_sigs.append({"label": arg_list[i]})
            else:
                if arg_obj.is_optional():
                    label = f"{arg_obj.name.lower()}={arg_obj.name.lower()}"
                else:
                    label = arg_obj.name.lower()
                msg = arg_obj.get_hover_md()
                # Create MarkupContent object
                msg = {"kind": "markdown", "value": msg}
                arg_sigs.append({"label": label, "documentation": msg})
        call_sig, _ = self.get_snippet()
        return call_sig, self.get_documentation(), arg_sigs

    # TODO: fix this
    def get_interface_array(
        self, keywords: list[str], signature: str, drop_arg=-1, change_strings=None
    ):
        interface_array = [" ".join(keywords) + signature]
        for i, arg_obj in enumerate(self.arg_objs):
            if arg_obj is None:
                return None
            arg_doc, docs = arg_obj.get_hover()
            if i == drop_arg:
                i0 = arg_doc.lower().find(change_strings[0].lower())
                if i0 >= 0:
                    i1 = i0 + len(change_strings[0])
                    arg_doc = arg_doc[:i0] + change_strings[1] + arg_doc[i1:]
            interface_array.append(f"{arg_doc} :: {arg_obj.name}")
        return interface_array

    def get_interface(self, name_replace=None, drop_arg=-1, change_strings=None):
        sub_sig, _ = self.get_snippet(name_replace=name_replace)
        keyword_list = get_keywords(self.keywords)
        keyword_list.append("SUBROUTINE ")
        interface_array = self.get_interface_array(
            keyword_list, sub_sig, drop_arg, change_strings
        )
        name = name_replace if name_replace is not None else self.name
        interface_array.append(f"END SUBROUTINE {name}")
        return "\n".join(interface_array)

    def check_valid_parent(self):
        if self.parent is not None:
            parent_type = self.parent.get_type()
            if (parent_type == CLASS_TYPE_ID) or (parent_type >= BLOCK_TYPE_ID):
                return False
        return True

    def get_diagnostics(self):
        errors = []
        for missing_obj in self.missing_args:
            new_diag = Diagnostic(
                missing_obj.sline - 1,
                f'Variable "{missing_obj.name}" with INTENT keyword not found in'
                " argument list",
                severity=1,
                find_word=missing_obj.name,
            )
            errors.append(new_diag)
        implicit_flag = self.get_implicit()
        if (implicit_flag is None) or implicit_flag:
            return errors
        arg_list = self.args.replace(" ", "").split(",")
        for i, arg_obj in enumerate(self.arg_objs):
            if arg_obj is None:
                arg_name = arg_list[i].strip()
                new_diag = Diagnostic(
                    self.sline - 1,
                    f'No matching declaration found for argument "{arg_name}"',
                    severity=1,
                    find_word=arg_name,
                )
                errors.append(new_diag)
        return errors
