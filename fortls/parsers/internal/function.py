from __future__ import annotations

from typing import TYPE_CHECKING

from fortls.constants import FUNCTION_TYPE_ID
from fortls.helper_functions import get_keywords

from .subroutine import Subroutine

if TYPE_CHECKING:
    from .ast import FortranAST
    from .variable import Variable


class Function(Subroutine):
    def __init__(
        self,
        file_ast: FortranAST,
        line_number: int,
        name: str,
        args: str = "",
        mod_flag: bool = False,
        keywords: list = None,
        keyword_info: dict = None,
        result_type: str = None,
        result_name: str = None,
    ):
        super().__init__(file_ast, line_number, name, args, mod_flag, keywords)
        self.args: str = args.replace(" ", "").lower()
        self.args_snip: str = self.args
        self.arg_objs: list = []
        self.in_children: list = []
        self.missing_args: list = []
        self.mod_scope: bool = mod_flag
        self.result_name: str = result_name
        self.result_type: str = result_type
        self.result_obj: Variable = None
        self.keyword_info: dict = keyword_info
        # Set the implicit result() name to be the function name
        if self.result_name is None:
            self.result_name = self.name
        # Used in Associated blocks
        if self.keyword_info is None:
            self.keyword_info = {}

    def copy_interface(self, copy_source: Function):
        # Call the parent class method
        child_names = super().copy_interface(copy_source)
        # Return specific options
        self.result_name = copy_source.result_name
        self.result_type = copy_source.result_type
        self.result_obj = copy_source.result_obj
        if (
            copy_source.result_obj is not None
            and copy_source.result_obj.name.lower() not in child_names
        ):
            self.in_children.append(copy_source.result_obj)

    def resolve_link(self, obj_tree):
        self.resolve_arg_link(obj_tree)
        result_var_lower = self.result_name.lower()
        for child in self.children:
            if child.name.lower() == result_var_lower:
                self.result_obj = child
                # Update result value and type
                self.result_name = child.name
                self.result_type = child.get_desc()

    def get_type(self, no_link=False):
        return FUNCTION_TYPE_ID

    def get_desc(self):
        token = "FUNCTION"
        return f"{self.result_type} {token}" if self.result_type else token

    def is_callable(self):
        return False

    def get_hover(self, long: bool = False, drop_arg: int = -1) -> tuple[str, str]:
        """Construct the hover message for a FUNCTION.
        Two forms are produced here the `long` i.e. the normal for hover requests

        [MODIFIERS] FUNCTION NAME([ARGS]) RESULT(RESULT_VAR)
          TYPE, [ARG_MODIFIERS] :: [ARGS]
          TYPE, [RESULT_MODIFIERS] :: RESULT_VAR

        note: intrinsic functions will display slightly different,
        `RESULT_VAR` and its `TYPE` might not always be present

        short form, used when functions are arguments in functions and subroutines:

        FUNCTION NAME([ARGS]) :: ARG_LIST_NAME

        Parameters
        ----------
        long : bool, optional
            toggle between long and short hover results, by default False
        drop_arg : int, optional
            Ignore argument at position `drop_arg` in the argument list, by default -1

        Returns
        -------
        tuple[str, bool]
            String representative of the hover message and the `long` flag used
        """
        fun_sig, _ = self.get_snippet(drop_arg=drop_arg)
        # short hover messages do not include the result()
        fun_sig += f" RESULT({self.result_name})" if long else ""
        keyword_list = get_keywords(self.keywords)
        keyword_list.append("FUNCTION")

        hover_array = [f"{' '.join(keyword_list)} {fun_sig}"]
        hover_array, docs = self.get_docs_full(hover_array, long, drop_arg)
        # Only append the return value if using long form
        if self.result_obj and long:
            # Parse the documentation from the result variable
            arg_doc, doc_str = self.result_obj.get_hover()
            if doc_str is not None:
                docs.append(f"\n**Return:**  \n`{self.result_obj.name}`{doc_str}")
            hover_array.append(arg_doc)
        # intrinsic functions, where the return type is missing but can be inferred
        elif self.result_type and long:
            # prepend type to function signature
            hover_array[0] = f"{self.result_type} {hover_array[0]}"
        return "\n ".join(hover_array), "  \n".join(docs)

    # TODO: fix this
    def get_interface(self, name_replace=None, drop_arg=-1, change_strings=None):
        fun_sig, _ = self.get_snippet(name_replace=name_replace)
        fun_sig += f" RESULT({self.result_name})"
        # XXX:
        keyword_list = []
        if self.result_type:
            keyword_list.append(self.result_type)
        keyword_list += get_keywords(self.keywords)
        keyword_list.append("FUNCTION ")

        interface_array = self.get_interface_array(
            keyword_list, fun_sig, drop_arg, change_strings
        )
        if self.result_obj is not None:
            arg_doc, docs = self.result_obj.get_hover()
            interface_array.append(f"{arg_doc} :: {self.result_obj.name}")
        name = name_replace if name_replace is not None else self.name
        interface_array.append(f"END FUNCTION {name}")
        return "\n".join(interface_array)
