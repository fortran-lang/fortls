from __future__ import annotations

import re
from dataclasses import dataclass
from typing import TYPE_CHECKING

from fortls.constants import ASSOC_TYPE_ID
from fortls.helper_functions import get_var_stack

from .block import Block
from .utilities import climb_type_tree, find_in_scope
from .variable import Variable

if TYPE_CHECKING:
    from .ast import FortranAST


@dataclass
class AssociateMap:
    var: Variable
    bind_name: str
    link_name: str


class Associate(Block):
    def __init__(self, file_ast: FortranAST, line_number: int, name: str):
        super().__init__(file_ast, line_number, name)
        self.links: list[AssociateMap] = []  # holds the info to associate variables

    def get_type(self, no_link=False):
        return ASSOC_TYPE_ID

    def get_desc(self):
        return "ASSOCIATE"

    def create_binding_variable(
        self, file_ast: FortranAST, line_number: int, bind_name: str, link_name: str
    ) -> Variable:
        """Create a new variable to be linked upon resolution to the real variable
        that contains the information of the mapping from the parent scope to the
        ASSOCIATE block scope.

        Parameters
        ----------
        file_ast : fortran_ast
            AST file
        line_number : int
            Line number
        bind_name : str
            Name of the ASSOCIATE block variable
        link_name : str
            Name of the parent scope variable

        Returns
        -------
        fortran_var
            Variable object holding the ASSOCIATE block variable, pending resolution
        """
        new_var = Variable(file_ast, line_number, bind_name, "UNKNOWN", [])
        self.links.append(AssociateMap(new_var, bind_name, link_name))
        return new_var

    def resolve_link(self, obj_tree):
        # Loop through the list of the associated variables map and resolve the links
        # find the AST node that that corresponds to the variable with link_name
        for assoc in self.links:
            # TODO: extract the dimensions component from the link_name
            # re.sub(r'\(.*\)', '', link_name) removes the dimensions component
            # keywords = re.match(r'(.*)\((.*)\)', link_name).groups()
            # now pass the keywords through the dimension_parser and set the keywords
            # in the associate object. Hover should now pick the local keywords
            # over the linked_object keywords
            assoc.link_name = re.sub(r"\(.*\)", "", assoc.link_name)
            var_stack = get_var_stack(assoc.link_name)
            is_member = len(var_stack) > 1
            if is_member:
                type_scope = climb_type_tree(var_stack, self, obj_tree)
                if type_scope is None:
                    continue
                var_obj = find_in_scope(type_scope, var_stack[-1], obj_tree)
            else:
                var_obj = find_in_scope(self, assoc.link_name, obj_tree)
            if var_obj is not None:
                assoc.var.link_obj = var_obj

    def require_link(self):
        return True
