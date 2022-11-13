from __future__ import annotations

import json
import os

from fortls.helper_functions import fortran_md, map_keywords
from fortls.objects import (
    FortranAST,
    FortranObj,
    Function,
    Module,
    Subroutine,
    Type,
    Variable,
)

none_ast = FortranAST()
lowercase_intrinsics = False


def set_lowercase_intrinsics():
    global lowercase_intrinsics
    lowercase_intrinsics = True


class Intrinsic(FortranObj):
    def __init__(
        self,
        name: str,
        type: int,
        doc_str: str | None = None,
        args: str = "",
        parent=None,
    ):
        self.name: str = name
        self.type: int = type
        self.doc_str: str = doc_str
        self.args: str = args.replace(" ", "")
        self.parent = parent
        self.file_ast: FortranAST = none_ast
        if lowercase_intrinsics:
            self.name = self.name.lower()
            self.args = self.args.lower()

    def get_type(self):
        return self.type

    def get_desc(self):
        if self.type == 2:
            return "SUBROUTINE"
        elif self.type == 14:
            return "KEYWORD"
        elif self.type == 15:
            return "STATEMENT"
        else:
            return "INTRINSIC"

    def get_snippet(self, name_replace=None, drop_arg=-1):
        if self.args == "":
            if self.type >= 14:
                return None, None
            arg_str = "()"
            arg_snip = None
        else:
            arg_list = self.args.split(",")
            arg_str, arg_snip = self.get_placeholders(arg_list)
        name = self.name
        if name_replace is not None:
            name = name_replace
        snippet = None
        if arg_snip is not None:
            snippet = name + arg_snip
        return name + arg_str, snippet

    def get_signature(self):
        arg_sigs = []
        for arg in self.args.split(","):
            arg_sigs.append({"label": arg})
        call_sig, _ = self.get_snippet()
        return call_sig, self.doc_str, arg_sigs

    def get_hover(self, long=False):
        return None, self.doc_str

    def get_hover_md(self, long=False):
        msg, docs = self.get_hover(long)
        msg = msg if msg else ""
        return fortran_md(msg, docs)

    def is_callable(self):
        if self.type == 2:
            return True
        else:
            return False


def load_intrinsics():
    def create_int_object(name, json_obj, type):
        args = json_obj.get("args", "")
        doc_str = json_obj.get("doc")
        if lowercase_intrinsics:
            name = name.lower()
            args = args.lower()
        return Intrinsic(name, type, doc_str=doc_str, args=args)

    def create_object(json_obj: dict, enc_obj=None):
        if enc_obj is not None:
            none_ast.enc_scope_name = enc_obj.FQSN
        else:
            none_ast.enc_scope_name = None
        if "mods" in json_obj:
            keywords, keyword_info = map_keywords(json_obj["mods"])
        else:
            keywords = []
            keyword_info = {}
        name = json_obj["name"]
        args = json_obj.get("args", "")
        if lowercase_intrinsics:
            name = name.lower()
            args = args.lower()
        if json_obj["type"] == 0:
            mod_tmp = Module(none_ast, 0, name)
            if "use" in json_obj:
                mod_tmp.add_use(json_obj["use"], 0)
            return mod_tmp
        elif json_obj["type"] == 1:
            return Subroutine(none_ast, 0, name, args=args)
        elif json_obj["type"] == 2:
            return Function(
                none_ast,
                0,
                name,
                args=args,
                result_type=json_obj["return"],
                keywords=keywords,
                # keyword_info=keyword_info,
            )
        elif json_obj["type"] == 3:
            return Variable(none_ast, 0, name, json_obj["desc"], keywords, keyword_info)
        elif json_obj["type"] == 4:
            return Type(none_ast, 0, name, keywords)
        else:
            raise ValueError

    def add_children(json_obj, fort_obj):
        for child in json_obj.get("children", []):
            child_obj = create_object(child, enc_obj=fort_obj)
            fort_obj.add_child(child_obj)
            add_children(child, child_obj)

    root = os.path.dirname(os.path.abspath(__file__))
    # Fortran statements taken from Intel Fortran documentation
    # (https://www.intel.com/content/www/us/en/develop/documentation/fortran-compiler-oneapi-dev-guide-and-reference/top/language-reference/a-to-z-reference)
    json_file = os.path.join(root, "statements.json")
    statements = {"var_def": [], "int_stmnts": []}
    with open(json_file, encoding="utf-8") as fid:
        intrin_file = json.load(fid)
        for key in statements:
            for name, json_obj in sorted(intrin_file[key].items()):
                statements[key].append(create_int_object(name, json_obj, 15))
    # Fortran keywords taken from Intel Fortran documentation
    # (https://www.intel.com/content/www/us/en/develop/documentation/fortran-compiler-oneapi-dev-guide-and-reference/top/language-reference/a-to-z-reference)
    json_file = os.path.join(root, "keywords.json")
    keywords = {"var_def": [], "arg": [], "type_mem": [], "vis": [], "param": []}
    with open(json_file, encoding="utf-8") as fid:
        intrin_file = json.load(fid)
        for key in keywords:
            for name, json_obj in sorted(intrin_file[key].items()):
                keywords[key].append(create_int_object(name, json_obj, 14))
    # Intrinsics procedure definitions, from gfortran
    # https://gcc.gnu.org/onlinedocs/gfortran/intrinsic-procedures.html
    json_file = os.path.join(root, "intrinsic.procedures.markdown.json")
    with open(json_file, encoding="utf-8") as fid:
        md_files = json.load(fid)
    json_file = os.path.join(root, "intrinsic.procedures.json")
    int_funs = []
    with open(json_file, encoding="utf-8") as fid:
        intrin_file = json.load(fid)
        for name, json_obj in sorted(intrin_file.items()):
            if name in md_files:
                json_obj["doc"] = md_files[name]
            int_funs.append(create_int_object(name, json_obj, json_obj["type"]))
    # Definitions taken from gfortran documentation
    # https://gcc.gnu.org/onlinedocs/gfortran/intrinsic-modules.html
    # Update OpenACC from here https://www.openacc.org/specification
    json_file = os.path.join(root, "intrinsic.modules.json")
    int_mods = []
    with open(json_file, encoding="utf-8") as fid:
        intrin_file = json.load(fid)
        for key, json_obj in intrin_file.items():
            fort_obj = create_object(json_obj)
            add_children(json_obj, fort_obj)
            int_mods.append(fort_obj)
    return statements, keywords, int_funs, int_mods


def get_intrinsic_keywords(statements, keywords, context=-1):
    if context == 0:
        return statements["int_stmnts"] + statements["var_def"] + keywords["vis"]
    elif context == 1:
        return keywords["var_def"] + keywords["vis"] + keywords["param"]
    elif context == 2:
        return keywords["var_def"] + keywords["arg"] + keywords["param"]
    elif context == 3:
        return keywords["var_def"] + keywords["type_mem"] + keywords["vis"]
    return keywords["var_def"] + keywords["param"]
