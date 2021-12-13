import json
import os

from fortls.objects import (
    fortran_ast,
    fortran_function,
    fortran_module,
    fortran_obj,
    fortran_subroutine,
    fortran_type,
    fortran_var,
    map_keywords,
)

none_ast = fortran_ast()
lowercase_intrinsics = False


def set_lowercase_intrinsics():
    global lowercase_intrinsics
    lowercase_intrinsics = True


class fortran_intrinsic_obj(fortran_obj):
    def __init__(self, name, type, doc_str=None, args="", parent=None):
        self.name = name
        self.type = type
        self.doc_str = doc_str
        self.args = args.replace(" ", "")
        self.parent = parent
        self.file_ast = none_ast
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
            place_holders = []
            for i, arg in enumerate(arg_list):
                opt_split = arg.split("=")
                if len(opt_split) > 1:
                    place_holders.append(
                        "{1}=${{{0}:{2}}}".format(i + 1, opt_split[0], opt_split[1])
                    )
                else:
                    place_holders.append("${{{0}:{1}}}".format(i + 1, arg))
            arg_str = "({0})".format(", ".join(arg_list))
            arg_snip = "({0})".format(", ".join(place_holders))
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

    def get_documentation(self):
        return self.doc_str

    def get_hover(self, long=False):
        return self.doc_str, False

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
        return fortran_intrinsic_obj(name, type, doc_str=doc_str, args=args)

    def create_object(json_obj, enc_obj=None):
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
            mod_tmp = fortran_module(none_ast, 0, name)
            if "use" in json_obj:
                mod_tmp.add_use(json_obj["use"], 0)
            return mod_tmp
        elif json_obj["type"] == 1:
            return fortran_subroutine(none_ast, 0, name, args=args)
        elif json_obj["type"] == 2:
            return fortran_function(
                none_ast,
                0,
                name,
                args=args,
                return_type=[json_obj["return"], keywords, keyword_info],
            )
        elif json_obj["type"] == 3:
            return fortran_var(
                none_ast, 0, name, json_obj["desc"], keywords, keyword_info
            )
        elif json_obj["type"] == 4:
            return fortran_type(none_ast, 0, name, keywords)
        else:
            raise ValueError

    def add_children(json_obj, fort_obj):
        for child in json_obj.get("children", []):
            child_obj = create_object(child, enc_obj=fort_obj)
            fort_obj.add_child(child_obj)
            add_children(child, child_obj)

    # Fortran statments taken from Intel Fortran documentation
    # (https://software.intel.com/en-us/fortran-compiler-18.0-developer-guide)
    json_file = os.path.join(
        os.path.dirname(os.path.abspath(__file__)), "statements.json"
    )
    statements = {"var_def": [], "int_stmnts": []}
    with open(json_file, "r") as fid:
        intrin_file = json.load(fid)
        for key in statements:
            for name, json_obj in sorted(intrin_file[key].items()):
                statements[key].append(create_int_object(name, json_obj, 15))
    # Fortran keywords taken from Intel Fortran documentation
    # (https://software.intel.com/en-us/fortran-compiler-18.0-developer-guide)
    json_file = os.path.join(
        os.path.dirname(os.path.abspath(__file__)), "keywords.json"
    )
    keywords = {"var_def": [], "arg": [], "type_mem": [], "vis": [], "param": []}
    with open(json_file, "r") as fid:
        intrin_file = json.load(fid)
        for key in keywords:
            for name, json_obj in sorted(intrin_file[key].items()):
                keywords[key].append(create_int_object(name, json_obj, 14))
    # Definitions taken from gfortran documentation
    # (https://gcc.gnu.org/onlinedocs/gfortran/Intrinsic-Procedures.html#Intrinsic-Procedures)
    json_file = os.path.join(
        os.path.dirname(os.path.abspath(__file__)), "intrinsic_funs.json"
    )
    int_funs = []
    with open(json_file, "r") as fid:
        intrin_file = json.load(fid)
        for name, json_obj in sorted(intrin_file.items()):
            int_funs.append(create_int_object(name, json_obj, json_obj["type"]))
    # Definitions taken from gfortran documentation
    # (https://gcc.gnu.org/onlinedocs/gfortran/Intrinsic-Modules.html#Intrinsic-Modules)
    json_file = os.path.join(
        os.path.dirname(os.path.abspath(__file__)), "intrinsic_mods.json"
    )
    int_mods = []
    with open(json_file, "r") as fid:
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
