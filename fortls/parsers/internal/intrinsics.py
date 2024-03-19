from __future__ import annotations

import glob
import json
import os
from pathlib import Path

from fortls.helper_functions import fortran_md, get_placeholders, map_keywords

from .ast import FortranAST
from .base import FortranObj
from .function import Function
from .module import Module
from .subroutine import Subroutine
from .type import Type
from .use import Use
from .variable import Variable

intrinsic_ast = FortranAST()
lowercase_intrinsics = False


def set_lowercase_intrinsics():
    global lowercase_intrinsics
    lowercase_intrinsics = True


def intrinsics_case(name: str, args: str):
    return (name.lower(), args.lower()) if lowercase_intrinsics else (name, args)


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
        self.doc_str: str | None = doc_str
        self.args: str = args.replace(" ", "")
        self.parent = parent
        self.file_ast: FortranAST = intrinsic_ast
        self.name, self.args = intrinsics_case(self.name, self.args)

    def get_type(self):
        return self.type

    def get_desc(self):
        if self.type == 2:
            return "SUBROUTINE"
        elif self.type == 14:
            return "KEYWORD"
        elif self.type == 15:
            return "STATEMENT"
        return "INTRINSIC"

    def get_snippet(self, name_replace=None, drop_arg=-1):
        if self.args == "":
            if self.type >= 14:
                return None, None
            arg_str = "()"
            arg_snip = None
        else:
            arg_list = self.args.split(",")
            arg_str, arg_snip = get_placeholders(arg_list)
        name = name_replace if name_replace is not None else self.name
        snippet = name + arg_snip if arg_snip is not None else None
        return name + arg_str, snippet

    def get_signature(self):
        arg_sigs = [{"label": arg} for arg in self.args.split(",")]
        call_sig, _ = self.get_snippet()
        return call_sig, self.doc_str, arg_sigs

    def get_hover(self, long=False):
        return None, self.doc_str

    def get_hover_md(self, long=False):
        msg, docs = self.get_hover(long)
        msg = msg or ""
        return fortran_md(msg, docs)

    def is_callable(self):
        return self.type == 2


def load_intrinsics():
    def create_int_object(name: str, json_obj: dict, type: int):
        args = json_obj.get("args", "")
        doc_str = json_obj.get("doc")
        name, args = intrinsics_case(name, args)
        return Intrinsic(name, type, doc_str=doc_str, args=args)

    def create_object(json_obj: dict, enc_obj=None):
        intrinsic_ast.enc_scope_name = None
        if enc_obj is not None:
            intrinsic_ast.enc_scope_name = enc_obj.FQSN
        keywords = []
        keyword_info = {}
        if "mods" in json_obj:
            keywords, keyword_info = map_keywords(json_obj["mods"])
        name = json_obj["name"]
        args = json_obj.get("args", "")
        name, args = intrinsics_case(name, args)

        if json_obj["type"] == 0:  # module, match "type": in JSON files
            mod_tmp = Module(intrinsic_ast, 0, name)
            if "use" in json_obj:
                mod_tmp.add_use(Use(json_obj["use"], line_number=0))
            return mod_tmp
        elif json_obj["type"] == 1:  # subroutine, match "type": in JSON files
            return Subroutine(intrinsic_ast, 0, name, args=args)
        elif json_obj["type"] == 2:  # function, match "type": in JSON files
            return Function(
                intrinsic_ast,
                0,
                name,
                args=args,
                result_type=json_obj["return"],
                keywords=keywords,
            )
        elif json_obj["type"] == 3:  # variable, match "type": in JSON files
            return Variable(
                intrinsic_ast, 0, name, json_obj["desc"], keywords, keyword_info
            )
        elif json_obj["type"] == 4:  # derived type, match "type": in JSON files
            return Type(intrinsic_ast, 0, name, keywords)
        else:
            raise ValueError

    def add_children(json_obj, fort_obj):
        for child in json_obj.get("children", []):
            child_obj = create_object(child, enc_obj=fort_obj)
            fort_obj.add_child(child_obj)
            add_children(child, child_obj)

    def load_statements(root: str):
        """Load the statements from the json file.
        Fortran statements taken from Intel Fortran documentation
        (https://www.intel.com/content/www/us/en/develop/documentation/fortran-compiler-oneapi-dev-guide-and-reference/top/language-reference/a-to-z-reference)


        Parameters
        ----------
        root : str
            root location of the json file.

        Returns
        -------
        dict
            statements dictionary
        """
        json_file = os.path.join(root, "statements.json")
        statements = {"var_def": [], "int_stmnts": []}
        with open(json_file, encoding="utf-8") as fid:
            json_data = json.load(fid)
            for key in statements:
                for name, json_obj in sorted(json_data[key].items()):
                    statements[key].append(create_int_object(name, json_obj, 15))
        return statements

    def load_keywords(root: str):
        """Load the Fortran keywords from the json file.
        Fortran statements taken from Intel Fortran documentation
        (https://www.intel.com/content/www/us/en/develop/documentation/fortran-compiler-oneapi-dev-guide-and-reference/top/language-reference/a-to-z-reference)


        Parameters
        ----------
        root : str
            root location of the json file.

        Returns
        -------
        dict
            keywords dictionary
        """
        json_file = os.path.join(root, "keywords.json")
        keywords = {"var_def": [], "arg": [], "type_mem": [], "vis": [], "param": []}
        with open(json_file, encoding="utf-8") as fid:
            json_data = json.load(fid)
            for key in keywords:
                for name, json_obj in sorted(json_data[key].items()):
                    keywords[key].append(create_int_object(name, json_obj, 14))
        return keywords

    def load_intrinsic_procedures(root: str):
        """Load Intrinsics procedure definitions, from gfortran
        (https://gcc.gnu.org/onlinedocs/gfortran/intrinsic-procedures.html)

        Parameters
        ----------
        root : str
            root location of the json file.

        Returns
        -------
        dict
            intrinsic procedures dictionary
        """
        json_file = os.path.join(root, "intrinsic.procedures.markdown.json")
        with open(json_file, encoding="utf-8") as f:
            md_files = json.load(f)
        json_file = os.path.join(root, "intrinsic.procedures.json")
        intrinsic_procedures = []
        with open(json_file, encoding="utf-8") as f:
            json_data = json.load(f)
            for name, json_obj in sorted(json_data.items()):
                # Replace the plain documentation with the Markdown if available
                if name in md_files:
                    json_obj["doc"] = md_files[name]
                intrinsic_procedures.append(
                    create_int_object(name, json_obj, json_obj["type"])
                )
        return intrinsic_procedures

    def load_intrinsic_modules(root: str):
        """Load Intrinsics procedure definitions, from gfortran
        (https://gcc.gnu.org/onlinedocs/gfortran/intrinsic-modules.html)
        Update OpenACC from here https://www.openacc.org/specification

        Parameters
        ----------
        root : str
            root location of the json file.

        Returns
        -------
        dict
            intrinsic modules dictionary
        """
        json_file = os.path.join(root, "intrinsic.modules.json")
        intrinsic_modules = []
        with open(json_file, encoding="utf-8") as fid:
            intrin_file = json.load(fid)
            for key, json_obj in intrin_file.items():
                fort_obj = create_object(json_obj)
                add_children(json_obj, fort_obj)
                intrinsic_modules.append(fort_obj)
        return intrinsic_modules

    root = os.path.dirname(os.path.abspath(__file__))
    statements = load_statements(root)
    keywords = load_keywords(root)
    intrinsic_procedures = load_intrinsic_procedures(root)
    intrinsic_modules = load_intrinsic_modules(root)

    return statements, keywords, intrinsic_procedures, intrinsic_modules


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


def update_m_intrinsics():
    try:
        files = glob.glob("M_intrinsics/md/*.md")
        markdown_intrinsics = {}
        for f in sorted(files):
            key = f.replace("M_intrinsics/md/", "")
            key = key.replace(".md", "").upper()  # remove md extension
            val = Path(f).read_text()
            # remove manpage tag
            val = val.replace(f"**{key.lower()}**(3)", f"**{key.lower()}**")
            val = val.replace(f"**{key.upper()}**(3)", f"**{key.upper()}**")
            markdown_intrinsics[key] = val

        with open(
            Path(__file__).parent / "intrinsic.procedures.markdown.json", "w"
        ) as f:
            json.dump(markdown_intrinsics, f, indent=2)
            f.write("\n")  # add newline at end of file
    except Exception as e:
        print(e)


if __name__ == "__main__":
    update_m_intrinsics()
