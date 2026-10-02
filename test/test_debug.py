import os
import subprocess
import sys

import pytest

SOURCE = "#ifdef HAVE_FOO\nmodule foo_mod\nend module foo_mod\n#endif\n"
SOURCE += "program p\nend program p\n"


def run_fortls(args: list[str], cwd) -> str:
    cmd = [sys.executable, "-m", "fortls", *args]
    # Ignore the user site-packages. test_version_update_pypi can install the PyPI
    # release there while the tests run, and that release would hide this one.
    env = {**os.environ, "PYTHONNOUSERSITE": "1"}
    return subprocess.run(cmd, cwd=cwd, env=env, capture_output=True, text=True).stdout


def section(out: str, start: str, end: str) -> str:
    return out.split(start)[1].split(end)[0]


@pytest.fixture()
def pp_dir(tmp_path):
    (tmp_path / "main.F90").write_text(SOURCE)
    (tmp_path / "inc").mkdir()
    (tmp_path / "inc" / "defs.h").write_text("#define HAVE_FOO\n")
    return tmp_path


@pytest.mark.parametrize(
    "config, extra_args, found",
    [
        (None, ["--pp_defs", '{"HAVE_FOO": ""}'], True),
        ("{}", ["--pp_defs", '{"HAVE_FOO": ""}'], True),
        ('{"pp_defs": {}}', ["--pp_defs", '{"HAVE_FOO": ""}'], False),
        (None, [], False),
    ],
)
def test_debug_parser_cli_pp_defs(pp_dir, config, extra_args, found):
    if config is not None:
        (pp_dir / ".fortls").write_text(config)
    args = ["--debug_parser", "--debug_filepath", "main.F90", "--debug_rootpath", "."]
    out = run_fortls(args + extra_args, pp_dir)
    tree = section(out, "Object Tree", "Exportable Objects")
    assert ("foo_mod" in tree) == found


@pytest.mark.parametrize("suffix_args, preprocessed", [([".f90"], True), ([], False)])
def test_debug_parser_cli_pp_suffixes(pp_dir, suffix_args, preprocessed):
    # A .f90 file is preprocessed only when its suffix is in pp_suffixes
    source = "#ifdef HAVE_FOO\nmodule foo_mod\nend module foo_mod\n#else\n"
    source += "module nofoo_mod\nend module nofoo_mod\n#endif\n"
    (pp_dir / "plain.f90").write_text(source)
    args = ["--debug_parser", "--debug_filepath", "plain.f90"]
    args += ["--pp_defs", '{"HAVE_FOO": ""}']
    if suffix_args:
        args += ["--pp_suffixes", *suffix_args]
    out = run_fortls(args, pp_dir)
    tree = section(out, "Object Tree", "Exportable Objects")
    assert "foo_mod" in tree
    assert ("nofoo_mod" not in tree) == preprocessed


def test_debug_parser_cli_include_dirs(pp_dir):
    (pp_dir / "main.F90").write_text('#include "defs.h"\n' + SOURCE)
    args = ["--debug_parser", "--debug_filepath", "main.F90", "--include_dirs", "inc"]
    out = run_fortls(args, pp_dir)
    tree = section(out, "Object Tree", "Exportable Objects")
    assert "foo_mod" in tree


def test_debug_preprocessor_cli_pp_defs(pp_dir):
    args = ["--debug_preproc", "--debug_filepath", "main.F90"]
    out = run_fortls(args + ["--pp_defs", '{"HAVE_FOO": ""}'], pp_dir)
    skipped = section(out, "Preprocessor Skipped Lines:", "Preprocessor Macros")
    assert "[1, 4]" not in skipped
    out = run_fortls(args, pp_dir)
    skipped = section(out, "Preprocessor Skipped Lines:", "Preprocessor Macros")
    assert "[1, 4]" in skipped
