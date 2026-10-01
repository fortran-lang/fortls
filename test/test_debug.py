import subprocess
import sys

import pytest

SOURCE = "#ifdef HAVE_FOO\nmodule foo_mod\nend module foo_mod\n#endif\n"
SOURCE += "program p\nend program p\n"


def run_fortls(args: list[str], cwd) -> str:
    cmd = [sys.executable, "-m", "fortls", *args]
    return subprocess.run(cmd, cwd=cwd, capture_output=True, text=True).stdout


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
    tree = out.split("Object Tree")[1].split("Exportable Objects")[0]
    assert ("foo_mod" in tree) == found


def test_debug_parser_cli_include_dirs(pp_dir):
    (pp_dir / "main.F90").write_text('#include "defs.h"\n' + SOURCE)
    args = ["--debug_parser", "--debug_filepath", "main.F90", "--include_dirs", "inc"]
    out = run_fortls(args, pp_dir)
    tree = out.split("Object Tree")[1].split("Exportable Objects")[0]
    assert "foo_mod" in tree


def test_debug_preprocessor_cli_pp_defs(pp_dir):
    args = ["--debug_preproc", "--debug_filepath", "main.F90"]
    out = run_fortls(args + ["--pp_defs", '{"HAVE_FOO": ""}'], pp_dir)
    skipped = out.split("Preprocessor Skipped Lines:")[1].split("Preprocessor Macros")[
        0
    ]
    assert "[1, 4]" not in skipped
    out = run_fortls(args, pp_dir)
    skipped = out.split("Preprocessor Skipped Lines:")[1].split("Preprocessor Macros")[
        0
    ]
    assert "[1, 4]" in skipped
