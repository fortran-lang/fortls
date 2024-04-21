import os
import sys
from pathlib import Path

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from fortls.interface import cli  # noqa: E402

parser = cli("fortls")


def test_command_line_general_options():
    args = parser.parse_args(
        "-c config_file.json -n 2 --notify_init --incremental_sync --sort_keywords"
        " --disable_autoupdate --debug_log".split()
    )
    assert args.config == "config_file.json"
    assert args.nthreads == 2
    assert args.notify_init
    assert args.incremental_sync
    assert args.sort_keywords
    assert args.disable_autoupdate
    assert args.debug_log


def test_command_line_file_parsing_options():
    args = parser.parse_args(
        "--source_dirs tmp ./local /usr/include/** --incl_suffixes .FF .fpc .h f20"
        " --excl_suffixes _tmp.f90 _h5hut_tests.F90 --excl_paths exclude tests".split()
    )
    assert args.source_dirs == {"tmp", "./local", "/usr/include/**"}
    assert args.incl_suffixes == {".FF", ".fpc", ".h", "f20"}
    assert args.excl_suffixes == {"_tmp.f90", "_h5hut_tests.F90"}
    assert args.excl_paths == {"exclude", "tests"}


def test_command_line_autocomplete_options():
    args = parser.parse_args(
        "--autocomplete_no_prefix --autocomplete_no_snippets --autocomplete_name_only"
        " --lowercase_intrinsics --use_signature_help".split()
    )
    assert args.autocomplete_no_prefix
    assert args.autocomplete_no_snippets
    assert args.autocomplete_name_only
    assert args.lowercase_intrinsics
    assert args.use_signature_help


def test_command_line_hover_options():
    args = parser.parse_args(
        "--hover_signature --hover_language FortranFreeForm".split()
    )
    assert args.hover_signature
    assert args.hover_language == "FortranFreeForm"


def test_command_line_diagnostic_options():
    args = parser.parse_args(
        "--max_line_length 80 --max_comment_line_length 8 --disable_diagnostics".split()
    )
    assert args.max_line_length == 80
    assert args.max_comment_line_length == 8
    assert args.disable_diagnostics


def test_command_line_preprocessor_options():
    args = parser.parse_args(
        "--pp_suffixes .h .fh --include_dirs /usr/include/** ./local/incl --pp_defs"
        ' {"HAVE_PETSC":"","HAVE_ZOLTAN":"","Mat":"type(tMat)"}'.split()
    )
    assert args.pp_suffixes == [".h", ".fh"]
    assert args.include_dirs == {"/usr/include/**", "./local/incl"}
    assert args.pp_defs == {"HAVE_PETSC": "", "HAVE_ZOLTAN": "", "Mat": "type(tMat)"}


def test_command_line_symbol_options():
    args = parser.parse_args("--symbol_skip_mem".split())
    assert args.symbol_skip_mem


def test_command_line_code_actions_options():
    args = parser.parse_args("--enable_code_actions".split())
    assert args.enable_code_actions


def unittest_server_init(conn=None):
    from fortls.langserver import LangServer

    root = (Path(__file__).parent / "test_source").resolve()
    parser = cli("fortls")
    args = parser.parse_args("-c f90_config.json".split())

    server = LangServer(conn, vars(args))
    server.root_path = root
    server._load_config_file()

    return server, root


def test_config_file_general_options():
    server, root = unittest_server_init()
    assert server.nthreads == 8
    assert server.notify_init
    assert server.incremental_sync
    assert server.sort_keywords
    assert server.disable_autoupdate
    assert server.recursion_limit == 1500


def test_config_file_dir_parsing_options():
    server, r = unittest_server_init()
    # File parsing
    assert server.source_dirs == {"pp/**", "subdir"}
    assert server.incl_suffixes == {".FF", ".fpc", ".h", "f20"}
    assert server.excl_suffixes == {"_tmp.f90", "_h5hut_tests.F90"}
    assert server.excl_paths == {"excldir", "hover/**"}


def test_config_file_autocomplete_options():
    server, root = unittest_server_init()
    # Autocomplete options
    assert server.autocomplete_no_prefix
    assert server.autocomplete_no_snippets
    assert server.autocomplete_name_only
    assert server.lowercase_intrinsics
    assert server.use_signature_help


def test_config_file_hover_options():
    server, root = unittest_server_init()
    # Hover options
    assert server.hover_signature
    assert server.hover_language == "FortranFreeForm"


def test_config_file_diagnostic_options():
    server, root = unittest_server_init()
    # Diagnostic options
    assert server.max_line_length == 80
    assert server.max_comment_line_length == 80
    assert server.disable_diagnostics


def test_config_file_preprocessor_options():
    server, root = unittest_server_init()
    # Preprocessor options
    assert server.pp_suffixes == [".h", ".fh"]
    assert server.include_dirs == {"./include/**"}
    assert server.pp_defs == {
        "HAVE_PETSC": "",
        "HAVE_ZOLTAN": "",
        "Mat": "type(tMat)",
    }


def test_config_file_symbols_options():
    server, root = unittest_server_init()
    # Symbols options
    assert server.symbol_skip_mem


def test_config_file_codeactions_options():
    server, root = unittest_server_init()
    # Code Actions options
    assert server.enable_code_actions


def test_version_update_pypi():
    from packaging import version

    from fortls.jsonrpc import JSONRPC2Connection, ReadWriter

    stdin, stdout = sys.stdin.buffer, sys.stdout.buffer
    s, root = unittest_server_init(JSONRPC2Connection(ReadWriter(stdin, stdout)))
    s.disable_autoupdate = False

    did_update = s._update_version_pypi(test=True)
    isconda = os.path.exists(os.path.join(sys.prefix, "conda-meta"))
    assert not did_update if isconda else did_update

    s.disable_autoupdate = True
    did_update = s._update_version_pypi()
    assert not did_update

    s.disable_autoupdate = False
    s._version = version.parse("999.0.0")
    did_update = s._update_version_pypi()
    assert not did_update
