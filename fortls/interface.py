from __future__ import annotations

import argparse
import json
import sys

from fortls.version import __version__


class SetAction(argparse.Action):
    def __call__(self, parser, namespace, values, option_string=None):
        setattr(namespace, self.dest, set(values))


def cli(name: str = "fortls") -> argparse.ArgumentParser:
    """Parses the command line arguments to the Language Server

    Returns
    -------
    argparse.ArgumentParser
        command line arguments
    """

    parser = argparse.ArgumentParser(
        description="fortls - Fortran Language Server",
        prog=name,
        usage="fortls [options] [debug options]",
        formatter_class=lambda prog: argparse.HelpFormatter(prog, max_help_position=60),
        epilog=(
            "All options starting with '--' can also be set in a configuration file, by"
            " default named '.fortlsrc', '.fortls.json' or '.fortls'"
            " (other names/paths can specified via -c or"
            " --config). For more details see our documentation:"
            " https://fortls.fortran-lang.org/options.html#available-options"
        ),
    )

    # General options ----------------------------------------------------------
    parser.add_argument(
        "-v",
        "--version",
        action="version",
        version=__version__,
        help="Print server version number and exit",
    )
    parser.add_argument(
        "-c",
        "--config",
        type=str,
        default=".fortlsrc",
        help=(
            "Configuration options file (default file name: %(default)s, other"
            " default supported names: .fortls.json, .fortls)"
        ),
    )
    parser.add_argument(
        "-n",
        "--nthreads",
        type=int,
        default=4,
        metavar="INTEGER",
        help=(
            "Number of threads to use during workspace initialization (default:"
            " %(default)s)"
        ),
    )
    parser.add_argument(
        "--notify_init",
        action="store_true",
        help="Send notification message when workspace initialization is complete",
    )
    parser.add_argument(
        "--incremental_sync",
        action="store_true",
        help="Use incremental document synchronization (beta)",
    )
    parser.add_argument(
        "--recursion_limit",
        type=int,
        default=1000,
        metavar="INTEGER",
        help="Set the maximum recursion depth for the parser (default: %(default)s)",
    )
    parser.add_argument(
        "--sort_keywords",
        action="store_true",
        help=(
            "Display variable keywords information, function/subroutine definitions,"
            " etc. in a consistent (sorted) manner default: no sorting, display code"
            " as is)"
        ),
    )
    parser.add_argument(
        "--disable_autoupdate",
        action="store_true",
        help=(
            "fortls automatically checks PyPi for newer version and installs them."
            "Use this option to disable the autoupdate feature."
        ),
    )
    # XXX: Deprecated, argument not attached to anything. Remove
    parser.add_argument(
        "--preserve_keyword_order",
        action="store_true",
        help="DEPRECATED, this is now the default. To sort use sort_keywords",
    )
    parser.add_argument(
        "--debug_log",
        action="store_true",
        help="Generate debug log in project root folder",
    )
    parser.add_argument(
        "--debug_help", action="help", help="Display options for debugging fortls"
    )

    # File parsing options -----------------------------------------------------
    group = parser.add_argument_group("Sources file parsing options")
    group.add_argument(
        "--source_dirs",
        type=str,
        nargs="*",
        default=set(),
        action=SetAction,
        metavar="DIRS",
        help="Folders containing source files (default: %(default)s)",
    )
    group.add_argument(
        "--incl_suffixes",
        type=str,
        nargs="*",
        default=set(),
        action=SetAction,
        metavar="SUFFIXES",
        help=(
            "Consider additional file extensions to the default (default: "
            ".F, .F77, .F90, .F95, .F03, .F08, .FOR, .FPP (lower & upper casing))"
        ),
    )
    group.add_argument(
        "--excl_suffixes",
        type=str,
        nargs="*",
        default=set(),
        action=SetAction,
        metavar="SUFFIXES",
        help="Source file extensions to be excluded (default: %(default)s)",
    )
    group.add_argument(
        "--excl_paths",
        type=str,
        nargs="*",
        default=set(),
        action=SetAction,
        metavar="DIRS",
        help="Folders to exclude from parsing",
    )

    # Autocomplete options -----------------------------------------------------
    group = parser.add_argument_group("Autocomplete options")
    group.add_argument(
        "--autocomplete_no_prefix",
        action="store_true",
        help="Do not filter autocomplete results by variable prefix",
    )
    group.add_argument(
        "--autocomplete_no_snippets",
        action="store_true",
        help="Do not use snippets with place holders in autocomplete results",
    )
    group.add_argument(
        "--autocomplete_name_only",
        action="store_true",
        help="Complete only the name of procedures and not the parameters",
    )
    group.add_argument(
        "--lowercase_intrinsics",
        action="store_true",
        help="Use lowercase for intrinsics and keywords in autocomplete requests",
    )
    group.add_argument(
        "--use_signature_help",
        action="store_true",
        help=(
            "Use signature help instead of subroutine/function snippets. This"
            " effectively sets --autocomplete_no_snippets"
        ),
    )

    # Hover options ------------------------------------------------------------
    group = parser.add_argument_group("Hover options")
    group.add_argument(
        "--variable_hover",
        action="store_true",
        help=(
            "DEPRECATED: This option is always on. Show hover information for variables"
        ),
    )
    group.add_argument(
        "--hover_signature",
        action="store_true",
        help="Show signature information in hover for arguments ",
    )
    group.add_argument(
        "--hover_language",
        type=str,
        default="fortran90",
        help=(
            "Language used for responses to hover requests a VSCode language id"
            " (default: %(default)s)"
        ),
    )

    # Diagnostic options -------------------------------------------------------
    group = parser.add_argument_group("Diagnostic options (error swigles)")
    group.add_argument(
        "--max_line_length",
        type=int,
        default=-1,
        metavar="INTEGER",
        help="Maximum line length (default: %(default)s)",
    )
    group.add_argument(
        "--max_comment_line_length",
        type=int,
        default=-1,
        metavar="INTEGER",
        help="Maximum comment line length (default: %(default)s)",
    )
    group.add_argument(
        "--disable_diagnostics", action="store_true", help="Disable diagnostics"
    )

    # Preprocessor options -----------------------------------------------------
    group = parser.add_argument_group("Preprocessor options")
    group.add_argument(
        "--pp_suffixes",
        type=str,
        default=set(),
        nargs="*",
        metavar="SUFFIXES",
        help=(
            "File extensions to be parsed ONLY for preprocessor commands "
            "(default: all uppercase source file suffixes)"
        ),
    )
    group.add_argument(
        "--include_dirs",
        # "--pp_include_dirs",  # TODO: make main
        type=str,
        nargs="*",
        default=set(),
        action=SetAction,
        metavar="DIRS",
        help="Folders containing preprocessor files with extensions PP_SUFFIXES.",
    )
    group.add_argument(
        "--pp_defs",
        type=json.loads,
        default={},
        metavar="JSON",
        help=(
            "A dictionary with additional preprocessor definitions. "
            "Preprocessor definitions are normally included via INCLUDE_DIRS"
        ),
    )

    # Symbols options ----------------------------------------------------------
    group = parser.add_argument_group("Symbols options")
    group.add_argument(
        "--symbol_skip_mem",
        action="store_true",
        help="Do not include type members in document symbol results",
    )

    # Code Actions options -----------------------------------------------------
    group = parser.add_argument_group("CodeActions options [limited]")
    group.add_argument(
        "--enable_code_actions",
        action="store_true",
        help="Enable experimental code actions (default: false)",
    )

    # Debug
    # By default debug arguments are hidden
    _debug_commandline_args(parser)

    return parser


# TODO: make this return a parser
def _debug_commandline_args(parser: argparse.ArgumentParser) -> None:
    """Parse the debug arguments if any are present.
    if none are present the arguments are suppressed in the help menu

    Parameters
    ----------
    parser : argparse.ArgumentParser
        an argument parser

    Returns
    -------
    None
        Operates and updates the parser
    """

    # Only show debug options if an argument starting with --debug_ was input.
    # if suppressed the option will be hidden from the help menu.
    HIDE_DEBUG = True
    if any("--debug_" in arg for arg in sys.argv):
        HIDE_DEBUG = False

    def hide_opt(help: str) -> str:
        if not HIDE_DEBUG:
            return help
        else:
            return argparse.SUPPRESS

    group = parser.add_argument_group(
        hide_opt("DEBUG"), hide_opt("Options for debugging language server")
    )
    group.add_argument(
        "--debug_filepath",
        type=str,
        help=hide_opt("File path for language server tests"),
    )
    group.add_argument(
        "--debug_rootpath",
        type=str,
        help=hide_opt("Root path for language server tests"),
    )
    group.add_argument(
        "--debug_parser",
        action="store_true",
        help=hide_opt("Test source code parser on specified file"),
    )
    group.add_argument(
        "--debug_preproc",
        action="store_true",
        help=hide_opt("Test source code preprocessor parser on specified file"),
    )
    group.add_argument(
        "--debug_hover",
        action="store_true",
        help=hide_opt(
            "Test `textDocument/hover` request for specified file and position"
        ),
    )
    group.add_argument(
        "--debug_rename",
        type=str,
        metavar="RENAME_STRING",
        help=hide_opt(
            "Test `textDocument/rename` request for specified file and position"
        ),
    )
    group.add_argument(
        "--debug_actions",
        action="store_true",
        help=hide_opt(
            "Test `textDocument/codeAction` request for specified file and position"
        ),
    )
    group.add_argument(
        "--debug_symbols",
        action="store_true",
        help=hide_opt("Test `textDocument/documentSymbol` request for specified file"),
    )
    group.add_argument(
        "--debug_completion",
        action="store_true",
        help=hide_opt(
            "Test `textDocument/completion` request for specified file and position"
        ),
    )
    group.add_argument(
        "--debug_signature",
        action="store_true",
        help=hide_opt(
            "Test `textDocument/signatureHelp` request for specified file and position"
        ),
    )
    group.add_argument(
        "--debug_definition",
        action="store_true",
        help=hide_opt(
            "Test `textDocument/definition` request for specified file and position"
        ),
    )
    group.add_argument(
        "--debug_references",
        action="store_true",
        help=hide_opt(
            "Test `textDocument/references` request for specified file and position"
        ),
    )
    group.add_argument(
        "--debug_diagnostics",
        action="store_true",
        help=hide_opt("Test diagnostic notifications for specified file"),
    )
    group.add_argument(
        "--debug_implementation",
        action="store_true",
        help=hide_opt(
            "Test `textDocument/implementation` request for specified file and position"
        ),
    )
    group.add_argument(
        "--debug_workspace_symbols",
        type=str,
        metavar="QUERY_STRING",
        help=hide_opt("Test `workspace/symbol` request"),
    )
    group.add_argument(
        "--debug_line",
        type=int,
        metavar="INTEGER",
        help=hide_opt("Line position for language server tests (1-indexed)"),
    )
    group.add_argument(
        "--debug_char",
        type=int,
        metavar="INTEGER",
        help=hide_opt("Character position for language server tests (1-indexed)"),
    )
    group.add_argument(
        "--debug_full_result",
        action="store_true",
        help=hide_opt("Print full result object instead of condensed version"),
    )
