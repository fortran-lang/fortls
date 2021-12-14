from __future__ import print_function

import argparse
import json
import os
import pprint
import sys
from multiprocessing import freeze_support

from ._version import __version__
from .jsonrpc import JSONRPC2Connection, ReadWriter, path_from_uri
from .langserver import LangServer, resolve_globs, only_dirs
from .parse_fortran import fortran_file, process_file


def error_exit(error_str):
    print("ERROR: {0}".format(error_str))
    sys.exit(-1)


def main():
    #
    freeze_support()
    args = parse_args()

    if args.version:
        print("{0}".format(__version__))
        sys.exit(0)
    debug_server = (
        args.debug_diagnostics
        or args.debug_symbols
        or args.debug_completion
        or args.debug_signature
        or args.debug_definition
        or args.debug_hover
        or args.debug_implementation
        or args.debug_references
        or (args.debug_rename is not None)
        or args.debug_actions
        or (args.debug_rootpath is not None)
        or (args.debug_workspace_symbols is not None)
    )
    #
    settings = set_settings(args)
    #
    if args.debug_parser:
        debug_server_parser(args)

    elif debug_server:
        debug_server_general(args, settings)

    else:
        stdin, stdout = _binary_stdio()
        LangServer(
            conn=JSONRPC2Connection(ReadWriter(stdin, stdout)),
            debug_log=args.debug_log,
            settings=settings,
        ).run()


def parse_args():
    """Parses the command line aguments to the Language Server

    Returns
    -------
    Namespace
        command line arguments
    """
    parser = argparse.ArgumentParser()
    parser.description = "FORTRAN Language Server ({0})".format(__version__)
    parser.add_argument(
        "--version", action="store_true", help="Print server version number and exit"
    )
    parser.add_argument(
        "--nthreads",
        type=int,
        default=4,
        help="Number of threads to use during workspace initialization (default: 4)",
    )
    parser.add_argument(
        "--notify_init",
        action="store_true",
        help="Send notification message when workspace initialization is complete",
    )
    parser.add_argument(
        "--symbol_skip_mem",
        action="store_true",
        help="Do not include type members in document symbol results",
    )
    parser.add_argument(
        "--incremental_sync",
        "--incrmental_sync",
        action="store_true",
        help="Use incremental document synchronization (beta)",
    )
    parser.add_argument(
        "--autocomplete_no_prefix",
        action="store_true",
        help="Do not filter autocomplete results by variable prefix",
    )
    parser.add_argument(
        "--autocomplete_no_snippets",
        action="store_true",
        help="Do not use snippets with place holders in autocomplete results",
    )
    parser.add_argument(
        "--autocomplete_name_only",
        action="store_true",
        help="Complete only the name of procedures and not the parameters",
    )
    parser.add_argument(
        "--lowercase_intrinsics",
        action="store_true",
        help="Use lowercase for intrinsics and keywords in autocomplete requests",
    )
    parser.add_argument(
        "--use_signature_help",
        action="store_true",
        help="Use signature help instead of subroutine/function snippets",
    )
    parser.add_argument(
        "--variable_hover",
        action="store_true",
        help=(
            "Show hover information for variables (default: subroutines/functions only)"
        ),
    )
    parser.add_argument(
        "--hover_signature",
        action="store_true",
        help=(
            "Show signature information in hover for argument (also enables"
            " '--variable_hover')"
        ),
    )
    parser.add_argument(
        "--hover_language",
        type=str,
        default=None,
        help=(
            "Language used for responses to hover requests (for editor syntax"
            " highlighting)"
        ),
    )
    parser.add_argument(
        "--preserve_keyword_order",
        action="store_true",
        help=(
            "Display variable keywords information in original order (default: sort to"
            " consistent ordering)"
        ),
    )
    parser.add_argument(
        "--enable_code_actions",
        action="store_true",
        help="Enable experimental code actions (default: false)",
    )
    parser.add_argument(
        "--max_line_length",
        type=int,
        default=-1,
        help="Maximum line length (default: none)",
    )
    parser.add_argument(
        "--max_comment_line_length",
        type=int,
        default=-1,
        help="Maximum comment line length (default: none)",
    )
    parser.add_argument(
        "--disable_diagnostics", action="store_true", help="Disable diagnostics"
    )
    parser.add_argument(
        "--debug_log",
        action="store_true",
        help="Generate debug log in project root folder",
    )
    group = parser.add_argument_group("DEBUG", "Options for debugging language server")
    group.add_argument(
        "--debug_parser",
        action="store_true",
        help="Test source code parser on specified file",
    )
    group.add_argument(
        "--debug_diagnostics",
        action="store_true",
        help="Test diagnostic notifications for specified file",
    )
    group.add_argument(
        "--debug_symbols",
        action="store_true",
        help="Test symbol request for specified file",
    )
    group.add_argument(
        "--debug_workspace_symbols", type=str, help="Test workspace/symbol request"
    )
    group.add_argument(
        "--debug_completion",
        action="store_true",
        help="Test completion request for specified file and position",
    )
    group.add_argument(
        "--debug_signature",
        action="store_true",
        help="Test signatureHelp request for specified file and position",
    )
    group.add_argument(
        "--debug_definition",
        action="store_true",
        help="Test definition request for specified file and position",
    )
    group.add_argument(
        "--debug_hover",
        action="store_true",
        help="Test hover request for specified file and position",
    )
    group.add_argument(
        "--debug_implementation",
        action="store_true",
        help="Test implementation request for specified file and position",
    )
    group.add_argument(
        "--debug_references",
        action="store_true",
        help="Test references request for specified file and position",
    )
    group.add_argument(
        "--debug_rename",
        type=str,
        help="Test rename request for specified file and position",
    )
    group.add_argument(
        "--debug_actions",
        action="store_true",
        help="Test codeAction request for specified file and position",
    )
    group.add_argument(
        "--debug_filepath", type=str, help="File path for language server tests"
    )
    group.add_argument(
        "--debug_rootpath", type=str, help="Root path for language server tests"
    )
    group.add_argument(
        "--debug_line",
        type=int,
        help="Line position for language server tests (1-indexed)",
    )
    group.add_argument(
        "--debug_char",
        type=int,
        help="Character position for language server tests (1-indexed)",
    )
    group.add_argument(
        "--debug_full_result",
        action="store_true",
        help="Print full result object instead of condensed version",
    )
    return parser.parse_args()


def set_settings(args):
    """Sets the settings dictionary from the command line arguments

    Parameters
    ----------
    args : Namespace
        command line arguments

    Returns
    -------
    dict[str, Any]
        settings for the Language Server
    """
    settings = {
        "nthreads": args.nthreads,
        "notify_init": args.notify_init,
        "symbol_include_mem": (not args.symbol_skip_mem),
        "sync_type": 2 if args.incremental_sync else 1,
        "autocomplete_no_prefix": args.autocomplete_no_prefix,
        "autocomplete_no_snippets": args.autocomplete_no_snippets,
        "autocomplete_name_only": args.autocomplete_name_only,
        "lowercase_intrinsics": args.lowercase_intrinsics,
        "use_signature_help": args.use_signature_help,
        "variable_hover": (args.variable_hover or args.hover_signature),
        "hover_signature": args.hover_signature,
        "sort_keywords": (not args.preserve_keyword_order),
        "enable_code_actions": (args.enable_code_actions or args.debug_actions),
        "max_line_length": args.max_line_length,
        "max_comment_line_length": args.max_comment_line_length,
        "disable_diagnostics": args.disable_diagnostics,
    }
    if args.hover_language is not None:
        settings["hover_language"] = args.hover_language

    return settings


def debug_server_general(args, settings):
    """Outputs debug information about the server.
    Triggers with any option under the DEBUG group except debug parser.
    For the parser see `debug_server_parser`

    Parameters
    ----------
    args : Namespace
        The arguments parsed from the `ArgumentParser`
    settings : dict
        Language server settings
    """
    prb, pwb = os.pipe()
    tmpin = os.fdopen(prb, "rb")
    tmpout = os.fdopen(pwb, "wb")
    s = LangServer(
        conn=JSONRPC2Connection(ReadWriter(tmpin, tmpout)),
        debug_log=args.debug_log,
        settings=settings,
    )
    #
    if args.debug_rootpath:
        dir_exists = os.path.isdir(args.debug_rootpath)
        if dir_exists is False:
            error_exit(
                "Specified 'debug_rootpath' does not exist or is not a directory"
            )
        print('\nTesting "initialize" request:')
        print('  Root = "{0}"'.format(args.debug_rootpath))
        s.serve_initialize({"params": {"rootPath": args.debug_rootpath}})
        if len(s.post_messages) == 0:
            print("  Successful!")
        else:
            print("  Successful with errors:")
            for message in s.post_messages:
                print("    {0}".format(message[1]))
        # Print module directories
        print("\n  Source directories:")
        for source_dir in s.source_dirs:
            print("    {0}".format(source_dir))
    #
    if args.debug_diagnostics:
        print('\nTesting "textDocument/publishDiagnostics" notification:')
        check_request_params(args, loc_needed=False)
        s.serve_onSave({"params": {"textDocument": {"uri": args.debug_filepath}}})
        diag_results, _ = s.get_diagnostics(args.debug_filepath)
        if diag_results is not None:
            if args.debug_full_result:
                print(json.dumps(diag_results, indent=2))
            else:
                sev_map = ["ERROR", "WARNING", "INFO"]
                if len(diag_results) == 0:
                    print("\nNo errors or warnings")
                else:
                    print("\nReported errors or warnings:")
                for diag in diag_results:
                    sline = diag["range"]["start"]["line"]
                    message = diag["message"]
                    sev = sev_map[diag["severity"] - 1]
                    print('  {0:5d}:{1}  "{2}"'.format(sline, sev, message))
    #
    if args.debug_symbols:
        print('\nTesting "textDocument/documentSymbol" request:')
        check_request_params(args, loc_needed=False)
        s.serve_onSave({"params": {"textDocument": {"uri": args.debug_filepath}}})
        symbol_results = s.serve_document_symbols(
            {"params": {"textDocument": {"uri": args.debug_filepath}}}
        )
        if args.debug_full_result:
            print(json.dumps(symbol_results, indent=2))
        else:
            for symbol in symbol_results:
                sline = symbol["location"]["range"]["start"]["line"]
                if "containerName" in symbol:
                    parent = symbol["containerName"]
                else:
                    parent = "null"
                print(
                    "  line {2:5d}  symbol -> {1:3d}:{0:30} parent = {3}".format(
                        symbol["name"], symbol["kind"], sline, parent
                    )
                )
    #
    if args.debug_workspace_symbols is not None:
        print('\nTesting "workspace/symbol" request:')
        if args.debug_rootpath is None:
            error_exit("'debug_rootpath' not specified for debug request")
        symbol_results = s.serve_workspace_symbol(
            {"params": {"query": args.debug_workspace_symbols}}
        )
        if args.debug_full_result:
            print(json.dumps(symbol_results, indent=2))
        else:
            for symbol in symbol_results:
                path = path_from_uri(symbol["location"]["uri"])
                sline = symbol["location"]["range"]["start"]["line"]
                if "containerName" in symbol:
                    parent = symbol["containerName"]
                else:
                    parent = "null"
                print(
                    "  {2}::{3:d}  symbol -> {1:3d}:{0:30} parent = {4}".format(
                        symbol["name"],
                        symbol["kind"],
                        os.path.relpath(path, args.debug_rootpath),
                        sline,
                        parent,
                    )
                )
    #
    if args.debug_completion:
        print('\nTesting "textDocument/completion" request:')
        check_request_params(args)
        s.serve_onSave({"params": {"textDocument": {"uri": args.debug_filepath}}})
        completion_results = s.serve_autocomplete(
            {
                "params": {
                    "textDocument": {"uri": args.debug_filepath},
                    "position": {
                        "line": args.debug_line - 1,
                        "character": args.debug_char - 1,
                    },
                }
            }
        )
        if completion_results is None:
            print("  No results!")
        else:
            print("  Results:")
            if args.debug_full_result:
                print(json.dumps(completion_results, indent=2))
            else:
                for obj in completion_results:
                    print(
                        "    {0}: {1} -> {2}".format(
                            obj["kind"], obj["label"], obj["detail"]
                        )
                    )
    #
    if args.debug_signature:
        print('\nTesting "textDocument/signatureHelp" request:')
        check_request_params(args)
        s.serve_onSave({"params": {"textDocument": {"uri": args.debug_filepath}}})
        signature_results = s.serve_signature(
            {
                "params": {
                    "textDocument": {"uri": args.debug_filepath},
                    "position": {
                        "line": args.debug_line - 1,
                        "character": args.debug_char - 1,
                    },
                }
            }
        )
        if signature_results is None:
            print("  No Results!")
        else:
            print("  Results:")
            if args.debug_full_result:
                print(json.dumps(signature_results, indent=2))
            else:
                active_param = signature_results.get("activeParameter", 0)
                print("    Active param = {0}".format(active_param))
                active_signature = signature_results.get("activeSignature", 0)
                print("    Active sig   = {0}".format(active_signature))
                for i, signature in enumerate(signature_results["signatures"]):
                    print("    {0}".format(signature["label"]))
                    for j, obj in enumerate(signature["parameters"]):
                        if (i == active_signature) and (j == active_param):
                            active_mark = "*"
                        else:
                            active_mark = " "
                        arg_desc = obj.get("documentation")
                        if arg_desc is not None:
                            print(
                                "{2}     {0} :: {1}".format(
                                    arg_desc, obj["label"], active_mark
                                )
                            )
                        else:
                            print("{1}     {0}".format(obj["label"], active_mark))
    #
    if args.debug_definition or args.debug_implementation:
        if args.debug_definition:
            print('\nTesting "textDocument/definition" request:')
        elif args.debug_implementation:
            print('\nTesting "textDocument/implementation" request:')
        check_request_params(args)
        s.serve_onSave({"params": {"textDocument": {"uri": args.debug_filepath}}})
        if args.debug_definition:
            definition_results = s.serve_definition(
                {
                    "params": {
                        "textDocument": {"uri": args.debug_filepath},
                        "position": {
                            "line": args.debug_line - 1,
                            "character": args.debug_char - 1,
                        },
                    }
                }
            )
        elif args.debug_implementation:
            definition_results = s.serve_implementation(
                {
                    "params": {
                        "textDocument": {"uri": args.debug_filepath},
                        "position": {
                            "line": args.debug_line - 1,
                            "character": args.debug_char - 1,
                        },
                    }
                }
            )
        print("  Result:")
        if definition_results is None:
            print("    No result found!")
        else:
            if args.debug_full_result:
                print(json.dumps(definition_results, indent=2))
            else:
                print('    URI  = "{0}"'.format(definition_results["uri"]))
                print(
                    "    Line = {0}".format(
                        definition_results["range"]["start"]["line"] + 1
                    )
                )
                print(
                    "    Char = {0}".format(
                        definition_results["range"]["start"]["character"] + 1
                    )
                )
    #
    if args.debug_hover:
        print('\nTesting "textDocument/hover" request:')
        check_request_params(args)
        s.serve_onSave({"params": {"textDocument": {"uri": args.debug_filepath}}})
        hover_results = s.serve_hover(
            {
                "params": {
                    "textDocument": {"uri": args.debug_filepath},
                    "position": {
                        "line": args.debug_line - 1,
                        "character": args.debug_char - 1,
                    },
                }
            }
        )
        print("  Result:")
        if hover_results is None:
            print("    No result found!")
        else:
            if args.debug_full_result:
                print(json.dumps(hover_results, indent=2))
            else:
                contents = hover_results["contents"]
                print("=======")
                if isinstance(contents, dict):
                    print(contents["value"])
                else:
                    print(contents)
                print("=======")
    #
    if args.debug_references:
        print('\nTesting "textDocument/references" request:')
        check_request_params(args)
        s.serve_onSave({"params": {"textDocument": {"uri": args.debug_filepath}}})
        ref_results = s.serve_references(
            {
                "params": {
                    "textDocument": {"uri": args.debug_filepath},
                    "position": {
                        "line": args.debug_line - 1,
                        "character": args.debug_char - 1,
                    },
                }
            }
        )
        print("  Result:")
        if ref_results is None:
            print("    No result found!")
        else:
            if args.debug_full_result:
                print(json.dumps(ref_results, indent=2))
            else:
                print("=======")
                for result in ref_results:
                    print(
                        "  {0}  ({1}, {2})".format(
                            result["uri"],
                            result["range"]["start"]["line"] + 1,
                            result["range"]["start"]["character"] + 1,
                        )
                    )
                print("=======")
    #
    if args.debug_rename is not None:
        print('\nTesting "textDocument/rename" request:')
        check_request_params(args)
        s.serve_onSave({"params": {"textDocument": {"uri": args.debug_filepath}}})
        ref_results = s.serve_rename(
            {
                "params": {
                    "textDocument": {"uri": args.debug_filepath},
                    "position": {
                        "line": args.debug_line - 1,
                        "character": args.debug_char - 1,
                    },
                    "newName": args.debug_rename,
                }
            }
        )
        print("  Result:")
        if ref_results is None:
            print("    No changes found!")
        else:
            if args.debug_full_result:
                print(json.dumps(ref_results, indent=2))
            else:
                print("=======")
                for uri, result in ref_results["changes"].items():
                    path = path_from_uri(uri)
                    print('File: "{0}"'.format(path))
                    file_obj = s.workspace.get(path)
                    if file_obj is not None:
                        file_contents = file_obj.contents_split
                        for change in result:
                            start_line = change["range"]["start"]["line"]
                            end_line = change["range"]["end"]["line"]
                            start_col = change["range"]["start"]["character"]
                            end_col = change["range"]["end"]["character"]
                            print("  {0}, {1}".format(start_line + 1, end_line + 1))
                            new_contents = []
                            for i in range(start_line, end_line + 1):
                                line = file_contents[i]
                                print("  - {0}".format(line))
                                if i == start_line:
                                    new_contents.append(
                                        line[:start_col] + change["newText"]
                                    )
                                if i == end_line:
                                    new_contents[-1] += line[end_col:]
                            for line in new_contents:
                                print("  + {0}".format(line))
                            print()
                    else:
                        print('Unknown file: "{0}"'.format(path))
                print("=======")
    #
    if args.debug_actions:

        pp = pprint.PrettyPrinter(indent=2, width=120)
        print('\nTesting "textDocument/getActions" request:')
        check_request_params(args)
        s.serve_onSave({"params": {"textDocument": {"uri": args.debug_filepath}}})
        action_results = s.serve_codeActions(
            {
                "params": {
                    "textDocument": {"uri": args.debug_filepath},
                    "range": {
                        "start": {
                            "line": args.debug_line - 1,
                            "character": args.debug_char - 1,
                        },
                        "end": {
                            "line": args.debug_line - 1,
                            "character": args.debug_char - 1,
                        },
                    },
                }
            }
        )
        if args.debug_full_result:
            print(json.dumps(action_results, indent=2))
        else:
            for result in action_results:
                print(
                    "Kind = '{0}', Title = '{1}'".format(
                        result["kind"], result["title"]
                    )
                )
                for editUri, editChange in result["edit"]["changes"].items():
                    print("\nChange: URI = '{0}'".format(editUri))
                    pp.pprint(editChange)
                print()
    tmpout.close()
    tmpin.close()


def debug_server_parser(args):
    """Debug the parser of the Language Server
    Triggered by `--debug_parser` option.

    Parameters
    ----------
    args : Namespace
        The arguments parsed from the `ArgumentParser`
    """
    if args.debug_filepath is None:
        error_exit("'debug_filepath' not specified for parsing test")
    file_exists = os.path.isfile(args.debug_filepath)
    if file_exists is False:
        error_exit("Specified 'debug_filepath' does not exist")
    # Get preprocessor definitions from config file
    pp_suffixes = None
    pp_defs = {}
    include_dirs = []
    if args.debug_rootpath:
        config_path = os.path.join(args.debug_rootpath, ".fortls")
        config_exists = os.path.isfile(config_path)
        if config_exists:
            try:
                with open(config_path, "r") as fhandle:
                    config_dict = json.load(fhandle)
                    pp_suffixes = config_dict.get("pp_suffixes", None)
                    pp_defs = config_dict.get("pp_defs", {})
                    include_dirs = []
                    for path in config_dict.get("include_dirs", []):
                        include_dirs.extend(
                            only_dirs(resolve_globs(path, args.debug_rootpath))
                        )

                    if isinstance(pp_defs, list):
                        pp_defs = {key: "" for key in pp_defs}
            except:
                print("Error while parsing '.fortls' settings file")
    #
    print("\nTesting parser")
    print('  File = "{0}"'.format(args.debug_filepath))
    file_obj = fortran_file(args.debug_filepath, pp_suffixes)
    err_str = file_obj.load_from_disk()
    if err_str is not None:
        error_exit("Reading file failed: {0}".format(err_str))
    print("  Detected format: {0}".format("fixed" if file_obj.fixed else "free"))
    print("\n=========\nParser Output\n=========\n")
    _, file_ext = os.path.splitext(os.path.basename(args.debug_filepath))
    preproc_file = False
    if pp_suffixes is not None:
        preproc_file = file_ext in pp_suffixes
    else:
        preproc_file = file_ext == file_ext.upper()
    if preproc_file:
        file_ast = process_file(
            file_obj, True, debug=True, pp_defs=pp_defs, include_dirs=include_dirs
        )
    else:
        file_ast = process_file(file_obj, True, debug=True)
    print("\n=========\nObject Tree\n=========\n")
    for obj in file_ast.get_scopes():
        print("{0}: {1}".format(obj.get_type(), obj.FQSN))
        print_children(obj)
    print("\n=========\nExportable Objects\n=========\n")
    for _, obj in file_ast.global_dict.items():
        print("{0}: {1}".format(obj.get_type(), obj.FQSN))


def check_request_params(args, loc_needed=True):
    if args.debug_filepath is None:
        error_exit("'debug_filepath' not specified for debug request")
    file_exists = os.path.isfile(args.debug_filepath)
    if file_exists is False:
        error_exit("Specified 'debug_filepath' does not exist")
    print('  File = "{0}"'.format(args.debug_filepath))
    if loc_needed:
        if args.debug_line is None:
            error_exit("'debug_line' not specified for debug request")
        print("  Line = {0}".format(args.debug_line))
        if args.debug_char is None:
            error_exit("'debug_char' not specified for debug request")
        print("  Char = {0}\n".format(args.debug_char))


def print_children(obj, indent=""):
    for child in obj.get_children():
        print("  {0}{1}: {2}".format(indent, child.get_type(), child.FQSN))
        print_children(child, indent + "  ")


def _binary_stdio():
    """Construct binary stdio streams (not text mode).
    This seems to be different for Window/Unix Python2/3, so going by:
        https://stackoverflow.com/questions/2850893/reading-binary-data-from-stdin
    """
    PY3K = sys.version_info >= (3, 0)

    if PY3K:
        stdin, stdout = sys.stdin.buffer, sys.stdout.buffer
    else:
        # Python 2 on Windows opens sys.stdin in text mode, and
        # binary data that read from it becomes corrupted on \r\n
        if sys.platform == "win32":
            # set sys.stdin to binary mode
            import msvcrt

            msvcrt.setmode(sys.stdin.fileno(), os.O_BINARY)
            msvcrt.setmode(sys.stdout.fileno(), os.O_BINARY)
        stdin, stdout = sys.stdin, sys.stdout

    return stdin, stdout
