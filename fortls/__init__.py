from __future__ import annotations

import json
import os
import pprint
import sys
from multiprocessing import freeze_support

from .helper_functions import only_dirs, resolve_globs
from .interface import cli
from .jsonrpc import JSONRPC2Connection, ReadWriter, path_from_uri
from .langserver import LangServer
from .parsers.internal.parser import FortranFile
from .version import __version__

__all__ = ["__version__"]


def error_exit(error_str: str):
    print(f"ERROR: {error_str}")
    sys.exit(-1)


def main():
    freeze_support()
    args = cli(__name__).parse_args()

    if args.debug_parser:
        debug_server_parser(args)

    elif is_debug_mode(args):
        debug_lsp(args, vars(args))

    else:
        stdin, stdout = sys.stdin.buffer, sys.stdout.buffer
        LangServer(
            conn=JSONRPC2Connection(ReadWriter(stdin, stdout)),
            settings=vars(args),
        ).run()


def is_debug_mode(args):
    debug_flags = [
        "debug_diagnostics",
        "debug_symbols",
        "debug_completion",
        "debug_signature",
        "debug_definition",
        "debug_hover",
        "debug_implementation",
        "debug_references",
        "debug_rename",
        "debug_actions",
        "debug_rootpath",
        "debug_workspace_symbols",
    ]
    return any(getattr(args, flag, False) for flag in debug_flags)


def debug_lsp(args, settings):
    debug_functions = {
        "debug_rootpath": debug_rootpath,
        "debug_diagnostics": debug_diagnostics,
        "debug_symbols": debug_symbols,
        "debug_workspace_symbols": debug_workspace_symbols,
        "debug_completion": debug_completion,
        "debug_hover": debug_hover,
        "debug_signature": debug_signature,
        "debug_definition": debug_definition,
        "debug_references": debug_references,
        "debug_implementation": debug_implementation,
        "debug_rename": debug_rename,
        "debug_actions": debug_actions,
    }

    r, w = os.pipe()
    with os.fdopen(r, "rb") as buffer_in, os.fdopen(w, "wb") as buffer_out:
        server = LangServer(
            conn=JSONRPC2Connection(ReadWriter(buffer_in, buffer_out)),
            settings=settings,
        )
        for flag, function in debug_functions.items():
            if getattr(args, flag, False):
                function(args, server)


def debug_rootpath(args, server):
    if not os.path.isdir(args.debug_rootpath):
        error_exit("'debug_rootpath' not specified for debug request")
    print('\nTesting "initialize" request:')
    print(f'  Root = "{args.debug_rootpath}"')
    server.serve_initialize({"params": {"rootPath": args.debug_rootpath}})
    if len(server.post_messages) == 0:
        print("  Successful!")
    else:
        print("  Successful with errors:")
        for message in server.post_messages:
            print(f"    {message[1]}")
    print("\n  Source directories:")
    for source_dir in server.source_dirs:
        print(f"    {source_dir}")


def debug_diagnostics(args, server):
    print('\nTesting "textDocument/publishDiagnostics" notification:')
    check_request_params(args, loc_needed=False)
    server.serve_onSave({"params": {"textDocument": {"uri": args.debug_filepath}}})
    results, _ = server.get_diagnostics(args.debug_filepath)
    if results is not None:
        if args.debug_full_result:
            print(json.dumps(results, indent=2))
        else:
            sev_map = ["ERROR", "WARNING", "INFO"]
            if len(results) == 0:
                print("\nNo errors or warnings")
            else:
                print("\nReported Diagnostics:")
            for diag in results:
                sline = diag["range"]["start"]["line"]
                message = diag["message"]
                sev = sev_map[diag["severity"] - 1]
                print(f'  {sline:5d}:{sev}  "{message}"')


def debug_symbols(args, server):
    print('\nTesting "textDocument/documentSymbol" request:')
    check_request_params(args, loc_needed=False)
    server.serve_onSave({"params": {"textDocument": {"uri": args.debug_filepath}}})
    results = server.serve_document_symbols(
        {"params": {"textDocument": {"uri": args.debug_filepath}}}
    )
    if args.debug_full_result:
        print(json.dumps(results, indent=2))
    else:
        for symbol in results:
            sline = symbol["location"]["range"]["start"]["line"]
            if "containerName" in symbol:
                parent = symbol["containerName"]
            else:
                parent = "null"
            print(
                f"  line {sline:5d}  symbol -> "
                f"{symbol['kind']:3d}:{symbol['name']:30} parent = {parent}"
            )


def debug_workspace_symbols(args, server):
    print('\nTesting "workspace/symbol" request:')
    if args.debug_rootpath is None:
        error_exit("'debug_rootpath' not specified for debug request")
    results = server.serve_workspace_symbol(
        {"params": {"query": args.debug_workspace_symbols}}
    )
    if args.debug_full_result:
        print(json.dumps(results, indent=2))
    else:
        for symbol in results:
            path = path_from_uri(symbol["location"]["uri"])
            sline = symbol["location"]["range"]["start"]["line"]
            if "containerName" in symbol:
                parent = symbol["containerName"]
            else:
                parent = "null"
            print(
                f"  {parent}::{sline}  symbol -> {symbol['name']:30} parent = "
                f"{os.path.relpath(path, args.debug_rootpath)}"
            )


def debug_completion(args, server):
    print('\nTesting "textDocument/completion" request:')
    check_request_params(args)
    server.serve_onSave({"params": {"textDocument": {"uri": args.debug_filepath}}})
    results = server.serve_autocomplete(
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
    if results is None:
        print("  No results!")
    else:
        print("  Results:")
        if args.debug_full_result:
            print(json.dumps(results, indent=2))
        else:
            for obj in results:
                print(f"    {obj['kind']}: {obj['label']} -> {obj['detail']}")


def debug_hover(args, server):
    print('\nTesting "textDocument/hover" request:')
    check_request_params(args)
    server.serve_onSave({"params": {"textDocument": {"uri": args.debug_filepath}}})
    results = server.serve_hover(
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
    if results is None:
        print("    No result found!")
    else:
        if args.debug_full_result:
            print(json.dumps(results, indent=2))
        else:
            contents = results["contents"]
            print("=======")
            if isinstance(contents, dict):
                print(contents["value"])
            else:
                print(contents)
            print("=======")


def debug_signature(args, server):
    print('\nTesting "textDocument/signatureHelp" request:')
    check_request_params(args)
    server.serve_onSave({"params": {"textDocument": {"uri": args.debug_filepath}}})
    results = server.serve_signature(
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
    if results is None:
        print("  No Results!")
    else:
        print("  Results:")
        if args.debug_full_result:
            print(json.dumps(results, indent=2))
        else:
            active_param = results.get("activeParameter", 0)
            print(f"    Active param = {active_param}")
            active_signature = results.get("activeSignature", 0)
            print(f"    Active sig   = {active_signature}")
            for i, signature in enumerate(results["signatures"]):
                print(f"    {signature['label']}")
                for j, obj in enumerate(signature["parameters"]):
                    if (i == active_signature) and (j == active_param):
                        active_mark = "*"
                    else:
                        active_mark = " "
                    arg_desc = obj.get("documentation")
                    if arg_desc is not None:
                        print(f"{active_mark}     {arg_desc} :: {obj['label']}")
                    else:
                        print(f"{active_mark}     {obj['label']}")


def debug_definition(args, server):
    print('\nTesting "textDocument/definition" request:')
    check_request_params(args)
    server.serve_onSave({"params": {"textDocument": {"uri": args.debug_filepath}}})
    results = server.serve_definition(
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
    if results is None:
        print("    No result found!")
    else:
        if args.debug_full_result:
            print(json.dumps(results, indent=2))
        else:
            print(f'    URI  = "{results["uri"]}"')
            print(f'    Line = {results["range"]["start"]["line"] + 1}')
            print(f'    Char = {results["range"]["start"]["character"] + 1}')


def debug_references(args, server):
    print('\nTesting "textDocument/references" request:')
    check_request_params(args)
    server.serve_onSave({"params": {"textDocument": {"uri": args.debug_filepath}}})
    results = server.serve_references(
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
    if results is None:
        print("    No result found!")
    else:
        if args.debug_full_result:
            print(json.dumps(results, indent=2))
        else:
            print("=======")
            for result in results:
                print(
                    f"  {result['uri']}  ({result['range']['start']['line'] + 1}"
                    f", {result['range']['start']['character'] + 1})"
                )
            print("=======")


def debug_implementation(args, server):
    print('\nTesting "textDocument/implementation" request:')
    check_request_params(args)
    server.serve_onSave({"params": {"textDocument": {"uri": args.debug_filepath}}})
    results = server.serve_implementation(
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
    if results is None:
        print("    No result found!")
    else:
        if args.debug_full_result:
            print(json.dumps(results, indent=2))
        else:
            print(f'    URI  = "{results["uri"]}"')
            print(f'    Line = {results["range"]["start"]["line"] + 1}')
            print(f'    Char = {results["range"]["start"]["character"] + 1}')


def debug_rename(args, server):
    print('\nTesting "textDocument/rename" request:')
    check_request_params(args)
    server.serve_onSave({"params": {"textDocument": {"uri": args.debug_filepath}}})
    results = server.serve_rename(
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
    if results is None:
        print("    No changes found!")
    else:
        if args.debug_full_result:
            print(json.dumps(results, indent=2))
        else:
            print("=======")
            for uri, changes in results["changes"].items():
                path = path_from_uri(uri)
                file_obj = server.workspace.get(path)
                if file_obj is not None:
                    file_contents = file_obj.contents_split
                    process_file_changes(path, changes, file_contents)
                else:
                    print(f'Unknown file: "{path}"')
            print("=======")


def process_file_changes(file_path, changes, file_contents):
    print(f'File: "{file_path}"')
    for change in changes:
        start_line = change["range"]["start"]["line"]
        end_line = change["range"]["end"]["line"]
        start_col = change["range"]["start"]["character"]
        end_col = change["range"]["end"]["character"]
        print(f"  {start_line + 1}, {end_line + 1}")
        new_contents = []
        for i in range(start_line, end_line + 1):
            line = file_contents[i]
            print(f"  - {line}")
            line_content = line
            if i == start_line:
                line_content = line[:start_col] + change["newText"]
            if i == end_line:
                line_content += line[end_col:]
            new_contents.append(line_content)
        for line in new_contents:
            print(f"  + {line}")
        print()


def debug_actions(args, server):
    print('\nTesting "textDocument/getActions" request:')
    check_request_params(args)
    server.serve_onSave({"params": {"textDocument": {"uri": args.debug_filepath}}})
    results = server.serve_codeActions(
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
    print("  Result:")
    pp = pprint.PrettyPrinter(indent=2, width=120)
    if results is None:
        print("    No actions found!")
    else:
        print("=======")
        if args.debug_full_result:
            print(json.dumps(results, indent=2))
        else:
            for result in results:
                print(f"Kind = '{result['kind']}', Title = '{result['title']}'")
                for edit_uri, edit_change in result["edit"]["changes"].items():
                    print(f"\nChange: URI = '{edit_uri}'")
                    pp.pprint(edit_change)
                print()
        print("=======")


def debug_server_parser(args):
    """Debug the parser of the Language Server
    Triggered by `--debug_parser` option.

    Parameters
    ----------
    args : Namespace
        The arguments parsed from the `ArgumentParser`
    """

    def locate_config(root: str) -> str | None:
        default_conf_files = [args.config, ".fortlsrc", ".fortls.json", ".fortls"]
        present_conf_files = [
            os.path.isfile(os.path.join(root, f)) for f in default_conf_files
        ]
        if not any(present_conf_files):
            return None

        # Load the first config file found
        for f, present in zip(default_conf_files, present_conf_files):
            if not present:
                continue
            config_path = os.path.join(root, f)
            return config_path

    ensure_file_accessible(args.debug_filepath)
    # Get preprocessor definitions from config file
    pp_suffixes = None
    pp_defs = {}
    include_dirs = set()
    if args.debug_rootpath:
        # Check for config files
        config_path = locate_config(args.debug_rootpath)
        config_exists = os.path.isfile(config_path)
        if config_exists:
            try:
                with open(config_path) as fhandle:
                    config_dict = json.load(fhandle)
                    pp_suffixes = config_dict.get("pp_suffixes", None)
                    pp_defs = config_dict.get("pp_defs", {})
                    include_dirs = set()
                    for path in config_dict.get("include_dirs", set()):
                        include_dirs.update(
                            only_dirs(resolve_globs(path, args.debug_rootpath))
                        )

                    if isinstance(pp_defs, list):
                        pp_defs = {key: "" for key in pp_defs}
            except:
                print(f"Error while parsing '{args.config}' settings file")

    print("\nTesting parser")
    print('  File = "{}"'.format(args.debug_filepath))
    file_obj = FortranFile(args.debug_filepath, pp_suffixes)
    err_str, _ = file_obj.load_from_disk()
    if err_str:
        error_exit(f"Reading file failed: {err_str}")
    print(f"  Detected format: {'fixed' if file_obj.fixed else 'free'}")
    print("\n=========\nParser Output\n=========\n")
    file_ast = file_obj.parse(debug=True, pp_defs=pp_defs, include_dirs=include_dirs)
    print("\n=========\nObject Tree\n=========\n")
    for obj in file_ast.get_scopes():
        print("{}: {}".format(obj.get_type(), obj.FQSN))
        print_children(obj)
    print("\n=========\nExportable Objects\n=========\n")
    for _, obj in file_ast.global_dict.items():
        print("{}: {}".format(obj.get_type(), obj.FQSN))


def ensure_file_accessible(filepath: str):
    """Ensure the file exists and is accessible, raising an error if not."""
    if not os.path.isfile(filepath):
        error_exit(f"File '{filepath}' does not exist or is not accessible")
    print(f'  File = "{filepath}"')


def check_request_params(args, loc_needed=True):
    ensure_file_accessible(args.debug_filepath)
    if loc_needed:
        if args.debug_line is None:
            error_exit("'debug_line' not specified for debug request")
        print(f"  Line = {args.debug_line}")
        if args.debug_char is None:
            error_exit("'debug_char' not specified for debug request")
        print(f"  Char = {args.debug_char}\n")


def print_children(obj, indent=""):
    for child in obj.get_children():
        print(f"  {indent}{child.get_type()}: {child.FQSN}")
        print_children(child, indent + "  ")
