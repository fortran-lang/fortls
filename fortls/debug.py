from __future__ import annotations

import logging
import os
import pprint
import sys

import json5

from .helper_functions import only_dirs, resolve_globs
from .jsonrpc import JSONRPC2Connection, ReadWriter, path_from_uri
from .langserver import LangServer
from .parsers.internal.parser import FortranFile, preprocess_file


class DebugError(Exception):
    """Base class for debug CLI."""


class ParameterError(DebugError):
    """Exception raised for errors in the parameters."""


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
                separator()


def debug_rootpath(args, server):
    if not os.path.isdir(args.debug_rootpath):
        raise DebugError("'debug_rootpath' not specified for debug request")
    print('\nTesting "initialize" request:')
    print(f'  Root = "{args.debug_rootpath}"')
    server.serve_initialize({"params": {"rootPath": args.debug_rootpath}})

    separator()
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
    def lsp_request():
        server.serve_onSave({"params": {"textDocument": {"uri": args.debug_filepath}}})
        results, _ = server.get_diagnostics(args.debug_filepath)
        return results

    def format_results(results, _):
        sev_map = ["ERROR", "WARNING", "INFO"]
        if len(results) == 0:
            print("No errors or warnings")
        else:
            print("Reported Diagnostics:")
        for diag in results:
            sline = diag["range"]["start"]["line"]
            message = diag["message"]
            sev = sev_map[diag["severity"] - 1]
            print(f'  {sline:5d}:{sev}  "{message}"')

    debug_generic(
        args,
        "textDocument/publishDiagnostics",
        lsp_request,
        format_results,
        loc_needed=False,
    )


def debug_symbols(args, server):
    def lsp_request():
        server.serve_onSave({"params": {"textDocument": {"uri": args.debug_filepath}}})
        return server.serve_document_symbols(
            {"params": {"textDocument": {"uri": args.debug_filepath}}}
        )

    def format_results(results, _):
        for symbol in results:
            sline = symbol["location"]["range"]["start"]["line"]
            parent = "null"
            if "containerName" in symbol:
                parent = symbol["containerName"]
            print(
                f"  line {sline:5d}  symbol -> "
                f"{symbol['kind']:3d}:{symbol['name']:30} parent = {parent}"
            )

    debug_generic(
        args,
        "textDocument/documentSymbol",
        lsp_request,
        format_results,
        loc_needed=False,
    )


def debug_workspace_symbols(args, server):
    def lsp_request():
        if args.debug_rootpath is None:
            raise DebugError("'debug_rootpath' not specified for debug request")
        return server.serve_workspace_symbol(
            {"params": {"query": args.debug_workspace_symbols}}
        )

    def format_results(results, args):
        for symbol in results:
            path = path_from_uri(symbol["location"]["uri"])
            sline = symbol["location"]["range"]["start"]["line"]
            parent = "null"
            if "containerName" in symbol:
                parent = symbol["containerName"]
            print(
                f"  {parent}::{sline}  symbol -> {symbol['name']:30} parent = "
                f"{os.path.relpath(path, args.debug_rootpath)}"
            )

    debug_generic(
        args,
        "workspace/symbol",
        lsp_request,
        format_results,
        loc_needed=False,
    )


def debug_completion(args, server):
    def lsp_request():
        server.serve_onSave({"params": {"textDocument": {"uri": args.debug_filepath}}})
        return server.serve_autocomplete(
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

    def format_results(results, _):
        for obj in results:
            print(f"    {obj['kind']}: {obj['label']} -> {obj['detail']}")

    debug_generic(args, "textDocument/completion", lsp_request, format_results)


def debug_hover(args, server):
    def lsp_request():
        server.serve_onSave({"params": {"textDocument": {"uri": args.debug_filepath}}})
        return server.serve_hover(
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

    def format_results(results, _):
        contents = results["contents"]
        if isinstance(contents, dict):
            print(contents["value"])
        else:
            print(contents)

    debug_generic(args, "textDocument/hover", lsp_request, format_results)


def debug_signature(args, server):
    def lsp_request():
        server.serve_onSave({"params": {"textDocument": {"uri": args.debug_filepath}}})
        return server.serve_signature(
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

    def format_results(results, _):
        active_param = results.get("activeParameter", 0)
        print(f"    Active param = {active_param}")
        active_signature = results.get("activeSignature", 0)
        print(f"    Active sig   = {active_signature}")
        for i, signature in enumerate(results["signatures"]):
            print(f"    {signature['label']}")
            for j, obj in enumerate(signature["parameters"]):
                active_mark = " "
                if (i == active_signature) and (j == active_param):
                    active_mark = "*"
                arg_desc = obj.get("documentation")
                if arg_desc is not None:
                    print(f"{active_mark}     {arg_desc} :: {obj['label']}")
                else:
                    print(f"{active_mark}     {obj['label']}")

    debug_generic(args, "textDocument/signatureHelp", lsp_request, format_results)


def debug_definition(args, server):
    def lsp_request():
        server.serve_onSave({"params": {"textDocument": {"uri": args.debug_filepath}}})
        return server.serve_definition(
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

    def format_results(results, _):
        print(f'    URI  = "{results["uri"]}"')
        print(f'    Line = {results["range"]["start"]["line"] + 1}')
        print(f'    Char = {results["range"]["start"]["character"] + 1}')

    debug_generic(args, "textDocument/definition", lsp_request, format_results)


def debug_references(args, server):
    def lsp_request():
        server.serve_onSave({"params": {"textDocument": {"uri": args.debug_filepath}}})
        return server.serve_references(
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

    def format_results(results, _):
        for result in results:
            print(
                f"  {result['uri']}  ({result['range']['start']['line'] + 1}"
                f", {result['range']['start']['character'] + 1})"
            )

    debug_generic(args, "textDocument/references", lsp_request, format_results)


def debug_implementation(args, server):
    def lsp_request():
        server.serve_onSave({"params": {"textDocument": {"uri": args.debug_filepath}}})
        return server.serve_implementation(
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

    def format_results(results, _):
        print(f'    URI  = "{results["uri"]}"')
        print(f'    Line = {results["range"]["start"]["line"] + 1}')
        print(f'    Char = {results["range"]["start"]["character"] + 1}')

    debug_generic(args, "textDocument/implementation", lsp_request, format_results)


def debug_rename(args, server):
    def lsp_request():
        server.serve_onSave({"params": {"textDocument": {"uri": args.debug_filepath}}})
        return server.serve_rename(
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

    def format_results(results, _):
        for uri, changes in results["changes"].items():
            path = path_from_uri(uri)
            file_obj = server.workspace.get(path)
            if file_obj is not None:
                file_contents = file_obj.contents_split
                process_file_changes(path, changes, file_contents)
            else:
                print(f'Unknown file: "{path}"')

    debug_generic(args, "textDocument/rename", lsp_request, format_results)


def debug_actions(args, server):
    def lsp_request():
        server.serve_onSave({"params": {"textDocument": {"uri": args.debug_filepath}}})
        return server.serve_codeActions(
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

    def process_results(results, _):
        pp = pprint.PrettyPrinter(indent=2, width=120)
        for result in results:
            print(f"Kind = '{result['kind']}', Title = '{result['title']}'")
            for edit_uri, edit_change in result["edit"]["changes"].items():
                print(f"\nChange: URI = '{edit_uri}'")
                pp.pprint(edit_change)
            print()

    debug_generic(args, "textDocument/getActions", lsp_request, process_results)


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


def debug_parser(args):
    """Debug the parser of the Language Server
    Triggered by `--debug_parser` option.

    Parameters
    ----------
    args : Namespace
        The arguments parsed from the `ArgumentParser`
    """

    print("\nTesting parser")
    separator()

    ensure_file_accessible(args.debug_filepath)
    pp_suffixes, pp_defs, include_dirs = read_config(args.debug_rootpath, args.config)

    print(f'  File = "{args.debug_filepath}"')
    file_obj = FortranFile(args.debug_filepath, pp_suffixes)
    err_str, _ = file_obj.load_from_disk()
    if err_str:
        raise DebugError(f"Reading file failed: {err_str}")
    print(f"  Detected format: {'fixed' if file_obj.fixed else 'free'}")
    print("\n" + "=" * 80 + "\nParser Output\n" + "=" * 80 + "\n")
    file_ast = file_obj.parse(debug=True, pp_defs=pp_defs, include_dirs=include_dirs)
    print("\n" + "=" * 80 + "\nObject Tree\n" + "=" * 80 + "\n")
    for obj in file_ast.get_scopes():
        print(f"{obj.get_type()}: {obj.FQSN}")
        print_children(obj)
    print("\n" + "=" * 80 + "\nExportable Objects\n" + "=" * 80 + "\n")
    for _, obj in file_ast.global_dict.items():
        print(f"{obj.get_type()}: {obj.FQSN}")
    separator()


def debug_preprocessor(args):
    """Debug the preprocessor of the Language Server
    Triggered by `--debug_preprocessor` option.

    Parameters
    ----------
    args : Namespace
        The arguments parsed from the `ArgumentParser`
    """

    def sep_lvl2(heading: str):
        print("\n" + "=" * 75 + f"\n{heading}\n" + "=" * 75)

    print("\nTesting preprocessor")
    separator()

    logging.basicConfig(level=logging.DEBUG, stream=sys.stdout, format="%(message)s")

    file = args.debug_filepath
    ensure_file_accessible(file)
    with open(file, encoding="utf-8") as f:
        lines = f.readlines()

    root = args.debug_rootpath if args.debug_rootpath else os.path.dirname(file)
    _, pp_defs, include_dirs = read_config(root, args.config)

    sep_lvl2("Preprocessor Pass:")
    output, skips, defines, defs = preprocess_file(
        lines, file, pp_defs, include_dirs, debug=True
    )

    sep_lvl2("Preprocessor Skipped Lines:")
    for line in skips:
        print(f"  {line}")

    sep_lvl2("Preprocessor Macros:")
    for key, value in defs.items():
        print(f"  {key} = {value}")

    sep_lvl2("Preprocessor Defines (#define):")
    for line in defines:
        print(f"  {line}")

    sep_lvl2("Preprocessor Final Output:")
    for line in output:
        print(rf"  {line.rstrip()}")

    separator()


def ensure_file_accessible(filepath: str):
    """Ensure the file exists and is accessible, raising an error if not."""
    if not os.path.isfile(filepath):
        raise DebugError(f"File '{filepath}' does not exist or is not accessible")
    print(f'  File = "{filepath}"')


def check_request_params(args, loc_needed=True):
    ensure_file_accessible(args.debug_filepath)
    if loc_needed:
        if args.debug_line is None:
            raise ParameterError("'debug_line' not specified for debug request")
        print(f"  Line = {args.debug_line}")
        if args.debug_char is None:
            raise ParameterError("'debug_char' not specified for debug request")
        print(f"  Char = {args.debug_char}\n")


def locate_config(root: str, input_config: str) -> str | None:
    default_conf_files = [input_config, ".fortlsrc", ".fortls.json5", ".fortls"]
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


def read_config(root: str | None, input_config: str):
    pp_suffixes = None
    pp_defs = {}
    include_dirs = set()
    if root is None:
        return pp_suffixes, pp_defs, include_dirs

    # Check for config files
    config_path = locate_config(root, input_config)
    print(f"  Config file = {config_path}")
    if config_path is None or not os.path.isfile(config_path):
        return pp_suffixes, pp_defs, include_dirs

    try:
        with open(config_path, encoding="utf-8") as fhandle:
            config_dict = json5.load(fhandle)
            pp_suffixes = config_dict.get("pp_suffixes", None)
            pp_defs = config_dict.get("pp_defs", {})
            for path in config_dict.get("include_dirs", set()):
                include_dirs.update(only_dirs(resolve_globs(path, root)))

            if isinstance(pp_defs, list):
                pp_defs = {key: "" for key in pp_defs}
    except ValueError as e:
        print(f"Error {e} while parsing '{config_path}' settings file")

    return pp_suffixes, pp_defs, include_dirs


def debug_generic(args, test_label, lsp_request, format_results, loc_needed=True):
    print(f'\nTesting "{test_label}" request:')
    check_request_params(args, loc_needed)
    results = lsp_request()
    separator()
    print_results(results, format_results, args)


def print_results(results, format_results, args):
    """Helper function to print results based on detail level requested."""
    if results is None:
        print("    No result found!")
        return

    if args.debug_full_result:
        print(json5.dumps(results, indent=2))
        return

    format_results(results, args)


def print_children(obj, indent=""):
    for child in obj.get_children():
        print(f"  {indent}{child.get_type()}: {child.FQSN}")
        print_children(child, indent + "  ")


def separator():
    print("=" * 80)
