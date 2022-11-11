import json
import os
import pprint
import sys
from multiprocessing import freeze_support

from .helper_functions import only_dirs, resolve_globs
from .interface import cli
from .jsonrpc import JSONRPC2Connection, ReadWriter, path_from_uri
from .langserver import LangServer
from .parse_fortran import FortranFile
from .version import __version__

__all__ = ["__version__"]


def error_exit(error_str: str):
    print(f"ERROR: {error_str}")
    sys.exit(-1)


def main():
    #
    freeze_support()
    args = cli(__name__).parse_args()

    if args.version:
        print(__version__)
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
        or args.debug_rename
        or args.debug_actions
        or args.debug_rootpath
        or args.debug_workspace_symbols
    )

    if args.debug_parser:
        debug_server_parser(args)

    elif debug_server:
        debug_server_general(args, vars(args))

    else:
        stdin, stdout = sys.stdin.buffer, sys.stdout.buffer
        LangServer(
            conn=JSONRPC2Connection(ReadWriter(stdin, stdout)),
            settings=vars(args),
        ).run()


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
        print('  Root = "{}"'.format(args.debug_rootpath))
        s.serve_initialize({"params": {"rootPath": args.debug_rootpath}})
        if len(s.post_messages) == 0:
            print("  Successful!")
        else:
            print("  Successful with errors:")
            for message in s.post_messages:
                print("    {}".format(message[1]))
        # Print module directories
        print("\n  Source directories:")
        for source_dir in s.source_dirs:
            print("    {}".format(source_dir))
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
                    print('  {:5d}:{}  "{}"'.format(sline, sev, message))
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
                        "    {}: {} -> {}".format(
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
                print("    Active param = {}".format(active_param))
                active_signature = signature_results.get("activeSignature", 0)
                print("    Active sig   = {}".format(active_signature))
                for i, signature in enumerate(signature_results["signatures"]):
                    print("    {}".format(signature["label"]))
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
                print('    URI  = "{}"'.format(definition_results["uri"]))
                print(
                    "    Line = {}".format(
                        definition_results["range"]["start"]["line"] + 1
                    )
                )
                print(
                    "    Char = {}".format(
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
                        "  {}  ({}, {})".format(
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
                    print('File: "{}"'.format(path))
                    file_obj = s.workspace.get(path)
                    if file_obj is not None:
                        file_contents = file_obj.contents_split
                        for change in result:
                            start_line = change["range"]["start"]["line"]
                            end_line = change["range"]["end"]["line"]
                            start_col = change["range"]["start"]["character"]
                            end_col = change["range"]["end"]["character"]
                            print("  {}, {}".format(start_line + 1, end_line + 1))
                            new_contents = []
                            for i in range(start_line, end_line + 1):
                                line = file_contents[i]
                                print("  - {}".format(line))
                                if i == start_line:
                                    new_contents.append(
                                        line[:start_col] + change["newText"]
                                    )
                                if i == end_line:
                                    new_contents[-1] += line[end_col:]
                            for line in new_contents:
                                print("  + {}".format(line))
                            print()
                    else:
                        print('Unknown file: "{}"'.format(path))
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
                    "Kind = '{}', Title = '{}'".format(result["kind"], result["title"])
                )
                for editUri, editChange in result["edit"]["changes"].items():
                    print("\nChange: URI = '{}'".format(editUri))
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

    if args.debug_filepath is None:
        error_exit("'debug_filepath' not specified for parsing test")
    file_exists = os.path.isfile(args.debug_filepath)
    if file_exists is False:
        error_exit("Specified 'debug_filepath' does not exist")
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


def check_request_params(args, loc_needed=True):
    if args.debug_filepath is None:
        error_exit("'debug_filepath' not specified for debug request")
    file_exists = os.path.isfile(args.debug_filepath)
    if file_exists is False:
        error_exit("Specified 'debug_filepath' does not exist")
    print('  File = "{}"'.format(args.debug_filepath))
    if loc_needed:
        if args.debug_line is None:
            error_exit("'debug_line' not specified for debug request")
        print("  Line = {}".format(args.debug_line))
        if args.debug_char is None:
            error_exit("'debug_char' not specified for debug request")
        print("  Char = {}\n".format(args.debug_char))


def print_children(obj, indent=""):
    for child in obj.get_children():
        print("  {}{}: {}".format(indent, child.get_type(), child.FQSN))
        print_children(child, indent + "  ")
