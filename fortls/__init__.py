from __future__ import annotations

import sys
from multiprocessing import freeze_support

from .debug import (
    DebugError,
    debug_lsp,
    debug_parser,
    debug_preprocessor,
    is_debug_mode,
)
from .interface import cli
from .jsonrpc import JSONRPC2Connection, ReadWriter
from .langserver import LangServer
from .version import __version__

__all__ = ["__version__"]


def main():
    freeze_support()
    args = cli(__name__).parse_args()

    try:
        if args.debug_parser:
            debug_parser(args)

        elif args.debug_preproc:
            debug_preprocessor(args)

        elif is_debug_mode(args):
            debug_lsp(args, vars(args))

        else:
            stdin, stdout = sys.stdin.buffer, sys.stdout.buffer
            LangServer(
                conn=JSONRPC2Connection(ReadWriter(stdin, stdout)),
                settings=vars(args),
            ).run()
    except DebugError as e:
        print(f"ERROR: {e}")
        sys.exit(-1)
