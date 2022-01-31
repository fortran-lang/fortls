from __future__ import annotations

import shlex
import subprocess
import sys
from io import StringIO
from pathlib import Path

root_dir = Path(__file__).parent.parent.resolve()
sys.path.insert(0, root_dir)

from fortls.jsonrpc import (  # noqa: E402, F401
    path_to_uri,
    read_rpc_messages,
    write_rpc_notification,
    write_rpc_request,
)

test_dir = root_dir / "test" / "test_source"


def run_request(request, fortls_args: list[str] = None):
    command = [
        sys.executable,
        str(root_dir / "fortls.py"),
        "--incremental_sync",
    ]
    if fortls_args:
        # Input args might not be sanitised, fix that
        for i in fortls_args:
            command.extend(shlex.split(i, posix=False))

    pid = subprocess.Popen(
        command,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    results = pid.communicate(input=request.encode())
    tmp_file = StringIO(results[0].decode())
    results = read_rpc_messages(tmp_file)
    parsed_results = []
    for result in results:
        try:
            parsed_results.append(result["result"])
        except KeyError:
            try:
                # Present in `method`s
                parsed_results.append(result["params"])
            except:
                raise RuntimeError(
                    "Only 'result' and 'params' keys have been implemented for testing."
                    " Please add the new key."
                )
        except:
            raise RuntimeError(
                "Unexpected error encountered trying to extract server results"
            )
    errcode = pid.poll()
    return errcode, parsed_results
