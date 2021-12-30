import os
import subprocess
import sys

try:
    from StringIO import StringIO
except ImportError:
    from io import StringIO
root_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
sys.path.insert(0, root_dir)

from fortls.jsonrpc import (  # noqa: E402, F401
    path_to_uri,
    read_rpc_messages,
    write_rpc_notification,
    write_rpc_request,
)

run_command = os.path.join(
    root_dir, "fortls.py --incremental_sync --use_signature_help"
)
test_dir = os.path.join(root_dir, "test", "test_source")


def run_request(request, fortls_args=""):
    pid = subprocess.Popen(
        run_command + fortls_args,
        shell=True,
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
