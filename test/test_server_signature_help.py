from pathlib import Path

from setup_tests import run_request, test_dir, write_rpc_request


def sigh_request(uri: Path, line: int, char: int):
    return write_rpc_request(
        1,
        "textDocument/signatureHelp",
        {
            "textDocument": {"uri": str(uri)},
            "position": {"line": line, "character": char},
        },
    )


def validate_sigh(results, refs):
    assert results.get("activeParameter", -1) == refs[0]
    signatures = results.get("signatures")
    assert signatures[0].get("label") == refs[2]
    assert len(signatures[0].get("parameters")) == refs[1]


def test_subroutine_signature_help():
    """Test that the signature help is correctly resolved for all arguments and
    that the autocompletion is correct for the subroutine signature.
    """
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    file_path = test_dir / "test_prog.f08"
    string += sigh_request(file_path, 25, 18)
    string += sigh_request(file_path, 25, 20)
    string += sigh_request(file_path, 25, 22)
    string += sigh_request(file_path, 25, 27)
    string += sigh_request(file_path, 25, 29)
    errcode, results = run_request(string)
    assert errcode == 0

    sub_sig = "test_sig_Sub(arg1, arg2, opt1=opt1, opt2=opt2, opt3=opt3)"
    ref = (
        [0, 5, sub_sig],
        [1, 5, sub_sig],
        [2, 5, sub_sig],
        [3, 5, sub_sig],
        [4, 5, sub_sig],
    )
    assert len(ref) == len(results) - 1
    for i, r in enumerate(ref):
        validate_sigh(results[i + 1], r)


def test_intrinsics():
    string = write_rpc_request(
        1, "initialize", {"rootPath": str(test_dir / "signature")}
    )
    file_path = test_dir / "signature" / "nested_sigs.f90"
    string += sigh_request(file_path, 8, 77)
    errcode, results = run_request(
        string, ["--hover_signature", "--use_signature_help", "-n", "1"]
    )
    assert errcode == 0

    ref = [[0, 2, "REAL(A, KIND=kind)"]]
    assert len(ref) == len(results) - 1
    for i, r in enumerate(ref):
        validate_sigh(results[i + 1], r)


def test_subroutine_markdown():
    """Test that the signature help is correctly resolved for all arguments and
    that the autocompletion is correct for the subroutine signature, when there
    is documentation present.
    """
    string = write_rpc_request(
        1, "initialize", {"rootPath": str(test_dir / "signature")}
    )
    file_path = test_dir / "signature" / "help.f90"
    string += sigh_request(file_path, 23, 18)
    errcode, results = run_request(
        string, ["--hover_signature", "--use_signature_help", "-n1"]
    )
    assert errcode == 0
    # Compare against the full signature help response
    ref = {
        "signatures": [
            {
                "label": "sub2call(arg1, arg2=arg2)",
                "parameters": [
                    {
                        "label": "arg1",
                        "documentation": {
                            "kind": "markdown",
                            "value": (
                                "```fortran90\nINTEGER, INTENT(IN) ::"
                                " arg1\n```\n-----\nDoc for arg1"
                            ),
                        },
                    },
                    {
                        "label": "arg2=arg2",
                        "documentation": {
                            "kind": "markdown",
                            "value": (
                                "```fortran90\nINTEGER, INTENT(IN), OPTIONAL ::"
                                " arg2\n```\n-----\nDoc for arg2"
                            ),
                        },
                    },
                ],
                "documentation": {"kind": "markdown", "value": "Top level Doc"},
            }
        ],
        "activeParameter": 0,
    }
    assert results[1] == ref
