from setup_tests import run_request, test_dir, write_rpc_request


def check_msg(ref, res):
    assert ref["type"] == res["type"]
    assert ref["message"] == res["message"]


# def test_config_file_non_existent():
#     string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
#     errcode, results = run_request(string, ["-c", "fake.json"])
#
#     ref = {"type": 1, "message": "Configuration file 'fake.json' not found"}
#     assert errcode == 0
#     check_msg(ref, results[0])


def test_config_file_non_existent_options():
    string = write_rpc_request(1, "initialize", {"rootPath": str(test_dir)})
    errcode, results = run_request(string, ["-c", "wrong_syntax.json"])

    ref = {
        "type": 1,
        "message": (
            'Error: "<string>:2 Unexpected "," at column 18" while reading'
            ' "wrong_syntax.json" Configuration file'
        ),
    }
    assert errcode == 0
    check_msg(ref, results[0])
