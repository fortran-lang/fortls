from pathlib import Path

import pytest
from setup_tests import run_request, write_rpc_notification, write_rpc_request

from fortls.interface import cli
from fortls.langserver import LangServer

# The in-process tests initialise the workspace with a pool of processes
pytestmark = pytest.mark.filterwarnings(
    "ignore:This process .* is multi-threaded:DeprecationWarning"
)

MODULE = """module shapes
  implicit none
  integer :: n_shapes = 0
  type :: circle
    real :: radius
  end type circle
contains
  function area(c, scale) result(a)
    type(circle), intent(in) :: c
    real, intent(in) :: scale
    real :: a
    real :: tmp
    tmp = 3.14 * c%radius**2
    if (tmp > 0) then
      a = scale * tmp
    end if
  end function area
  subroutine count_shape()
    integer :: i
    do i = 1, 2
      n_shapes = n_shapes + 1
    end do
  end subroutine count_shape
end module shapes
"""

PROGRAM = """program main
  use shapes, only: circle, area, n_shapes
  implicit none
  type(circle) :: c
  real :: x
  x = area(c, 2.0)
  n_shapes = 1
end program main
"""


class Conn:
    def send_notification(self, *args, **kwargs):
        pass


@pytest.fixture()
def project(tmp_path: Path) -> Path:
    (tmp_path / "shapes.f90").write_text(MODULE)
    (tmp_path / "main.f90").write_text(PROGRAM)
    return tmp_path


def init_server(root: Path) -> LangServer:
    server = LangServer(Conn(), vars(cli("fortls").parse_args(["-n", "1"])))
    server.serve_initialize({"params": {"rootPath": str(root)}})
    return server


def names(scope) -> set[str]:
    return {child.name.lower() for child in scope.children}


def test_workspace_files_are_summarized(project):
    server = init_server(project)
    shapes = server.workspace[str(project / "shapes.f90")]
    assert shapes.summary
    assert shapes.contents_split == []
    module = shapes.ast.global_dict["shapes"]
    assert names(module) == {"n_shapes", "circle", "area", "count_shape"}
    area = next(c for c in module.children if c.name == "area")
    # The signature is kept: arguments and result, not the local variable
    assert names(area) == {"c", "scale", "a"}
    count_shape = next(c for c in module.children if c.name == "count_shape")
    # Neither the local variable nor the DO block
    assert names(count_shape) == set()
    assert names(module.children[1]) == {"radius"}
    assert [scope.name for scope in shapes.ast.scope_list] == [
        "shapes",
        "circle",
        "area",
        "count_shape",
    ]
    # Links to the kept objects are resolved
    assert [arg.name for arg in area.arg_objs] == ["c", "scale"]
    assert area.result_obj.name == "a"


def test_open_parses_in_full_and_close_summarizes(project):
    server = init_server(project)
    path = str(project / "shapes.f90")
    request = {"params": {"textDocument": {"uri": path}}}
    server.serve_onOpen(request)
    shapes = server.workspace[path]
    assert not shapes.summary
    assert shapes.contents_split[0] == "module shapes"
    count_shape = shapes.ast.global_dict["shapes"].children[3]
    assert names(count_shape) == {"i", "#do1"}
    server.serve_onClose(request)
    assert shapes.summary
    assert shapes.contents_split == []
    assert names(shapes.ast.global_dict["shapes"].children[3]) == set()


def test_request_parses_summarized_file_in_full(project):
    server = init_server(project)
    path = str(project / "shapes.f90")
    hover = server.serve_hover(
        {
            "params": {
                "textDocument": {"uri": path},
                "position": {"line": 11, "character": 13},
            }
        }
    )
    assert "REAL :: tmp" in hover["contents"]["value"]
    assert not server.workspace[path].summary


def test_definition_and_references_in_summarized_files(project):
    shapes, main = project / "shapes.f90", project / "main.f90"
    request_string = write_rpc_request(1, "initialize", {"rootPath": str(project)})
    # shapes.f90 is summarized: the position is found in the file on disk
    request_string += write_rpc_request(
        2,
        "textDocument/definition",
        {"textDocument": {"uri": str(main)}, "position": {"line": 5, "character": 7}},
    )
    request_string += write_rpc_notification(
        "textDocument/didClose", {"textDocument": {"uri": str(main)}}
    )
    request_string += write_rpc_notification(
        "textDocument/didOpen", {"textDocument": {"uri": str(shapes)}}
    )
    # main.f90 is summarized again: its references are found in a full parse
    request_string += write_rpc_request(
        3,
        "textDocument/references",
        {
            "textDocument": {"uri": str(shapes)},
            "position": {"line": 2, "character": 14},
        },
    )
    errcode, results = run_request(request_string, ["-n 2", "--disable_diagnostics"])
    assert errcode == 0
    assert Path(results[1]["uri"]).name == "shapes.f90"
    assert results[1]["range"]["start"] == {"line": 7, "character": 11}
    refs = sorted(
        (Path(ref["uri"]).name, ref["range"]["start"]["line"]) for ref in results[2]
    )
    assert refs == [
        ("main.f90", 1),
        ("main.f90", 6),
        ("shapes.f90", 2),
        ("shapes.f90", 20),
        ("shapes.f90", 20),
    ]


def test_summarized_file_keeps_definitions_of_its_last_parse(tmp_path):
    counter, user = tmp_path / "counter.F90", tmp_path / "user.f90"
    counter.write_text("""module counter
#ifdef HAVE_TOTAL
  integer :: total = 0
#endif
contains
  subroutine bump()
    total = total + 1
  end subroutine bump
end module counter
""")
    user.write_text("program user\n  use counter\n  total = 2\nend program user\n")
    server = init_server(tmp_path)
    # A file opened after initialize adds its definitions to the server's
    config = tmp_path / "config.F90"
    config.write_text("#define HAVE_TOTAL\n")
    server.serve_onOpen({"params": {"textDocument": {"uri": str(config)}}})
    # counter.F90 changed since initialize: parsed with the server's definitions
    counter.write_text(counter.read_text() + "! changed\n")
    request = {"params": {"textDocument": {"uri": str(counter)}}}
    server.serve_onOpen(request)
    server.serve_onClose(request)
    # References in counter.F90 are searched with the same definitions
    refs = server.serve_references(
        {
            "params": {
                "textDocument": {"uri": str(user)},
                "position": {"line": 2, "character": 3},
            }
        }
    )
    assert sorted(
        (Path(ref["uri"]).name, ref["range"]["start"]["line"]) for ref in refs
    ) == [
        ("counter.F90", 2),
        ("counter.F90", 6),
        ("counter.F90", 6),
        ("user.f90", 2),
    ]
    # And counter.F90 is parsed in full again with them
    assert "total" in names(server.get_file(str(counter)).ast.global_dict["counter"])


@pytest.mark.parametrize("request_file, line", [("decl.f90", 0), ("prog.f90", 4)])
def test_references_through_includes_of_summarized_files(tmp_path, request_file, line):
    (tmp_path / "decl.f90").write_text("integer :: total\n")
    (tmp_path / "prog.f90").write_text("""module counters
  include "decl.f90"
contains
  subroutine bump()
    total = total + 1
  end subroutine bump
end module counters
""")
    server = init_server(tmp_path)
    # The other file is summarized: its full parse resolves its includes, or
    # is included where the summarized file is
    refs = server.serve_references(
        {
            "params": {
                "textDocument": {"uri": str(tmp_path / request_file)},
                "position": {"line": line, "character": 12},
            }
        }
    )
    assert sorted(
        (Path(ref["uri"]).name, ref["range"]["start"]["line"]) for ref in refs
    ) == [("decl.f90", 0), ("prog.f90", 4), ("prog.f90", 4)]
    # The workspace is restored: the module includes the objects of decl.f90
    module = server.workspace[str(tmp_path / "prog.f90")].ast.global_dict["counters"]
    totals = [child for child in module.children if child.name == "total"]
    assert len(totals) == 1
    assert totals[0].parent is module
    assert totals[0].file_ast is server.workspace[str(tmp_path / "decl.f90")].ast
