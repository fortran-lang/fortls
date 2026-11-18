"""Tests for pp_include go-to-definition in langserver.py.

This test file specifically covers the pp_includes branch in serve_definition().
"""

from __future__ import annotations

import os
from unittest.mock import MagicMock

from fortls.parsers.internal.parser import FortranFile, preprocess_file


class TestServeDefinitionPpIncludes:
    """Tests for serve_definition() pp_includes branch in langserver.py."""

    def _create_langserver_mock(self):
        """Create a mock LangServer for testing."""
        from fortls.langserver import LangServer

        # Create mock connection
        mock_conn = MagicMock()

        # Default settings
        settings = {
            "source_dirs": set(),
            "excl_paths": set(),
            "incl_suffixes": {".f90", ".F90"},
            "excl_suffixes": set(),
            "pp_suffixes": [".F90", ".f90"],
            "pp_defs": {},
            "include_dirs": set(),
            "debug_log": False,
            "debug_parser": False,
            "debug_preproc": False,
            "incremental_sync": True,
            "autocomplete_name_only": False,
            "autocomplete_no_prefix": False,
            "autocomplete_no_snippets": False,
            "hover_signature": True,
            "hover_language": "fortran",
            "notify_init": False,
            "sort_keywords": True,
            "lowercase_intrinsics": False,
            "use_signature_help": True,
            "enable_code_actions": False,
            "disable_diagnostics": False,
            "max_line_length": -1,
            "max_comment_line_length": -1,
            "symbol_skip_mem": False,
            "recursion_limit": 10000,
            "nthreads": 1,
            "config": ".fortls",
            "disable_autoupdate": True,
        }

        return LangServer(mock_conn, settings)

    def test_serve_definition_pp_include_with_valid_file(self, tmp_path):
        """Test go-to-definition for #include when the included file exists."""
        # Create the include file
        inc_file = tmp_path / "test.inc"
        inc_file.write_text("integer :: x\n")

        # Create main file with #include on line 1 (0-indexed)
        main_file = tmp_path / "main.F90"
        main_file.write_text('#include "test.inc"\nmodule test\nend module\n')

        # Create FortranFile, load from disk, and parse to populate pp_includes
        ff = FortranFile(str(main_file), pp_suffixes=[".F90", ".f90"])
        ff.load_from_disk()  # Must load first to populate contents_split
        ff.parse(pp_defs={}, include_dirs={str(tmp_path)})

        # Verify pp_includes is populated
        assert len(ff.pp_includes) == 1
        inc_line, inc_filename, inc_path = ff.pp_includes[0]
        assert inc_filename == "test.inc"
        assert inc_path is not None
        assert os.path.isfile(inc_path)
        assert "\\" not in inc_path

        # Create mock workspace with our file
        mock_workspace = {str(main_file): ff}

        # Create mock LangServer
        ls = self._create_langserver_mock()
        ls.workspace = mock_workspace
        ls.root_path = str(tmp_path)

        # Create request for definition at line 0 (the #include line)
        request = {
            "params": {
                "textDocument": {"uri": str(main_file)},
                "position": {"line": 0, "character": 5},
            }
        }

        # Call serve_definition
        result = ls.serve_definition(request)

        # Verify result is a valid URI JSON pointing to the include file
        assert result is not None
        assert "uri" in result
        assert "range" in result
        assert inc_filename in result["uri"]

    def test_serve_definition_pp_include_no_file(self, tmp_path):
        """Test go-to-definition for #include when the included file doesn't exist."""
        # Create main file with #include pointing to non-existent file
        main_file = tmp_path / "main.F90"
        main_file.write_text('#include "nonexistent.inc"\nmodule test\nend module\n')

        # Create FortranFile, load from disk, and parse.
        # The include is tracked but unresolved because file is missing.
        ff = FortranFile(str(main_file), pp_suffixes=[".F90", ".f90"])
        ff.load_from_disk()  # Must load first
        ff.parse(pp_defs={}, include_dirs={str(tmp_path)})

        # Verify pp_includes has the include tracked but path is None
        assert len(ff.pp_includes) == 1
        inc_line, inc_filename, inc_path = ff.pp_includes[0]
        assert inc_filename == "nonexistent.inc"
        assert inc_path is None  # File doesn't exist

        # Create mock workspace
        mock_workspace = {str(main_file): ff}

        # Create mock LangServer
        ls = self._create_langserver_mock()
        ls.workspace = mock_workspace
        ls.root_path = str(tmp_path)

        # Create request for definition at line 0
        request = {
            "params": {
                "textDocument": {"uri": str(main_file)},
                "position": {"line": 0, "character": 5},
            }
        }

        # Call serve_definition - should return None because file doesn't exist
        result = ls.serve_definition(request)

        # Should fall through to normal definition handling or return None
        # The pp_includes branch returns None when `inc_path` is None
        # or `os.path.isfile` fails.
        assert result is None

    def test_serve_definition_pp_include_empty_list(self, tmp_path):
        """Test serve_definition when file has no pp_includes."""
        # Create main file WITHOUT #include
        main_file = tmp_path / "main.F90"
        main_file.write_text("module test\nend module\n")

        # Create FortranFile, load, and parse so ast is populated
        ff = FortranFile(str(main_file), pp_suffixes=[".F90", ".f90"])
        ff.load_from_disk()
        ff.ast = ff.parse(pp_defs={}, include_dirs=set())

        # Create mock workspace
        mock_workspace = {str(main_file): ff}

        # Create mock LangServer
        ls = self._create_langserver_mock()
        ls.workspace = mock_workspace
        ls.root_path = str(tmp_path)

        # Create request for definition
        request = {
            "params": {
                "textDocument": {"uri": str(main_file)},
                "position": {"line": 0, "character": 5},
            }
        }

        # Call serve_definition - should not crash
        ls.serve_definition(request)
        # Result depends on whether there's a valid definition at that position

    def test_serve_definition_pp_include_line_match(self, tmp_path):
        """Test that the line matching logic works correctly."""
        # Create the include file
        inc_file = tmp_path / "test.inc"
        inc_file.write_text("integer :: x\n")

        # Create main file with #include on line 2 (0-indexed)
        main_file = tmp_path / "main.F90"
        main_file.write_text('program main\n#include "test.inc"\nend program\n')

        # Create FortranFile, load from disk, and parse
        ff = FortranFile(str(main_file), pp_suffixes=[".F90", ".f90"])
        ff.load_from_disk()  # Must load first
        ff.parse(pp_defs={}, include_dirs={str(tmp_path)})

        # Verify pp_includes
        assert len(ff.pp_includes) == 1
        inc_line, inc_filename, inc_path = ff.pp_includes[0]
        # inc_line is 1-indexed (line 2 in the file = inc_line 2)
        assert inc_line == 2

        # Create mock workspace
        mock_workspace = {str(main_file): ff}

        # Create mock LangServer
        ls = self._create_langserver_mock()
        ls.workspace = mock_workspace
        ls.root_path = str(tmp_path)

        # Request at line 1 (0-indexed) - this is the #include line
        request = {
            "params": {
                "textDocument": {"uri": str(main_file)},
                "position": {"line": 1, "character": 5},
            }
        }

        result = ls.serve_definition(request)
        assert result is not None
        assert "uri" in result


class TestPpIncludesTracking:
    """Tests for pp_includes tracking in the preprocessor."""

    def test_pp_includes_tracked_in_preprocess_file(self, tmp_path):
        """Test that #include statements are tracked in preprocess_file output."""
        # Create include file
        inc_file = tmp_path / "test.inc"
        inc_file.write_text("integer :: x\n")

        # Create main file
        main_file = tmp_path / "main.F90"
        main_file.write_text('#include "test.inc"\nmodule test\nend module\n')

        # Run preprocess_file
        contents = ['#include "test.inc"', "module test", "end module"]
        output, pp_skips, pp_defines, pp_defs, pp_includes = preprocess_file(
            contents,
            file_path=str(main_file),
            include_dirs={str(tmp_path)},
        )

        # Verify tracking
        assert len(pp_includes) == 1
        inc_line, inc_filename, inc_path = pp_includes[0]
        assert inc_line == 1  # 1-indexed
        assert inc_filename == "test.inc"
        assert inc_path is not None
        assert os.path.isfile(inc_path)
        assert "\\" not in inc_path

    def test_pp_includes_multiple_includes(self, tmp_path):
        """Test tracking multiple #include statements."""
        # Create include files
        inc1 = tmp_path / "inc1.inc"
        inc1.write_text("integer :: a\n")
        inc2 = tmp_path / "inc2.inc"
        inc2.write_text("integer :: b\n")

        # Create main file with multiple includes
        main_file = tmp_path / "main.F90"
        main_file.write_text(
            '#include "inc1.inc"\n#include "inc2.inc"\nmodule test\nend module\n'
        )

        # Run preprocess_file
        contents = [
            '#include "inc1.inc"',
            '#include "inc2.inc"',
            "module test",
            "end module",
        ]
        output, pp_skips, pp_defines, pp_defs, pp_includes = preprocess_file(
            contents,
            file_path=str(main_file),
            include_dirs={str(tmp_path)},
        )

        # Verify both are tracked
        assert len(pp_includes) == 2
        assert pp_includes[0][1] == "inc1.inc"
        assert pp_includes[1][1] == "inc2.inc"

    def test_pp_includes_conditionally_skipped(self):
        """Test that #includes in skipped preprocessor blocks are not tracked."""
        contents = [
            "#if 0",
            '#include "should_not_track.inc"',
            "#endif",
            "module test",
            "end module",
        ]
        output, pp_skips, pp_defines, pp_defs, pp_includes = preprocess_file(contents)

        # The include is inside #if 0 block, should be skipped
        assert pp_includes == []
