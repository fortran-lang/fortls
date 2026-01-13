"""Tests for compile_commands.json parser"""

from __future__ import annotations

import json
import os

# Import the module under test
import sys
import tempfile
from pathlib import Path

from setup_tests import run_request, test_dir, write_rpc_request

sys.path.insert(0, str(Path(__file__).parent.parent))
from fortls.compile_commands import (
    _parse_compiler_args,
    _parse_define,
    find_compile_commands,
    parse_compile_commands,
)


class TestParseDefine:
    """Tests for _parse_define function"""

    def test_simple_define(self):
        name, value = _parse_define("DEBUG")
        assert name == "DEBUG"
        assert value == "True"

    def test_define_with_value(self):
        name, value = _parse_define("VERSION=1.0")
        assert name == "VERSION"
        assert value == "1.0"

    def test_define_with_quoted_value(self):
        name, value = _parse_define('STR="hello world"')
        assert name == "STR"
        assert value == '"hello world"'

    def test_define_with_equals_in_value(self):
        name, value = _parse_define("EXPR=a=b")
        assert name == "EXPR"
        assert value == "a=b"

    def test_empty_define(self):
        name, value = _parse_define("")
        assert name == ""
        assert value == ""


class TestParseCompilerArgs:
    """Tests for _parse_compiler_args function"""

    def test_include_dir_attached(self):
        """Test -Ipath format"""
        args = ["gfortran", "-I/usr/include", "-c", "test.f90"]
        inc_dirs, pp_defs, module_dir = _parse_compiler_args(args, "/home/user")
        assert "/usr/include" in inc_dirs
        assert len(pp_defs) == 0
        assert module_dir is None

    def test_include_dir_separate(self):
        """Test -I path format (with space)"""
        args = ["gfortran", "-I", "/usr/include", "-c", "test.f90"]
        inc_dirs, *_ = _parse_compiler_args(args, "/home/user")
        assert "/usr/include" in inc_dirs

    def test_include_dir_relative(self):
        """Test relative include path"""
        args = ["gfortran", "-Iinclude", "-c", "test.f90"]
        inc_dirs, *_ = _parse_compiler_args(args, "/home/user/project")
        assert "/home/user/project/include" in inc_dirs

    def test_module_dir_gfortran(self):
        """Test -J flag (gfortran module directory)"""
        args = ["gfortran", "-Jbuild/mod", "-c", "test.f90"]
        inc_dirs, _, module_dir = _parse_compiler_args(args, "/home/user/project")
        assert "/home/user/project/build/mod" in inc_dirs
        assert module_dir == "/home/user/project/build/mod"

    def test_module_dir_intel(self):
        """Test -module flag (Intel Fortran)"""
        args = ["ifort", "-module", "build/mod", "-c", "test.f90"]
        inc_dirs, _, module_dir = _parse_compiler_args(args, "/home/user/project")
        assert "/home/user/project/build/mod" in inc_dirs
        assert module_dir == "/home/user/project/build/mod"

    def test_preprocessor_define_simple(self):
        """Test -DNAME format"""
        args = ["gfortran", "-DDEBUG", "-c", "test.f90"]
        _, pp_defs, _ = _parse_compiler_args(args, "/home/user")
        assert pp_defs["DEBUG"] == "True"

    def test_preprocessor_define_with_value(self):
        """Test -DNAME=value format"""
        args = ["gfortran", "-DVERSION=1.0", "-c", "test.f90"]
        _, pp_defs, _ = _parse_compiler_args(args, "/home/user")
        assert pp_defs["VERSION"] == "1.0"

    def test_preprocessor_define_separate(self):
        """Test -D NAME format (with space)"""
        args = ["gfortran", "-D", "DEBUG", "-c", "test.f90"]
        _, pp_defs, _ = _parse_compiler_args(args, "/home/user")
        assert pp_defs["DEBUG"] == "True"

    def test_multiple_flags(self):
        """Test multiple -I and -D flags"""
        args = [
            "gfortran",
            "-I/usr/include",
            "-Ilib",
            "-DDEBUG",
            "-DVERSION=2",
            "-c",
            "test.f90",
        ]
        inc_dirs, pp_defs, _ = _parse_compiler_args(args, "/home/user/project")
        assert "/usr/include" in inc_dirs
        assert "/home/user/project/lib" in inc_dirs
        assert pp_defs["DEBUG"] == "True"
        assert pp_defs["VERSION"] == "2"


class TestFindCompileCommands:
    """Tests for find_compile_commands function"""

    def test_custom_path_absolute(self):
        """Test with absolute custom path"""
        with tempfile.TemporaryDirectory() as tmpdir:
            cc_path = os.path.join(tmpdir, "my_compile_commands.json")
            with open(cc_path, "w") as f:
                json.dump([], f)
            result = find_compile_commands("/some/root", cc_path)
            assert result == cc_path

    def test_custom_path_relative(self):
        """Test with relative custom path"""
        with tempfile.TemporaryDirectory() as tmpdir:
            cc_path = os.path.join(tmpdir, "custom.json")
            with open(cc_path, "w") as f:
                json.dump([], f)
            result = find_compile_commands(tmpdir, "custom.json")
            assert result == cc_path

    def test_custom_path_not_found(self):
        """Test with non-existent custom path"""
        result = find_compile_commands("/tmp", "/nonexistent/path.json")
        assert result is None

    def test_root_path_detection(self):
        """Test auto-detection in root path"""
        with tempfile.TemporaryDirectory() as tmpdir:
            cc_path = os.path.join(tmpdir, "compile_commands.json")
            with open(cc_path, "w") as f:
                json.dump([], f)
            result = find_compile_commands(tmpdir)
            assert result == cc_path

    def test_build_dir_detection(self):
        """Test auto-detection in build/ subdirectory"""
        with tempfile.TemporaryDirectory() as tmpdir:
            build_dir = os.path.join(tmpdir, "build")
            os.makedirs(build_dir)
            cc_path = os.path.join(build_dir, "compile_commands.json")
            with open(cc_path, "w") as f:
                json.dump([], f)
            result = find_compile_commands(tmpdir)
            assert result == cc_path

    def test_not_found(self):
        """Test when no compile_commands.json exists"""
        with tempfile.TemporaryDirectory() as tmpdir:
            result = find_compile_commands(tmpdir)
            assert result is None


class TestParseCompileCommands:
    """Tests for parse_compile_commands function"""

    def test_cmake_format(self):
        """Test parsing CMake-style compile_commands.json (command string)"""
        cc_path = test_dir / "compile_commands" / "cmake_format.json"
        config = parse_compile_commands(str(cc_path))

        # Check include dirs
        assert "/home/user/project/include" in config.include_dirs
        assert "/home/user/project/src" in config.include_dirs
        assert "/home/user/project/build/mod" in config.include_dirs

        # Check preprocessor definitions
        assert config.pp_defs["DEBUG"] == "1"
        # Note: shlex.split() removes outer quotes from -DVERSION="1.0"
        assert config.pp_defs["VERSION"] == "1.0"

        # Check source files
        assert "/home/user/project/src/main.f90" in config.source_files
        assert "/home/user/project/src/module.f90" in config.source_files

    def test_fpm_format(self):
        """Test parsing fpm-style compile_commands.json (arguments array)"""
        cc_path = test_dir / "compile_commands" / "fpm_format.json"
        config = parse_compile_commands(str(cc_path))

        # Check include dirs (should resolve relative to directory)
        assert "/home/user/fpm_project/include" in config.include_dirs
        assert "/home/user/fpm_project/build" in config.include_dirs

        # Check preprocessor definitions
        assert config.pp_defs["USE_MPI"] == "True"
        assert config.pp_defs["APP_NAME"] == "myapp"

        # Check source files (relative paths resolved)
        assert "/home/user/fpm_project/src/module.f90" in config.source_files
        assert "/home/user/fpm_project/app/main.f90" in config.source_files

    def test_invalid_json(self):
        """Test handling of invalid JSON file"""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".json", delete=False) as f:
            f.write("not valid json")
            f.flush()
            config = parse_compile_commands(f.name)
            assert len(config.include_dirs) == 0
            assert len(config.pp_defs) == 0
            os.unlink(f.name)

    def test_nonexistent_file(self):
        """Test handling of non-existent file"""
        config = parse_compile_commands("/nonexistent/path.json")
        assert len(config.include_dirs) == 0
        assert len(config.pp_defs) == 0

    def test_empty_array(self):
        """Test handling of empty compile_commands.json"""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".json", delete=False) as f:
            json.dump([], f)
            f.flush()
            config = parse_compile_commands(f.name)
            assert len(config.include_dirs) == 0
            assert len(config.pp_defs) == 0
            os.unlink(f.name)


class TestIntegration:
    """Integration tests with the language server"""

    def test_server_with_compile_commands(self):
        """Test that server loads compile_commands.json"""
        # Create a temporary project with compile_commands.json
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create build directory with compile_commands.json
            build_dir = os.path.join(tmpdir, "build")
            os.makedirs(build_dir)
            cc_path = os.path.join(build_dir, "compile_commands.json")

            # Create a Fortran source file
            src_file = os.path.join(tmpdir, "test.f90")
            with open(src_file, "w") as f:
                f.write("program test\nend program test\n")

            # Create compile_commands.json with a preprocessor define
            compile_commands = [
                {
                    "directory": tmpdir,
                    "arguments": ["gfortran", "-DTEST_DEFINE=42", "-c", "test.f90"],
                    "file": "test.f90",
                }
            ]
            with open(cc_path, "w") as f:
                json.dump(compile_commands, f)

            # Initialize the server and check it works
            string = write_rpc_request(1, "initialize", {"rootPath": tmpdir})
            errcode, results = run_request(string)
            assert errcode == 0
            assert results[0]["capabilities"]["hoverProvider"] is True

    def test_server_with_disabled_compile_commands(self):
        """Test that --disable_compile_commands works"""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create build directory with compile_commands.json
            build_dir = os.path.join(tmpdir, "build")
            os.makedirs(build_dir)
            cc_path = os.path.join(build_dir, "compile_commands.json")

            # Create a Fortran source file
            src_file = os.path.join(tmpdir, "test.f90")
            with open(src_file, "w") as f:
                f.write("program test\nend program test\n")

            # Create compile_commands.json
            compile_commands = [
                {
                    "directory": tmpdir,
                    "arguments": ["gfortran", "-c", "test.f90"],
                    "file": "test.f90",
                }
            ]
            with open(cc_path, "w") as f:
                json.dump(compile_commands, f)

            # Initialize the server with --disable_compile_commands
            string = write_rpc_request(1, "initialize", {"rootPath": tmpdir})
            errcode, _ = run_request(string, ["--disable_compile_commands"])
            assert errcode == 0

    def test_file_to_module_dir_mapping(self):
        """Test that compile_commands.json extracts file to module dir mapping.

        When files are compiled with different -J flags, the file_to_module_dir
        mapping should group them correctly for disambiguation.
        """
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create directory structure
            build_dir = os.path.join(tmpdir, "build")
            mod1_dir = os.path.join(build_dir, "mod", "exe1")
            mod2_dir = os.path.join(build_dir, "mod", "exe2")
            os.makedirs(mod1_dir)
            os.makedirs(mod2_dir)

            exe1_path = os.path.join(tmpdir, "exe1.f90")
            module1_path = os.path.join(tmpdir, "module1.f90")
            exe2_path = os.path.join(tmpdir, "exe2.f90")
            module2_path = os.path.join(tmpdir, "module2.f90")

            # Create compile_commands.json that groups exe1+module1 and exe2+module2
            compile_commands = [
                {
                    "directory": build_dir,
                    "command": f"/usr/bin/f95 -J{mod1_dir} -c {exe1_path}",
                    "file": exe1_path,
                },
                {
                    "directory": build_dir,
                    "command": f"/usr/bin/f95 -J{mod1_dir} -c {module1_path}",
                    "file": module1_path,
                },
                {
                    "directory": build_dir,
                    "command": f"/usr/bin/f95 -J{mod2_dir} -c {exe2_path}",
                    "file": exe2_path,
                },
                {
                    "directory": build_dir,
                    "command": f"/usr/bin/f95 -J{mod2_dir} -c {module2_path}",
                    "file": module2_path,
                },
            ]
            cc_path = os.path.join(build_dir, "compile_commands.json")
            with open(cc_path, "w") as f:
                json.dump(compile_commands, f)

            # Parse the compile_commands.json
            config = parse_compile_commands(cc_path)

            # Verify file_to_module_dir mapping
            assert exe1_path in config.file_to_module_dir
            assert module1_path in config.file_to_module_dir
            assert exe2_path in config.file_to_module_dir
            assert module2_path in config.file_to_module_dir

            # exe1 and module1 should share the same module dir
            assert config.file_to_module_dir[exe1_path] == mod1_dir
            assert config.file_to_module_dir[module1_path] == mod1_dir

            # exe2 and module2 should share the same module dir
            assert config.file_to_module_dir[exe2_path] == mod2_dir
            assert config.file_to_module_dir[module2_path] == mod2_dir

            # The two groups should have different module dirs
            assert (
                config.file_to_module_dir[exe1_path]
                != config.file_to_module_dir[exe2_path]
            )
