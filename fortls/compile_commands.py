"""Parse compile_commands.json (Clang Compilation Database) for fortls."""

from __future__ import annotations

import json
import logging
import os
import shlex
from dataclasses import dataclass, field

log = logging.getLogger(__name__)


@dataclass
class CompileCommandsConfig:
    """Configuration extracted from compile_commands.json."""

    include_dirs: set[str] = field(default_factory=set)
    pp_defs: dict[str, str] = field(default_factory=dict)
    source_files: set[str] = field(default_factory=set)
    file_to_module_dir: dict[str, str] = field(default_factory=dict)


def find_compile_commands(root_path: str, custom_path: str | None = None) -> str | None:
    """Search for compile_commands.json.

    Search order:
    1. custom_path (if provided)
    2. root_path/compile_commands.json
    3. root_path/build/compile_commands.json
    4. root_path/builddir/compile_commands.json
    5. Recursive search in subdirectories
    """
    if custom_path:
        if os.path.isabs(custom_path):
            check_path = custom_path
        else:
            check_path = os.path.join(root_path, custom_path)
        if os.path.isfile(check_path):
            return os.path.abspath(check_path)
        log.warning("Specified compile_commands.json not found: %s", custom_path)
        return None

    search_paths = [
        os.path.join(root_path, "compile_commands.json"),
        os.path.join(root_path, "build", "compile_commands.json"),
        os.path.join(root_path, "builddir", "compile_commands.json"),
    ]

    for path in search_paths:
        if os.path.isfile(path):
            return os.path.abspath(path)

    # Recursive search for projects in subdirectories
    for root, dirs, files in os.walk(root_path):
        dirs[:] = [
            d
            for d in dirs
            if not d.startswith(".") and d not in ("node_modules", "__pycache__")
        ]
        if "compile_commands.json" in files:
            return os.path.abspath(os.path.join(root, "compile_commands.json"))

    return None


def parse_compile_commands(
    path: str, root_path: str | None = None
) -> CompileCommandsConfig:
    """Parse compile_commands.json and extract configuration."""
    config = CompileCommandsConfig()

    try:
        with open(path, encoding="utf-8") as f:
            entries = json.load(f)
    except (OSError, json.JSONDecodeError) as e:
        log.error("Failed to parse compile_commands.json: %s", e)
        return config

    if not isinstance(entries, list):
        log.error("compile_commands.json must contain a JSON array")
        return config

    # Map basenames to workspace paths for path normalization
    workspace_files: dict[str, str] = {}
    if root_path:
        build_dir = os.path.dirname(path)
        for root, _, files in os.walk(root_path):
            if build_dir and root.startswith(build_dir):
                continue
            for f in files:
                if "-pp.f90" in f.lower() or "-pp.f" in f.lower():
                    continue
                if _is_fortran_file(f):
                    workspace_files[f] = os.path.join(root, f)

    for entry in entries:
        if not isinstance(entry, dict):
            continue

        directory = entry.get("directory", "")
        source_file = _normalize_source_path(
            entry.get("file", ""), directory, root_path, workspace_files
        )

        if source_file:
            config.source_files.add(source_file)

        args = _get_arguments(entry)
        if args is None:
            continue

        inc_dirs, defines, module_dir = _parse_compiler_args(args, directory)
        config.include_dirs.update(inc_dirs)

        for name, value in defines.items():
            if name not in config.pp_defs:
                config.pp_defs[name] = value

        if source_file and module_dir:
            config.file_to_module_dir[source_file] = module_dir

    log.info(
        "Loaded compile_commands.json: %d include dirs, %d defines, %d files",
        len(config.include_dirs),
        len(config.pp_defs),
        len(config.source_files),
    )

    return config


def _is_fortran_file(filename: str) -> bool:
    """Check if filename has a Fortran extension."""
    return filename.endswith(
        (".f90", ".F90", ".f", ".F", ".f95", ".F95", ".f03", ".F03", ".f08", ".F08")
    )


def _normalize_source_path(
    source_file: str,
    directory: str,
    root_path: str | None,
    workspace_files: dict[str, str],
) -> str | None:
    """Normalize source file path, falling back to basename lookup."""
    if not source_file:
        return None

    if not os.path.isabs(source_file):
        source_file = os.path.join(directory, source_file)

    source_file = os.path.normpath(source_file)

    if root_path and not os.path.exists(source_file):
        basename = os.path.basename(source_file)
        if basename in workspace_files:
            source_file = workspace_files[basename]

    return source_file


def _get_arguments(entry: dict) -> list | None:
    """Extract arguments from compile_commands entry."""
    if "arguments" in entry:
        args = entry["arguments"]
        return args if isinstance(args, list) else None
    elif "command" in entry:
        try:
            return shlex.split(entry["command"])
        except ValueError:
            return None
    return None


def _parse_compiler_args(
    args: list, directory: str
) -> tuple[set[str], dict[str, str], str | None]:
    """Parse compiler arguments for include dirs, defines, and module dir."""
    include_dirs: set[str] = set()
    pp_defs: dict[str, str] = {}
    module_dir: str | None = None

    i = 0
    while i < len(args):
        arg = args[i]

        if arg.startswith("-I"):
            path = _extract_flag_value(arg, "-I", args, i)
            if path is not None:
                if arg == "-I":
                    i += 1
                resolved = _resolve_path(path, directory)
                if resolved:
                    include_dirs.add(resolved)

        elif arg.startswith("-J"):
            path = _extract_flag_value(arg, "-J", args, i)
            if path is not None:
                if arg == "-J":
                    i += 1
                resolved = _resolve_path(path, directory)
                if resolved:
                    include_dirs.add(resolved)
                    module_dir = resolved

        elif arg == "-module" and i + 1 < len(args):
            path = args[i + 1]
            i += 1
            resolved = _resolve_path(path, directory)
            if resolved:
                include_dirs.add(resolved)
                module_dir = resolved

        elif arg.startswith("-D"):
            define = _extract_flag_value(arg, "-D", args, i)
            if define is not None:
                if arg == "-D":
                    i += 1
                name, value = _parse_define(define)
                if name:
                    pp_defs[name] = value

        i += 1

    return include_dirs, pp_defs, module_dir


def _extract_flag_value(arg: str, flag: str, args: list, index: int) -> str | None:
    """Extract value from -Xvalue or -X value format."""
    if arg == flag:
        return args[index + 1] if index + 1 < len(args) else None
    return arg[len(flag) :]


def _resolve_path(path: str, directory: str) -> str | None:
    """Resolve path relative to directory."""
    if not path:
        return None
    if not os.path.isabs(path):
        path = os.path.join(directory, path)
    return os.path.normpath(path)


def _parse_define(define: str) -> tuple[str, str]:
    """Parse preprocessor definition into (name, value)."""
    if not define:
        return ("", "")
    if "=" in define:
        name, value = define.split("=", 1)
        return (name.strip(), value)
    return (define.strip(), "True")
