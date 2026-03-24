"""Parse compile_commands.json (Clang Compilation Database) for fortls."""

from __future__ import annotations

import json
import logging
import os
import shlex
from dataclasses import dataclass, field
from pathlib import Path

log = logging.getLogger(__name__)


@dataclass
class CompileCommandsConfig:
    """Configuration extracted from compile_commands.json."""

    include_dirs: set[str] = field(default_factory=set)
    pp_defs: dict[str, str] = field(default_factory=dict)
    source_files: set[str] = field(default_factory=set)
    file_to_module_dir: dict[str, str] = field(default_factory=dict)


def _normpath(path: Path) -> Path:
    """Normalize paths without resolving symlinks."""
    return Path(os.path.normpath(str(path)))


def find_compile_commands(root_path: str, custom_path: str | None = None) -> str | None:
    """Search for compile_commands.json.

    Search order:
    1. custom_path (if provided)
    2. root_path/compile_commands.json
    3. root_path/build/compile_commands.json
    4. root_path/builddir/compile_commands.json
    5. Recursive search in subdirectories
    """
    root = Path(root_path)
    if custom_path:
        custom = Path(custom_path)
        check_path = custom if custom.is_absolute() else root / custom
        if check_path.is_file():
            return str(check_path.absolute())
        log.warning("Specified compile_commands.json not found: %s", custom_path)
        return None

    search_paths = [
        root / "compile_commands.json",
        root / "build" / "compile_commands.json",
        root / "builddir" / "compile_commands.json",
    ]

    for path in search_paths:
        if path.is_file():
            return str(path.absolute())

    # Recursive search for projects in subdirectories
    for root, dirs, files in os.walk(root_path):
        dirs[:] = [
            d
            for d in dirs
            if not d.startswith(".") and d not in ("node_modules", "__pycache__")
        ]
        if "compile_commands.json" in files:
            return str((Path(root) / "compile_commands.json").absolute())

    return None


def parse_compile_commands(
    path: str, root_path: str | None = None
) -> CompileCommandsConfig:
    """Parse compile_commands.json and extract configuration."""
    config = CompileCommandsConfig()
    path_obj = Path(path)

    try:
        with path_obj.open(encoding="utf-8") as f:
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
        build_dir = path_obj.parent
        build_dir_str = "" if build_dir == Path(".") else str(build_dir)
        for root, _, files in os.walk(root_path):
            if build_dir_str and root.startswith(build_dir_str):
                continue
            for f in files:
                if "-pp.f90" in f.lower() or "-pp.f" in f.lower():
                    continue
                if f.endswith(
                    (
                        ".f90",
                        ".F90",
                        ".f",
                        ".F",
                        ".f95",
                        ".F95",
                        ".f03",
                        ".F03",
                        ".f08",
                        ".F08",
                    )
                ):
                    workspace_files[f] = str(Path(root) / f)

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


def _normalize_source_path(
    source_file: str,
    directory: str,
    root_path: str | None,
    workspace_files: dict[str, str],
) -> str | None:
    """Normalize source file path, falling back to basename lookup."""
    if not source_file:
        return None

    source_path = Path(source_file)
    if not source_path.is_absolute():
        source_path = Path(directory) / source_path

    source_path = _normpath(source_path)

    if root_path and not source_path.exists():
        basename = source_path.name
        if basename in workspace_files:
            source_path = Path(workspace_files[basename])

    return str(source_path)


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
                # Parse define: NAME or NAME=VALUE
                if "=" in define:
                    name, value = define.split("=", 1)
                    name = name.strip()
                else:
                    name, value = define.strip(), "True"
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
    resolved = Path(path)
    if not resolved.is_absolute():
        resolved = Path(directory) / resolved
    return str(_normpath(resolved))
