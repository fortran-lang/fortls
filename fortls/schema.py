from __future__ import annotations

import json
import pathlib

from pydantic import Field, create_model

from fortls.interface import cli


def create_schema(root: pathlib.Path | None = None):
    if not root:
        root = pathlib.Path(__file__).parent

    parser = cli("fortls")
    only_vals = {}
    for arg in parser._actions:
        if (
            arg.dest == "help"
            or arg.dest == "version"
            or arg.help == "==SUPPRESS=="
            or (arg.dest.startswith("debug") and arg.dest != "debug_log")
        ):
            continue
        val = arg.default
        desc: str = arg.help.replace("%(default)s", str(val))  # type: ignore
        only_vals[arg.dest] = (type(val), Field(val, description=desc))  # type: ignore

    m = create_model("fortls schema", **only_vals)
    m.__doc__ = "Schema for the fortls Fortran Language Server"

    with open(str(root / "fortls.schema.json"), "w", encoding="utf-8") as f:
        print(json.dumps(m.model_json_schema(), indent=2), file=f)
    print(f"Created schema file: {root / 'fortls.schema.json'}")


if __name__ == "__main__":
    create_schema()
