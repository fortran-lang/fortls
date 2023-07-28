from __future__ import annotations


def range_json(sln: int, sch: int, eln: int = None, ech: int = None):
    return {
        "range": {
            "start": {"line": sln, "character": sch},
            "end": {"line": eln if eln else sln, "character": ech if ech else sch},
        }
    }


def diagnostic_json(sln: int, sch: int, eln: int, ech: int, msg: str, sev: int):
    return {**range_json(sln, sch, eln, ech), "message": msg, "severity": sev}


def uri_json(uri: str, sln: int, sch: int, eln: int = None, ech: int = None):
    return {"uri": uri, **range_json(sln, sch, eln, ech)}


def location_json(uri: str, sln: int, sch: int, eln: int = None, ech: int = None):
    return {"location": uri_json(uri, sln, sch, eln, ech)}


def symbol_json(
    name: str,
    kind: int,
    uri: str,
    sln: int,
    sch: int,
    eln: int = None,
    ech: int = None,
    container_name: str = None,
):
    if container_name:
        return {
            "name": name,
            "kind": kind,
            **location_json(uri, sln, sch, eln, ech),
            "containerName": container_name,
        }
    return {"name": name, "kind": kind, **location_json(uri, sln, sch, eln, ech)}


def change_json(new_text: str, sln: int, sch: int, eln: int = None, ech: int = None):
    return {**range_json(sln, sch, eln, ech), "newText": new_text}
