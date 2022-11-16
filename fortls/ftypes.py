from __future__ import annotations

from dataclasses import dataclass, field
from typing import NamedTuple

#: A single line range tuple
Range = NamedTuple("Range", [("start", int), ("end", int)])


@dataclass
class VarInfo:
    """Holds information about a Fortran VARIABLE"""

    var_type: str  #: Type of variable e.g. ``INTEGER``, ``REAL``, etc.
    #: keywords associated with this variable e.g. SAVE, DIMENSION, etc.
    keywords: list[str]  #: Keywords associated with variable
    var_names: list[str]  #: Variable names
    #: Kind of variable e.g. ``INTEGER*4`` etc.
    var_kind: str | None = field(default=None)


@dataclass
class SelectInfo:
    """Holds information about a SELECT construct"""

    type: int  #: Type of SELECT e.g. normal, select type, select kind, select rank
    binding: str  #: Variable/Object being selected upon
    desc: str  #: Description of select e.g. "TYPE", "CLASS", None


@dataclass
class ClassInfo:
    """Holds information about a Fortran CLASS"""

    name: str  #: Class name
    parent: str  #: Parent object of class e.g. ``TYPE, EXTENDS(scaled_vector) :: a``
    keywords: list[str]  #: Keywords associated with the class


@dataclass
class UseInfo:
    """Holds information about a Fortran USE statement"""

    mod_name: str  #: Module name
    #: List of procedures, variables, interfaces, etc. imported via only
    only_list: set[str]
    #: A dictionary holding the new names after a rename operation
    rename_map: dict[str, str]


@dataclass
class GenProcDefInfo:
    """Holds information about a GENERIC PROCEDURE DEFINITION"""

    bound_name: str  #: Procedure name
    pro_links: list[str]  #: Procedure links
    vis_flag: int  #: Visibility flag, public or private


@dataclass
class SmodInfo:
    """Holds information about Fortran SUBMODULES"""

    name: str  #: Submodule name
    parent: str  #: Submodule i.e. module, parent


@dataclass
class InterInfo:
    """Holds information about a Fortran INTERFACE"""

    name: str  #: Interface name
    abstract: bool  #: Whether or not the interface is abstract


@dataclass
class VisInfo:
    """Holds information about the VISIBILITY of a module's contents"""

    type: int  #: Visibility type 0: PUBLIC 1: PRIVATE TODO: convert to boolean
    obj_names: list[str]  #: Module variables, procedures, etc. with that visibility


@dataclass
class IncludeInfo:
    """Holds information about a Fortran INCLUDE statement"""

    line_number: int  #: Line number of include
    path: str  #: File path to include
    file: None  # fortran_file  #: fortran_file object
    scope_objs: list[str]  #: A list of available scopes


@dataclass
class SubInfo:
    """Holds information about a Fortran SUBROUTINE"""

    name: str  #: Procedure name
    args: str  #: Argument list
    #: Keywords associated with procedure
    keywords: list[str] = field(default_factory=list)
    #: Whether or not this is a ``MODULE PROCEDURE``
    mod_flag: bool = field(default=False)


@dataclass
class ResultSig:
    """Holds information about the RESULT section of a Fortran FUNCTION"""

    name: str | None = field(default=None)  #: Variable name of result
    type: str | None = field(default=None)  #: Variable type of result
    kind: str | None = field(default=None)  #: Variable kind of result
    #: Keywords associated with the result variable, can append without init
    keywords: list[str] = field(default_factory=list)


@dataclass
class FunSig(SubInfo):
    """Holds information about a Fortran FUNCTION"""

    #: Function's result with default ``result.name = name``
    result: ResultSig = field(default_factory=ResultSig)

    def __post_init__(self):
        if not self.result.name:
            self.result.name = self.name
