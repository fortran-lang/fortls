from __future__ import annotations


class DebugError(Exception):
    """Base class for debug CLI."""


class ParameterError(DebugError):
    """Exception raised for errors in the parameters."""


class ParserError(Exception):
    """Parser base class exception"""


class FortranFileNotFoundError(ParserError, FileNotFoundError):
    """File not found"""
