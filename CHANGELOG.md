# CHANGELOG

## Unreleased

## 3.1.0

### Fixed

- Fixed bug where parser would crash when trying to retrieve an invalid line no.
  ([#398](https://github.com/fortran-lang/fortls/issues/398))
- Fixed bug with string quotes not being escaped when looking for parenthesis
  ([#250](https://github.com/fortran-lang/fortls/issues/250))
- Fixed bug with line continuations in lexical tokens
  ([#235](https://github.com/fortran-lang/fortls/issues/235))

## 3.0.0

### Added

- Added support for changing the default Python recursion depth
  ([#312](https://github.com/fortran-lang/fortls/issues/312))
- Added support for preprocessor macro expansions
  ([#368](https://github.com/fortran-lang/fortls/pull/368))
- Added support for leading white spaces in preprocessor directives
  ([#297](https://github.com/fortran-lang/fortls/issues/297))
- Added hover messages for Types and Modules
  ([#208](https://github.com/fortran-lang/fortls/issues/208))
- Added support for Markdown intrinsics from the M_intrinsics repository
  ([#215](https://github.com/fortran-lang/fortls/issues/215))
- Added and create a schema for fortls configuration files
  ([#204](https://github.com/fortran-lang/fortls/issues/204))
- Added dependabot alers for PyPi
  ([#222](https://github.com/fortran-lang/fortls/issues/222))
- Added `CITATION.cff` file to project roots

### Changed

- Changed `--incl_suffixes` option to faithfully match the suffixes that are
  provided in the option, without performing any type of modification.
  ([#300](https://github.com/fortran-lang/fortls/issues/300))
- Changed the completion signature to include the full Markdown documentation
  for the completion item.
  ([#219](https://github.com/fortran-lang/fortls/issues/219))
- Changed hover messages and signature help to use Markdown
  ([#45](https://github.com/fortran-lang/fortls/issues/45))
- Changed automatic detection of fixed/free-form of files to ignore
  preprocessor lines.
  ([#302](https://github.com/fortran-lang/fortls/pull/302))
- Moved project setup from `setup.cfg` to `pyproject.toml`
  ([#384](https://github.com/fortran-lang/fortls/pull/384))
- Bumped `setuptools` version to `>=61.0.0`
  ([#384](https://github.com/fortran-lang/fortls/pull/384))

### Fixed

- Fixed end of scope errors raised by trailing semicolon in native parser
  ([#265](https://github.com/fortran-lang/fortls/issues/265))
- Fixed bug where parent scope for includes in AST could be `None`
  ([#329](https://github.com/fortran-lang/fortls/issues/329))
- Fixed preprocessor bug with `if` and `elif` conditionals
  ([#322](https://github.com/fortran-lang/fortls/issues/322))
- Fixed bug where type fields or methods were not detected if spaces were
  used around `%`
  ([#286](https://github.com/fortran-lang/fortls/issues/286))
- Fixed bug where Go To Implementation would not work for submodules
  ([#74](https://github.com/fortran-lang/fortls/issues/74))
- Fixed bug where `associate` blocks for variables pointing to function results
  where not properly resolved
  ([#269](https://github.com/fortran-lang/fortls/issues/269))
- Fixed bug where the `langid` was not propagated correctly from the user
  settings to the LSP creation stage for all types of requests.
  ([#257](https://github.com/fortran-lang/fortls/issues/257))
- Fixed end of scope for `CRITICAL` keyword blocks
  ([#255](https://github.com/fortran-lang/fortls/issues/255))
- Fixed bug where completion of interfaces in USE ONLY would produce the snippet
  ([#150](https://github.com/fortran-lang/fortls/issues/150))
- Fixed bug where diagnostic messages were raised for non-existent variables
  ([#173](https://github.com/fortran-lang/fortls/issues/173))
  ([#175](https://github.com/fortran-lang/fortls/issues/175))
- Fixed submodule crashing bug and document/Symbol request failure
  ([#233](https://github.com/fortran-lang/fortls/issues/233))
- Fixed debug interface parser not loading all configuration files
  ([#221](https://github.com/fortran-lang/fortls/issues/221))
- Fixed name mangling of type-bound procedure pointers while hovering
  ([#214](https://github.com/fortran-lang/fortls/issues/214))
- Fixed parsing start of multilines into AST
  ([#217](https://github.com/fortran-lang/fortls/issues/217))

### Removed

- Removed `setuptools_scm_git_archive` due to package deprecation
  ([#326](https://github.com/fortran-lang/fortls/issues/326))

## 2.13.0

### Added

- Added additional default configuration file names `.fortlsrc`, `.fortls.json`
  ([#184](https://github.com/fortran-lang/fortls/issues/184))
- Added coverage testing for multiple Python versions
  ([#168](https://github.com/fortran-lang/fortls/pull/178))
- Added pre-commit.ci to `fortls`
  ([#168](https://github.com/fortran-lang/fortls/issues/168))

### Fixed

- Fixed `intent(in out)` not being parsed correctly
  ([#160](https://github.com/fortran-lang/fortls/issues/160))

## 2.12.0

### Fixed

- Fixed issue where `pip` would install without `--user` and fail
  ([#163](https://github.com/fortran-lang/fortls/issues/163))

## 2.11.0

### Fixed

- Fixed bug thorowing diagnostic errors if arguments were named `pure`, `elemental`, etc.
  ([#157](https://github.com/fortran-lang/fortls/issues/157))

## 2.10.0

### Fixed

- Fixed `workspace/didChangeConfiguration` requests that caused errors in VS Code logs
  ([#114](https://github.com/fortran-lang/fortls/issues/114))

## 2.9.0

### Fixed

- Fixed glob pattern resolution for command line arguments
  ([#142](https://github.com/fortran-lang/fortls/issues/142))

### Changed

- Changed the default value of the following options when a mix the command line
  interface and the `json` interface are used. Instead of having the `json` interface
  default the values to an empty set it now defaults to the values of the
  command line: `excl_paths`, `source_dirs`, `incl_suffixes`, `excl_suffixes`, `include_dirs`
  ([#143](https://github.com/fortran-lang/fortls/issues/143))

## 2.8.0

### Added

- Added support for comments in the `json` configuration files
  ([#137](https://github.com/fortran-lang/fortls/issues/137))
- Added `sitemap.xml` to documentation webpage
  ([#134](https://github.com/fortran-lang/fortls/pull/134))

### Fixed

- Fixed bug where error messages did not post correctly
  ([#135](https://github.com/fortran-lang/fortls/issues/135))

## 2.7.0

### Added

- Added doctests in the pytest test suite
  ([#131](https://github.com/fortran-lang/fortls/issues/131))

### Changed

- Renamed variables to simplify parser
  ([#133](https://github.com/fortran-lang/fortls/pull/133))
- Redesigned parsing functions for short-hand declarations of array dimensions,
  character length and parsing of kind
  ([#130](https://github.com/fortran-lang/fortls/pull/130))

## 2.6.0

### Changed

- Redesigned the `fortls` website to be more aesthetically pleasing and user-friendly
  ([#112](https://github.com/fortran-lang/fortls/issues/112))

### Fixed

- Fixed bug where submodule procedure scopes would terminate early if keyword modifiers were used
  ([#119](https://github.com/fortran-lang/fortls/issues/119))

## 2.5.0

### Added

- Added `textDocument/documentHighlight` request for Vim and EMACS
  ([#43](https://github.com/fortran-lang/fortls/issues/43))
- Added `pre-commit` hook with `flake`, `black`, `isort` and `pyupgrade`
  ([#106](https://github.com/fortran-lang/fortls/issues/106))
- Added test support for diagnostic messages

### Changed

- Changed code structure, reformatted files and changed name conventions
  ([#109](https://github.com/fortran-lang/fortls/issues/109))
- Updated CONTRIBUTING instructions

## 2.4.0

### Added

- Added multiple unittests for diagnostic messages
- Added `pre-commit` hook to the project
  ([#106](https://github.com/fortran-lang/fortls/issues/106))
- Added Code of Conduct
- Added basic support for hovering over `ASSOCIATE` blocks
  ([#62](https://github.com/fortran-lang/fortls/issues/62))

### Changed

- Changed the naming convention for Fortran Objects
  ([#109](https://github.com/fortran-lang/fortls/issues/109))
- Formatted all files with `pre-commit`

## 2.3.1

### Fixed

- Fixed remote coverage report line diff
  ([#101](https://github.com/fortran-lang/fortls/issues/101))

## 2.3.0

### Added

- Added keywords to the PyPi manifest
  ([#99](https://github.com/fortran-lang/fortls/issues/99))

### Changed

- Updated `README` to include logo and animations
- Updated `README` to include conda-forge installation instructions

## 2.2.14

### Added

- Added unittests for intrinsics and improved overall coverage

### Changed

- Restructured unittests to individual files for more granular reporting

## 2.2.13

### Added

- Automated the update for GitHub Actions

## 2.2.12

### Added

- Added coverage reporting for Unix + Windows

## 2.2.11

### Added

- Improved autocompletion for Fortran statements F2018 compliant
  ([#63](https://github.com/fortran-lang/fortls/issues/63))

## 2.2.10

### Fixed

- Fixes GoTo Implementation error for intrinsics
  ([#80](https://github.com/fortran-lang/fortls/issues/80))

## 2.2.9

### Changed

- Changed how renaming of implicitly named type-bound procedures and their
  implementations is handled. Unittest was added.
- Rewrote the Fortran parser to be clearer and more modular

## 2.2.8

### Changed

- Disable PyPi autoupdating for pre-releases and dev versions

## 2.2.7

### Changed

- Changed the auto-update feature to skip `anaconda` environments since they
  handle their dependencies through `conda` and not `pip`

### Fixed

- Fixed missing dependency from `setup.cfg`
  ([#78](https://github.com/fortran-lang/fortls/issues/78))
- Updated configuration file variables in documentation

## 2.2.6

### Added

- Added the capability for `fortls` to auto-update use `--disable_autoupdate` to disable
  ([#76](https://github.com/fortran-lang/fortls/issues/76))

## Deprecated

- Deprecated `--variable_hover` option and now is always enabled
  ([#46](https://github.com/fortran-lang/fortls/issues/46))

## 2.2.5

### Changed

- Updated `setup.cfg` in preparation of submitting package to `conda-forge`
- Added `Editor Integration` section in documentation

### Fixed

- Fixed parsing of `defined` without by parenthesis surrounding the definition
  ([#67](https://github.com/fortran-lang/fortls/pull/67))

## 2.2.4

### Fixed

- Fixed hovering value of literal and constant strings not displaying
  ([#54](https://github.com/fortran-lang/fortls/issues/54))
- Fixed hovering string length size is now matching the string
  ([#55](https://github.com/fortran-lang/fortls/issues/55))
- Fixed space separated keywords not being displayed upon hover
  ([#60](https://github.com/fortran-lang/fortls/issues/60))

## 2.2.3

### Changed

- Changed reading in `json` files to be encoded as UTF-8
  ([#51](https://github.com/fortran-lang/fortls/pull/51))

## 2.2.2

### Changed

- Changed the way function hover messages are displayed, now signatures are standardised
  ([#47](https://github.com/fortran-lang/fortls/issues/47))

### Fixed

- Fixed hovering over functions displaying as theire result types
  ([#22](https://github.com/fortran-lang/fortls/issues/22))
- Fixed function modifiers not displaying upon hover
  ([#48](https://github.com/fortran-lang/fortls/issues/48))
- Fixed function hover when returning arrays
  ([#50](https://github.com/fortran-lang/fortls/issues/50))

## 2.2.1

### Changed

- Changed default branch to master and updated Actions workflows

## 2.2.0

### Added

- Added semantic versioning with `setuptools_scm`
  ([#34](https://github.com/fortran-lang/fortls/issues/34))

### Changed

- Changes from `setup.py` to `setup.toml` and `pyproject.toml`
  ([#33](https://github.com/fortran-lang/fortls/issues/33))
- Changed documentation CI to up to date action
- Formatted Python imports with `isort`

## 2.1.2

### Fixed

- Fixed code autocompletion bug with f-strings
  ([#39](https://github.com/hansec/fortran-language-server/issues/39))

## 2.1.1

### Added

- Added additional shields to REAMDE, including coverage and Python versions

## 2.1.0

### Added

- Added coverage metric for Codecov
- Added coverage for `WHERE`, `ENUM`, max line/comment diagnostics and multilines
- Adds Windows CI

### Fixed

- Fixed global `sort_keywords` option not propagating during parsing on Windows
  ([#36](https://github.com/fortran-lang/fortls/issues/36))
- Fixed unittests not propagating debugger state
  ([#35](https://github.com/fortran-lang/fortls/issues/35))

## 2.0.1

### Added

- Add support for absolute include, source and exclude paths

### Changed

- Changed `USE_info` named tuple to storing use modules as `sets` instead of `lists`
- Changed `include_dirs` from a `list` to a `set`
- Automates the versioning with GitHub releases

### Fixed

- Fixed some mutable default argument warnings in methods and classes
- Fixed private variables showing in autocomplete
  ([#191](https://github.com/hansec/fortran-language-server/issues/191))
  ([#3](https://github.com/fortran-lang/fortls/issues/3))

## 2.0.0

### Added

- Adds support for including preprocessor definitions from files same as `pp_defs`
- Adds hover support for preprocessor variables
- Adds Go To Definition for `include` statements
- Adds intrinsic support for `OpenACC` version 3.1
- Adds sphinx autogenerated documentation
- Adds `incl_suffixes` as a configuration option
- Adds `EXTERNAL` as an attribute upon hover

### Changed

- Update constant parameters for `omp_lib` and `omp_lib_kinds` Interface v5.0
- Format json files with `prettier`
- Initialises the log channel and adds `$/setTrace` to override client's (i.e. VS Code) loglevel
- Unified the interfaces from the command line and the configuration options file
  ([#17](https://github.com/fortran-lang/fortls/issues/17))
- Updated the documentation and simplified the README.md

### Deprecated

- Option `--preserve_keyword_order` has been substituted with its opposite `--sort_keywords`

### Fixed

- Fixes the hover of preprocessor functions. It now displays the function name
  witout the argument list and the function body. The argument list cannot be
  multiline but the function body can.
- Fixes objects marked `EXTERNAL` across multiple lines
  ([#169](https://github.com/hansec/fortran-language-server/issues/169))
- Fixes include with external files
  ([#13](https://github.com/fortran-lang/fortls/issues/13))
- `POINTER` attribute now displays upon hover
  ([#16](https://github.com/fortran-lang/fortls/issues/16))
- Fixes `END FORALL` end of scope error
  ([#18](https://github.com/fortran-lang/fortls/issues/18))
- Fixes Fortran line continuation definitions intermingled with preprocessor directives
  ([#203](https://github.com/hansec/fortran-language-server/issues/203))
  ([#4](https://github.com/fortran-lang/fortls/issues/4))
- Fixes `USE` directive ordering issues
  ([#184](https://github.com/hansec/fortran-language-server/issues/184))
  ([#7](https://github.com/fortran-lang/fortls/issues/7))

## 1.16.0

### Added

- Adds value for `PARAMETER` variables on hover
  ([#116](https://github.com/hansec/fortran-language-server/issues/116))
  ([#1](https://github.com/fortran-lang/fortls/issues/1))

## 1.15.2

### Fixed

- Further improves the literal variable hover added in v1.14.0

## 1.15.1

### Fixed

- Fixes premature end of scope with variables named `end`
  ([#9](https://github.com/fortran-lang/fortls/issues/9))

## 1.15.0

### Added

- Adds `--config` option which allows arbitrary named configuration files

## 1.14.4

### Fixed

- Fixes import host association includes (autocomplete work not complete)
  ([#187](https://github.com/hansec/fortran-language-server/issues/187))

## 1.14.3

### Fixed

- Fixes parsing of `non_intrinsic` modules
  ([#206](https://github.com/hansec/fortran-language-server/issues/206))

## 1.14.2

### Fixed

- Fixes error while parsing submodule parent name with spaces
  ([#207](https://github.com/hansec/fortran-language-server/issues/207))

## 1.14.1

### Fixed

- Fixes language server features not triggering for variables in column 0

## 1.14.0

### Fixed

- Fixes (partially) Fortran literal variable hover
  ([#188](https://github.com/hansec/fortran-language-server/issues/188))

## 1.13.0

### Added

- Adds Python glob support for `excl_paths`, `source_dirs`, `include_dirs`

## 1.12.1

### Fixed

- Fixes diagnostic error with interfaces as function arguments
  ([#200](https://github.com/hansec/fortran-language-server/issues/200))

## 1.12.0

### Changed

- Add support for disabling diagnostics globally or on a per-project basis, ref [PR 163](https://github.com/hansec/fortran-language-server/pull/163)

### Fixed

- Fix bug with enum declarations, fixes [#167](https://github.com/hansec/fortran-language-server/issues/167)
- Fix typo in "ISHIFT" and "ISHIFTC" intrinsic functions, ref [PR 165](https://github.com/hansec/fortran-language-server/pull/165)

## 1.11.1

### Fixed

- Fix bug with hover requests introduced in v1.11.0, fixes [#159](https://github.com/hansec/fortran-language-server/issues/159)

## 1.11.0

### Changed

- Add support for specifying the language name returned for hover requests, ref [Fortran IntelliSense #17](https://github.com/hansec/vscode-fortran-ls/issues/17)
- Add support for submodule implementations using the "PROCEDURE" keyword, fixes [#152](https://github.com/hansec/fortran-language-server/issues/152)

### Fixed

- Fix bug with keywords in old style function declarations, fixes [#154](https://github.com/hansec/fortran-language-server/issues/154)
- Fix bug when searching an empty scope, fixes [#151](https://github.com/hansec/fortran-language-server/issues/151)
- Remove erroneous double definition/masking checks for interfaces, fixes [#18](https://github.com/hansec/fortran-language-server/issues/18) and [#138](https://github.com/hansec/fortran-language-server/issues/138)
- README: Add fix for possible installation error

## 1.10.3

### Fixed

- Fix parsing bug with spaces in "old-style" kind specifications, fixes [#142](https://github.com/hansec/fortran-language-server/issues/142)
- Fix issue with erroneous sub-word matching in preprocessor macro substitutions, fixes [#141](https://github.com/hansec/fortran-language-server/issues/141)

## 1.10.2

### Changed

- Add support for "old-style" character length specification, fixes [#130](https://github.com/hansec/fortran-language-server/issues/130) and [#134](https://github.com/hansec/fortran-language-server/issues/134)

### Fixed

- Fix "can't set attribute" error in USE traversal, fixes [#132](https://github.com/hansec/fortran-language-server/issues/132)
- Fix bugs related to optional leading ampersands on continuation lines, fixes [#131](https://github.com/hansec/fortran-language-server/issues/131)
- Fix bug in block parsing with string literals, fixes [#128](https://github.com/hansec/fortran-language-server/issues/128)

## 1.10.1

### Fixed

- Fix bug in semicolon parsing, fixes [#127](https://github.com/hansec/fortran-language-server/issues/127)

## 1.10.0

### Changed

- Initial implementation of preprocessor include file handling, ref [#115](https://github.com/hansec/fortran-language-server/issues/115)
- Add support for specifying file suffixes for preprocessing, ref [#115](https://github.com/hansec/fortran-language-server/issues/115)
- Add support for completion in visibility statements, fixes [#120](https://github.com/hansec/fortran-language-server/issues/120)
- Support "onOpen" requests before a file is written to disk, fixes [#123](https://github.com/hansec/fortran-language-server/issues/123)
- Add support for IMPURE keyword (contributed by @mcocdawc)
- Improve readability by replacing various result arrays with namedtuples

### Fixed

- Fix bug in open string literal detection, fixes [#124](https://github.com/hansec/fortran-language-server/issues/124)
- Fix bug with multiline docstrings that start with a trailing comment, fixes [#118](https://github.com/hansec/fortran-language-server/issues/118)
- Fix symbols types for subroutines and methods in "documentSymbol" and "completion" requests, fixes [#117](https://github.com/hansec/fortran-language-server/issues/117)
- Fix bug where ONLY renaming was not fully tracked in some circumstances
- Fix bug with inline dimension specifications for variables
- Fix accidental message swap in "object not found" and "object not imported" diagnostics
- Fix bug where errors were reported with "module subroutine" and "module function" definitions (no import required)

## 1.9.1

### Fixed

- Fix bug in USE ONLY accounting used for graph pruning, fixes [#122](https://github.com/hansec/fortran-language-server/issues/122)

## 1.9.0

### Changed

- Add support for USE statement renaming requests, ref [#109](https://github.com/hansec/fortran-language-server/issues/109)
- Add support for argument information in variable hover requests, fixes [#107](https://github.com/hansec/fortran-language-server/issues/107)
- Add support for disabling snippets in autocomplete results, fixes [#112](https://github.com/hansec/fortran-language-server/issues/112)
- Prevent file AST updates on Open/Close requests when contents have not changed, ref [#105](https://github.com/hansec/fortran-language-server/issues/105)
- Reduce unnecessary parsing with single line file changes
- Debugging: Add support for printing full result object

### Fixed

- Remove required space between "DOUBLE PRECISION" and "DOUBLE COMPLEX" definitions, fixes [#110](https://github.com/hansec/fortran-language-server/issues/110)
- Fix requests when a user-defined type variable has the same name as a defined type used in that scope

## 1.8.2

### Fixed

- Fix parsing single line WHERE statements with trailing parentheses, fixes [#106](https://github.com/hansec/fortran-language-server/issues/106)
- Fix erroneous object duplication diagnostics for DO, INTERFACE, etc. blocks
- Remove erroneous "unimplemented procedure" diagnostics from abstract type definitions
- Fix parsing bugs with semicolons in trailing comments

## 1.8.1

### Fixed

- Fix bug with requests in lines with tab characters, fixes [#93](https://github.com/hansec/fortran-language-server/issues/93)
- Fix bug with requests following "WRITE(\*,\*)" statements

## 1.8.0

### Changed

- Add full support for ASSOCIATE statements, fixes [#101](https://github.com/hansec/fortran-language-server/issues/101)
- Filter completion suggestions after "MODULE PROCEDURE" statements, fixes [#103](https://github.com/hansec/fortran-language-server/issues/103)
- Filter completion suggestions in type-bound procedure links
- Add support for including external source file directories
- Diagnostics: Line length exceeds maximum length errors
- Speedup language server initialization
- Speedup "textDocument/references" requests

## 1.7.3

### Fixed

- Fix case preservation in hover requests, fixes [#102](https://github.com/hansec/fortran-language-server/issues/102)
- Fix rename requests for type-bound procedures without an explicit link statement (ie. "=>"), fixes [#104](https://github.com/hansec/fortran-language-server/issues/104)
- Fix incorrect "CONTAINS" diagnostic errors with procedure pointers and external interfaces
- Fix bug in diagnostic construction/reporting (introduced in v1.7)
- Fix bugs caused by accidental modification of child object lists

## 1.7.2

### Fixed

- Fix bug with definition/hover requests involving intrinsic functions/modules/variables (introduced in v1.7)

## 1.7.1

### Fixed

- Fix bug with completion and signatureHelp requests on continuation lines (introduced in v1.7)
- Fix out-of-range error with various requests on zero-length lines (introduced in v1.7)

## 1.7.0

### Changed

- Add initial support for "textDocument/codeAction" requests, generate unimplemented deferred procedures
- Show subroutine/function keywords ("PURE", "ELEMENTAL", etc.)
- Add position of object in line to "textDocument/definition" and "textDocument/implementation" results
- Diagnostics: CONTAINS statement placement errors
- Diagnostics: Visibility statement placement errors
- Command line options: Notify when workspace initialization is complete
- Command line options: Set number of threads used during initialization
- Significant refactoring of core code

### Fixed

- Fix "RecursionError" exception with circular user-defined type references, fixes [#100](https://github.com/hansec/fortran-language-server/issues/100)
- Fix bug detecting TYPE definitions with an immediately following colon, ref [#100](https://github.com/hansec/fortran-language-server/issues/100)
- Fix incorrect diagnostics for interface statements with USE instead of IMPORT statements

## 1.6.0

### Changed

- Add support for EXTERNAL subroutines
- Diagnostics: Missing subroutine/function arguments and argument declarations
- Diagnostics: Unimplemented deferred type-bound procedures
- Diagnostics: Unknown TYPE/KIND objects (only if candidate is visible in workspace)
- Diagnostics: IMPORT statements (missing objects and placement)
- Diagnostics: Basic handling for IMPLICIT statements

## 1.5.1

### Changed

- Add support for semicolon separators and multiline preprocessor macros, fixes [#98](https://github.com/hansec/fortran-language-server/issues/98)
- Add various "parsing errors" to debug_parser output

### Fixed

- Use consistent file access method across debug_parser run and language server requests

## 1.5.0

### Changed

- Add support for "textDocument/rename" requests
- Add initial support for Doxygen and FORD style comment blocks, ref [#44](https://github.com/hansec/fortran-language-server/issues/44)

### Fixed

- Fix language server crash with unknown user-defined type fields

### Other changes

- Deprecate "mod_dirs" option in favor of more accurate "source_dirs". Support for "mod_dirs" will be removed in a future release.

## 1.4.0

### Changed

- Add support for "textDocument/implementation" requests, ref [#94](https://github.com/hansec/fortran-language-server/issues/94)
- Add option to preserve keyword ordering, ref [#97](https://github.com/hansec/fortran-language-server/issues/97)

### Fixed

- Fix parsing bug with single line WHERE statements, fixes [#92](https://github.com/hansec/fortran-language-server/issues/92)
- Fix bug with keyword parsing with nested parenthesis, fixes [#97](https://github.com/hansec/fortran-language-server/issues/97)
- Differentiate between type-bound procedures and implementations in "textDocument/references" requests, fixes [#94](https://github.com/hansec/fortran-language-server/issues/94)
- Fix typos in MAX and MIN intrinsic functions, ref [#96](https://github.com/hansec/fortran-language-server/pull/96)

## 1.3.0

### Changed

- Add support for user-defined type members in "textDocument/references" requests, fixes [#88](https://github.com/hansec/fortran-language-server/issues/88)
- Link type-bound procedures with no explicit link to matching named scope in module, fixes [#89](https://github.com/hansec/fortran-language-server/issues/89)
- Report diagnostics related to misplaced "CONTAINS" statements
- Restructure README for improved clarity on capabilities/limitations

### Fixed

- Fix bug with blank/empty lines in free-format continuations, fixes [#91](https://github.com/hansec/fortran-language-server/issues/91)
- Fix exception in "textDocument/references" requests when no object is found, fixes [#86](https://github.com/hansec/fortran-language-server/issues/86)
- Fix bug when relative path is used for --debug_rootpath, fixes [#87](https://github.com/hansec/fortran-language-server/issues/87)

## 1.2.1

### Fixed

- Fix bug in nested user-defined type inheritance, fixes [#85](https://github.com/hansec/fortran-language-server/issues/85)
- Fix bug in completion requests with empty parenthesis in request line

## 1.2.0

### Changed

- Add support for local variables/objects in "textDocument/references" requests, ref [#84](https://github.com/hansec/fortran-language-server/issues/78)
- Improve preprocessing to handle more types of conditional statements and macro substitution, ref [#78](https://github.com/hansec/fortran-language-server/issues/78)
- Report diagnostics for excess "END" statements instead of causing parser failure, ref [#78](https://github.com/hansec/fortran-language-server/issues/78)

### Fixed

- Fix missing "textDocument/references" results when line starts with target object, fixes [#84](https://github.com/hansec/fortran-language-server/issues/84)

## 1.1.1

### Fixed

- Fix bug with backslash URI separators on Windows, fixes [#83](https://github.com/hansec/fortran-language-server/issues/83)

## 1.1.0

### Changed

- Add initial implementation of simple preprocessor, ref [#78](https://github.com/hansec/fortran-language-server/issues/78)

### Fixed

- Updated Fixed/Free detection logic using ampersands to check for comment line, fixes [#81](https://github.com/hansec/fortran-language-server/issues/81)
- Support use of "END" as a variable, fixes [#82](https://github.com/hansec/fortran-language-server/issues/82)

## 1.0.5

### Fixed

- Add support for named "SELECT" statements, fixes [#80](https://github.com/hansec/fortran-language-server/issues/80)
- Track scopes for "ASSIGNMENT" and "OPERATOR" interface statements, fixes [#79](https://github.com/hansec/fortran-language-server/issues/79)
- Fix bug in parsing "SELECT" statements with no space, fixes [#77](https://github.com/hansec/fortran-language-server/issues/77)
- Further improve discrimination between end statements and other items, ref [#73](https://github.com/hansec/fortran-language-server/issues/73)

## 1.0.4

### Fixed

- Normalize file paths when storing/accessing file index, fixes [#75](https://github.com/hansec/fortran-language-server/issues/75)
- Fix intrinsic statement "COUNT" ([#76](https://github.com/hansec/fortran-language-server/pull/76))

## 1.0.3

### Fixed

- Further improve discrimination between end statements and variables/block labels, ref [#73](https://github.com/hansec/fortran-language-server/issues/73)
- Fix autocomplete errors when ASSOCIATE and ENUM statements are present
- Fix severity reporting with "debug_diagnostics" command line option

## 1.0.2

### Fixed

- Fix discrimination between end statements and variables with underscores, fixes [#73](https://github.com/hansec/fortran-language-server/issues/73)
- Detect enum definitions, fixes [#74](https://github.com/hansec/fortran-language-server/issues/74)

## 1.0.1

### Fixed

- Detect and support associate statements, fixes [#72](https://github.com/hansec/fortran-language-server/issues/72)

## 1.0.0

### Changed

- Add parsing of DO/IF/WHERE blocks and report scope end errors
- Detect and report errors with invalid parent for scope definitions
- Improve highlighting for hover requests in VSCode
- Downgrade missing use warnings to information level
- Add intrinsic declaration statement "double complex" ([#70](https://github.com/hansec/fortran-language-server/pull/70))

### Fixed

- Fix bug with leading whitespace on visibility statements, fixes [#69](https://github.com/hansec/fortran-language-server/issues/69)
- Fix parsing errors when "&" and "!" characters are present inside string literals
- Fix parsing bug with multiple leading function/subroutine modifiers (PURE, ELEMENTAL, etc.)

## 0.9.3

### Fixed

- Fix detection of function definitions with leading module and variable statements, fixes [#66](https://github.com/hansec/fortran-language-server/issues/66)
- Properly close remaining open scopes at end of file
- Initialize scope "eline" property, [PR #67](https://github.com/hansec/fortran-language-server/pull/67)

## 0.9.2

### Changed

- Improve handling of different file encodings, [PR #57](https://github.com/hansec/fortran-language-server/pull/57)

### Fixed

- Fix autocomplete results for inherited members of user-defined types when the member type definition is only available in parent type's scope

## 0.9.1

### Changed

- Add support for generic interfaces in type-bound procedures, [#64](https://github.com/hansec/fortran-language-server/issues/64)
- Add parent scope information to masked variable errors, [#48](https://github.com/hansec/fortran-language-server/issues/48)

### Fixed

- Fix parsing deferred length character definitions, [#61](https://github.com/hansec/fortran-language-server/issues/61)
- Fix parsing function definitions with modifiers before type, [#63](https://github.com/hansec/fortran-language-server/issues/63)
- Fix parsing with array construction in subroutine/function calls, [#60](https://github.com/hansec/fortran-language-server/issues/60)

## 0.9.0

### Changed

- Add basic support for workspace/symbol requests
- Add support for excluding source files based on a common suffix

### Fixed

- Prevent detection of variables starting with "use" as USE statements, [#59](https://github.com/hansec/fortran-language-server/issues/59)
- Improve parsing of USE ONLY statements, [#53](https://github.com/hansec/fortran-language-server/issues/53)
- Make sure explicitly specified module directories exist, fixes [#52](https://github.com/hansec/fortran-language-server/issues/52)
- Fix visibility statements with trailing comments, [#49](https://github.com/hansec/fortran-language-server/issues/49)

## 0.8.4

### Fixed

- Check for existence of file during "textDocument/didClose" requests, [#46](https://github.com/hansec/fortran-language-server/issues/46)
- Encode text as UTF-8 in change requests, fixes [#41](https://github.com/hansec/fortran-language-server/issues/41)

## 0.8.3

### Changed

- Add support for generating debug logs
- Add Fortran statements to autocomplete suggestions
- Add support for explicit dimension specifications, fixes [#37](https://github.com/hansec/fortran-language-server/issues/37)

## 0.8.2

### Changed

- Add support for F03 style bracket array initialization, fixes [#35](https://github.com/hansec/fortran-language-server/issues/35)

## 0.8.1

### Fixed

- Fix crash in completion requests with intrinsic modules

## 0.8.0

### Changed

- Reformat completion information and snippets to match common language server conventions
- Provide hover information for overloaded interfaces
- Add support for autocompletion in select type statements
- Add support for type bound procedures with explicit pass statements
- Add support for arguments defined as interfaces in hover and signatureHelp requests
- Unbetafy signatureHelp support

### Fixed

- Fix linking type bound procedures with same name as subroutine/function definition

## 0.7.3

### Fixed

- Improve detection of block statements, fixes [#32](https://github.com/hansec/fortran-language-server/issues/32)
- Fix autocompletion with mixed case object definitions

## 0.7.2

### Fixed

- Fix variable definition detection without spaces, fixes [#30](https://github.com/hansec/fortran-language-server/issues/30)

## 0.7.1

### Changed

- Add option for displaying hover information for variables
- Add subroutine/function keywords to hover information
- Add more keywords to variable information
- Support spaces between subroutine name and parentheses in signatureHelp

### Fixed

- Fix bug with file paths that include spaces, fixes [#29](https://github.com/hansec/fortran-language-server/issues/29)
- Fix bug where arguments were erroneously dropped for procedure variables
- Fix bug where arguments of procedure type did not have definition information in subroutine/function hover results
- Correct spelling of incremental_sync argument, fixes [#28](https://github.com/hansec/fortran-language-server/issues/28)

## 0.7.0

### Changed

- Add support for signatureHelp requests with non-overloaded subroutines/functions
- Provide autocomplete and hover information for procedures with explicit interface definitions
- Add support for Fortran 2008 block constructs, fixes [#23](https://github.com/hansec/fortran-language-server/issues/23)
- Add support for "DOUBLE COMPLEX" datatype

### Fixed

- Fix bug where external interfaces were erroneously public in default private modules
- Fix bug producing repeated objects with include statements

## 0.6.2

### Changed

- Catch and report more types of errors related to file processing, fixes [#21](https://github.com/hansec/fortran-language-server/issues/21)

## 0.6.1

### Fixed

- Fix bug with incremental sync using VSCode on windows, fixes [#20](https://github.com/hansec/fortran-language-server/issues/20)

## 0.6.0

### Changed

- Add keywords to autocomplete results in variable definition statements
- Filter autocompletion results in extend, import, and procedure statements
- Ignore completion requests on scope definition and ending lines to reduce autocomplete noise
- Filter autocompletion results in variable definition statements to reduce autocomplete noise (variables only)
- Ignore autocomplete and definition requests on preprocessor lines
- Add option to test completion and definition requests in debug mode

### Fixed

- Improve export of abstract and external interfaces for completion and definition requests
- Fix scope name detection to prevent confusing variables that start with Fortran statement names
- Fix handling of external and abstract interface specifications
- Fix bug preventing unrestricted USE statements from overriding USE only statements
- Fix bug where file parsing ended prematurely in some cases with line continuations

## 0.5.0

### Changed

- Add intrinsic functions and modules to autocomplete suggestions
- Add support for include statements

### Fixed

- Remove erroneously included global objects from autocomplete results in USE ONLY statements
- Fix displayed type for derived type objects in autocomplete requests

## 0.4.0

### Changed

- Add support for find_references, global and top-level module objects only
- Filter autocomplete suggestions for callable objects in call statements
- Speedup initialization and updates on large projects by accelerating construction of USE tree

### Fixed

- Fix parser error with definitions requiring enclosing scopes in #include files and unnamed programs, fixes [#17](https://github.com/hansec/fortran-language-server/issues/17)
- Fix parser failure with visibility statements in included fortran files, fixes [#16](https://github.com/hansec/fortran-language-server/issues/16)
- Fix detection of lines with trailing comments

## 0.3.7

### Changed

- Automatically trigger autocomplete on `%` character
- Show named interfaces and prototypes in document outline
- Add support for autocomplete without prefix filtering

### Fixed

- Fix occasional language server error in autocompletion with class methods

## 0.3.6

### Changed

- Add support for fortran submodules, fixes [#14](https://github.com/hansec/fortran-language-server/issues/14) and [#15](https://github.com/hansec/fortran-language-server/issues/15)
- Improve line tokenization and parsing

### Fixed

- Fix parsing errors with incomplete function definitions
- Fix bugs in symbol and parser debugging

## 0.3.5

### Fixed

- Improve unicode file handling with Python 3.x
- Add support for unnamed programs, fixes [#13](https://github.com/hansec/fortran-language-server/issues/13)

## 0.3.4

### Fixed

- Fix parser error with uppercase characters in scope names, fixes [#11](https://github.com/hansec/fortran-language-server/issues/11)
- Add support for object names with a leading underscore, fixes [#9](https://github.com/hansec/fortran-language-server/issues/9)
- Do not report diagnostics inside preprocessor if statements, fixes [#7](https://github.com/hansec/fortran-language-server/issues/7)

## 0.3.3

### Changed

- Improved Windows support and added AppVeyor CI testing
- Add support for snippets in autocompletion
- Ignore requests in comment sections

### Fixed

- Fix bug with string/byte handling in Python 3
- Fix bug with multiprocess support on Windows
- Fix bug with URI formatting and paths on Windows, fixes [#8](https://github.com/hansec/fortran-language-server/issues/8)

## 0.3.2

### Fixed

- Fix parsing variable definitions containing separators inside strings, fixes [#4](https://github.com/hansec/fortran-language-server/issues/4)
- Fix incorrect variable masking error in functions, fixes [#5](https://github.com/hansec/fortran-language-server/issues/5)
- Do not report intrinsic modules as unknown, fixes [#2](https://github.com/hansec/fortran-language-server/issues/2) and [#3](https://github.com/hansec/fortran-language-server/issues/3)

## 0.3.1

### Changed

- Do not show warnings for variable masking in interface definitions
- Respect visibility statements when searching for object in scope

### Fixed

- Fix bug in incremental document sync with ending newline

## 0.3.0

### Changed

- Add basic file diagnostics (double declaration, variable masking, unknown USE)
- Indicate optional arguments in autocomplete suggestions
- Detect source code format from file contents instead of extension
- Add support for incremental document synchronization

### Fixed

- Fix parsing error when variable definition line is incomplete
- Fix incorrect line handling with open parentheses
- Fix bug when file parsing/hashing fails in workspace initialization

## 0.2.0

### Changed

- Add support for recursive directory inclusion from "root_path"
- Provide option to skip type members in documentSymbol requests
- Apply visibility statements to objects for autocomplete suggestions
- Filter interface suggestions to only show unique signatures
- Link imported procedures in interface definitions

### Fixed

- Fix line continuation handling for free form files with trailing and leading ampersands
- Improve parentheses matching in line parsing

## 0.1.4

### Changed

- Handle line continuations in language server requests
- Add server version number to help output

### Fixed

- Fix bug when parsing files with unicode characters

## 0.1.3

### Changed

- Include interfaces in autocomplete suggestions
- Restrict autocomplete suggestions by object visibility
- Improve USE statement traversal
- Add notifications for parser failures

### Fixed

- Fix bug where parsing errors during workspace initialization could crash the language server

## 0.1.2

- Synchronize version numbers

## 0.1.1

- fix download link in setup.py

## 0.1.0 - First Release

- Initial release
