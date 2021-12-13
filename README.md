# Fortran Language Server - dev version

> This is the developer's version for @hansec's original `fortran-language-server`.
> It contains bug fixes and new features that are yet to be merged into the main repository.

[![PyPI Latest Release](https://img.shields.io/pypi/v/fortls.svg)](https://pypi.org/project/fortls/)
[![Tests](https://github.com/gnikit/fortran-language-server/actions/workflows/main.yml/badge.svg)](https://github.com/gnikit/fortran-language-server/actions/workflows/main.yml)
[![image](https://img.shields.io/github/license/hansec/fortran-language-server.svg)](https://github.com/hansec/fortran-language-server/blob/master/LICENSE)
[![Code style: black](https://img.shields.io/badge/code%20style-black-000000.svg)](https://github.com/psf/black)

A Fortran implementation of the [Language Server
Protocol](https://github.com/Microsoft/language-server-protocol) using
Python (2.7+ or 3.0+).

Editor extensions using this language server to provide autocomplete and
other IDE-like functionality are available for
[Atom](https://atom.io/packages/ide-fortran), [Visual Studio
Code](https://marketplace.visualstudio.com/items?itemName=hansec.fortran-ls),
[Visual Studio](https://github.com/michaelkonecny/vs-fortran-ls-client),
[(Neo)vim](https://github.com/hansec/fortran-language-server/wiki/Using-forts-with-vim),
and [Emacs](https://github.com/emacs-lsp/lsp-mode).

## Language Server Features

- Document symbols (`textDocument/documentSymbol`)
- Auto-complete (`textDocument/completion`)
- Signature help (`textDocument/signatureHelp`)
- GoTo/Peek definition (`textDocument/definition`)
- Hover (`textDocument/hover`)
- GoTo implementation (`textDocument/implementation`)
- Find/Peek references (`textDocument/references`)
- Project-wide symbol search (`workspace/symbol`)
- Symbol renaming (`textDocument/rename`)
- Documentation parsing ([Doxygen](http://www.doxygen.org/) and
  [FORD](https://github.com/Fortran-FOSS-Programmers/ford) styles)
- Diagnostics (limited)
  - Multiple definitions with the same variable name
  - Variable definition masks definition from parent scope
  - Missing subroutine/function arguments
  - Unknown user-defined type used in "TYPE"/"CLASS" definition
    (only if visible in project)
  - Unclosed blocks/scopes
  - Invalid scope nesting
  - Unknown modules in "USE" statement
  - Unimplemented deferred type-bound procedures
  - Use of unimported variables/objects in interface blocks
  - Statement placement errors ("CONTAINS", "IMPLICIT", "IMPORT")
- Code actions (`textDocument/codeAction`) \[Experimental\]
  - Generate type-bound procedures and implementation templates for
    deferred procedures

### Notes/Limitations

- Signature help is not available for overloaded subroutines/functions
- Diagnostics are only updated when files are saved or opened/closed

## Installation

`pip install fortran-language-server`

If you get the following error:

> `'install_requires' must be a string or list of strings containing valid project/version requirement specifiers`

try updating setuptools:

    pip install -U setuptools
    pip install fortran-language-server

## Language server settings

The following global settings can be used when launching the language
server.

- `--nthreads` Number of threads to use during workspace
  initialization (default: 4)
- `--notify_init` Send notification message when workspace
  initialization is complete
- `--symbol_skip_mem` Do not include type members in document symbol
  results
- `--incremental_sync` Use incremental document synchronization
- `--autocomplete_no_prefix` Do not filter autocomplete results by
  variable prefix
- `--autocomplete_no_snippets` Do not use snippets with place holders
  in autocomplete results
- `--autocomplete_name_only` Complete only the name of procedures and
  not the parameters
- `--lowercase_intrinsics` Use lowercase for intrinsics and keywords
  in autocomplete requests
- `--use_signature_help` Use signature help instead of snippets for
  subroutines/functions
- `--variable_hover` Show hover information for variables (default:
  subroutines/functions only)
- `--hover_signature` Show signature information in hover for argument
  (also enables '--variable_hover')
- `--preserve_keyword_order` Display variable keywords information in
  original order (default: sort to consistent ordering)
- `--enable_code_actions` Enable experimental code actions (default:
  false)
- `--disable_diagnostics` Disable code diagnostics (default: false)
- `--max_line_length` Maximum line length (default: none)
- `--max_comment_line_length` Maximum comment line length (default:
  none)
- `--debug_log` Write debug information to `root_dir/fortls_debug.log`
  (requires a specified `root_dir` during initialization)

### Debug settings

The following settings can be used to perform [standalone debug
tests](https://github.com/hansec/fortran-language-server/wiki) on the
language server.

- `--debug_filepath=DEBUG_FILEPATH` File path for language server
  tests
- `--debug_rootpath=DEBUG_ROOTPATH` Root path for language server
  tests
- `--debug_line=DEBUG_LINE` Line position for language server tests
  (1-indexed)
- `--debug_char=DEBUG_CHAR` Character position for language server
  tests (1-indexed)
- `--debug_full_result` Print full result object instead of condensed
  version
- `--debug_parser` Test source code parser on specified file
- `--debug_diagnostics` Test diagnostic notifications for specified
  file
- `--debug_symbols` Test symbol request for specified file
- `--debug_workspace_symbols=QUERY_STRING` Test workspace/symbol
  request for project-wide search
- `--debug_completion` Test completion request for specified file and
  position
- `--debug_signature` Test signatureHelp request for specified file
  and position
- `--debug_definition` Test definition request for specified file and
  position
- `--debug_hover` Test hover request for specified file and position
- `--debug_implementation` Test implementation request for specified
  file and position
- `--debug_references` Test references request for specified file and
  position
- `--debug_rename=RENAME_STRING` Test rename request for specified
  file and position
- `--debug_actions` Test codeAction request for specified file and
  position

## Configuration

Project specific settings can be specified by placing a JSON file named
`.fortls` (example below) in the `root_dir` directory.

- `lowercase_intrinsics` Use lowercase for intrinsics and keywords in
  autocomplete requests (default: false)
- `debug_log` Write debug information to `root_dir/fortls_debug.log`
  (default: false)
- `disable_diagnostics` Disable diagnostics for this project only
  (default: false)
- `max_line_length` Maximum line length (default: none)
- `max_comment_line_length` Maximum comment line length (default:
  none)

### Setup source file search paths

By default all files with the suffix `F,F77,F90,F95,F03,F08,FOR,FPP`
(case-insensitive) in the `root_dir` directory, specified during
initialization, and all its sub-directories are parsed and included in
the project.

Directories and files can be excluded from the project by specifying
their paths (relative to `root_dir`) in the `excl_paths` variable in the
`.fortls` file. Excluded directories also exclude all sub-directories.
Source files with a common suffix may also be excluded using the
`excl_suffixes` variable.

Source file directories can also be specified manually by specifying
their paths (relative to `root_dir`) in the `source_dirs` variable in
the `.fortls` file. When `source_dirs` is specified directories are not
added recursively, so any nested sub directories must be explicitly
listed. However, `root_dir` does not need to be specified manually as it
is always included.

External source files (ex. libraries) can also be included in language
server results by specifying their paths in the `ext_source_dirs`
variable in the `.fortls` file. These files will be parsed during
initialization, but will not be updated with any changes made until the
language server is restarted. As with `source_dirs`, specified
directories are not added recursively, so any nested sub directories
must be explicitly listed.

**Note:** The previous naming convention for source file directories
(`mod_dirs`) is still supported but has been deprecated.

### Preprocessing

**Note:** Preprocessor support is not "complete", see below. For
preprocessed files the language server will only analyze code within
preprocessor conditional regions if the conditional test can be
evaluated by the server or if the region is the _default_ path (ie. a
bare `#else` region).

**Note:** Currently, `#include` statements are only used for
preprocessing (ie. tracking definitions). Fortran objects defined in
these files will not be processed.

File suffixes for preprocessing can be controlled with the variable
`pp_suffixes` in a workspace's `.fortls` file. When this variable is
used _only_ those files with the specified suffixes will be
preprocessed. If an empty array is specified then _no_ preprocessing
will be performed on any files. By default, or if the variable is
ommited or `null`, only files with upper case suffixes are preprocessed.

Preprocessor definitions can be set for each project, to improve support
for Fortran files using conditional compilation, using the `pp_defs`
variable in the `.fortls` file. Preprocessing is performed _only_ for
files where the file extension is all caps (ie. ".F90", ".F", etc.).
Currently, support for preprocessing is limited to variables declared in
the project's `.fortls` file or in the source file of interest as
`#include` files and inheritance through `USE` statements are yet not
supported. Variable substitution is also performed within files, but is
currently limited to non-recursive cases. For example, `#define PP_VAR1 PP_VAR2` will cause `PP_VAR1` to be replaced with the text `PP_VAR2`
throughout the file, not that value of `PP_VAR2`.

Include directories can be specified using the variable `include_dirs`
in a workspace's `.fortls` file. These directories are _only_ used to
search for preprocessor `#include`'d files. The directory containing the
file where an `#include` statement is encountered is always searched.
File search is performed starting with the containing directory followed
by the specified `include_dirs` specified paths, in order (left to
right).

    {
      "source_dirs": ["subdir1", "subdir2"],
      "excl_paths": ["subdir3", "subdir1/file_to_skip.F90"],
      "excl_suffixes": ["_skip.f90"],
      "pp_suffixes": [".f03", ".F90"],
      "pp_defs": {"HAVE_PACKAGE": ""},
      "include_dirs": ["rel_include/dir_path", "/abs/include/dir/path"],
      "ext_source_dirs": ["/path/to/fortran/library"],
      "lowercase_intrinsics": false,
      "debug_log": false
    }

## Bug reports

When [filing
bugs](https://github.com/hansec/fortran-language-server/issues/new)
please provide example code to reproduce the observed issue.

## License

This project is made available under the [MIT
License](https://github.com/hansec/fortran-language-server/blob/master/LICENSE).

## Support

If you _really_ like [this
package](https://github.com/hansec/fortran-language-server) you can [buy
me a coffee](https://paypal.me/hansec) to say thanks.

## Editor examples (Atom)

Document symbols (`textDocument/documentSymbol`):

![image](https://raw.githubusercontent.com/hansec/fortran-language-server/master/images/fortls_outline.png)

Auto-complete (`textDocument/completion`):

![image](https://raw.githubusercontent.com/hansec/fortran-language-server/master/images/fortls_autocomplete.gif)

Signature help (`textDocument/signatureHelp`):

![image](https://raw.githubusercontent.com/hansec/fortran-language-server/master/images/fortls_sigHelp.gif)

Goto definition (`textDocument/definition`):

![image](https://raw.githubusercontent.com/hansec/fortran-language-server/master/images/fortls_gotodef.gif)

Hover (`textDocument/hover`):

![image](https://raw.githubusercontent.com/hansec/fortran-language-server/master/images/fortls_hover.gif)

Find references (`textDocument/references`):

![image](https://raw.githubusercontent.com/hansec/fortran-language-server/master/images/fortls_refs.png)

Diagnostics:

![image](https://raw.githubusercontent.com/hansec/fortran-language-server/master/images/fortls_diag.png)
