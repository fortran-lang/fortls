# fortls - the Fortran Language Server

[![PyPI Latest Release](https://img.shields.io/pypi/v/fortls.svg)](https://pypi.org/project/fortls/)
[![Tests](https://github.com/gnikit/fortran-language-server/actions/workflows/main.yml/badge.svg)](https://github.com/gnikit/fortran-language-server/actions/workflows/main.yml)
[![image](https://img.shields.io/github/license/gnikit/fortran-language-server.svg)](https://github.com/gnikit/fortran-language-server/blob/master/LICENSE)
[![Code style: black](https://img.shields.io/badge/code%20style-black-000000.svg)](https://github.com/psf/black)

`fortls` is an implementation of the [Language Server Protocol](https://github.com/Microsoft/language-server-protocol)
(LSP) for Fortran using Python (3.6+).

Editor extensions that can integrate with `fortls` to provide autocomplete and
other IDE-like functionality are available for
[Visual Studio Code](https://github.com/krvajal/vscode-fortran-support),
[Atom](https://atom.io/packages/ide-fortran),
[Visual Studio](https://github.com/michaelkonecny/vs-fortran-ls-client),
[(Neo)vim](https://github.com/hansec/fortran-language-server/wiki/Using-forts-with-vim),
and [Emacs](https://github.com/emacs-lsp/lsp-mode).

## fortls vs fortran-language-server

This project is based on @hansec's original Language Server implementation but the two projects have since diverged.
`fortls` (this project) is now developed independently of the upstream `hansec/fortran-language-server` project and contains numerous bug fixes and new features
the original `fortran-language-server` does not.

The name of executable for this project has been chosen to remain `fortls`
to allow for integration with pre-existing plugins and workflows but it is
potentially subject to change.

## Features

- Project-wide and Document symbol detection and Renaming
- Hover support, Signature help and Auto-completion
- GoTo/Peek implementation and Find/Peek references
- Symbol renaming (`textDocument/rename`)
- Documentation parsing ([Doxygen](http://www.doxygen.org/) and
  [FORD](https://github.com/Fortran-FOSS-Programmers/ford) styles)
- Access to multiple intrinsic modules and functions
  - `ISO_FORTRAN_ENV` GCC 11.2.0
  - `IOS_C_BINDING` GCC 11.2.0
  - `IEEE_EXCEPTIONS`, `IEEE_ARITHMETIC`, `IEEE_FEATURES` GCC 11.2.0
  - OpenMP `OMP_LIB`, `OMP_LIB_KINDS` v5.0
  - OpenACC `OPENACC`, `OPENACC_KINDS` v3.1
- Diagnostics
  - Multiple definitions with the same variable name
  - Variable definition masks definition from parent scope
  - Missing subroutine/function arguments
  - Unknown user-defined type used in `TYPE`/`CLASS` definition
    (only if visible in project)
  - Unclosed blocks/scopes
  - Invalid scope nesting
  - Unknown modules in `USE` statement
  - Unimplemented deferred type-bound procedures
  - Use of unimported variables/objects in interface blocks
  - Statement placement errors (`CONTAINS`, `IMPLICIT`, `IMPORT`)
- Code actions (`textDocument/codeAction`) \[Experimental\]
  - Generate type-bound procedures and implementation templates for
    deferred procedures

### Notes/Limitations

- Signature help is not available for overloaded subroutines/functions
- Diagnostics are only updated when files are saved or opened/closed

## Installation

```sh
pip install fortls
```

## fortls settings

The following global settings can be used when launching the language
server.

- `--config` Configuration options file (default: `.fortls`)
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
fortls

- `--debug_filepath=DEBUG_FILEPATH` File path for fortls tests
- `--debug_rootpath=DEBUG_ROOTPATH` Root path for fortls tests
- `--debug_line=DEBUG_LINE` Line position for fortls (1-indexed)
- `--debug_char=DEBUG_CHAR` Character position for fortls tests (1-indexed)
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

Project specific settings can be specified by creating a **options** JSON file.
You can specify the location and name of the file with the `--config` option.
By default the options file is assumed to be `root_dir` with the name `.fortls`.

All command line options are also available through the **options** file as well.

- `lowercase_intrinsics` Use lowercase for intrinsics and keywords in
  autocomplete requests (default: false)
- `debug_log` Write debug information to `root_dir/fortls_debug.log`
  (default: false)
- `disable_diagnostics` Disable diagnostics for this project only
  (default: false)
- `max_line_length` Maximum line length (default: none)
- `max_comment_line_length` Maximum comment line length (default:
  none)

## Additional settings

### Default file extensions

By default all files with the suffix `F,F77,F90,F95,F03,F08,FOR,FPP`
(case-insensitive) in the `root_dir` directory, specified during
initialization, and all its sub-directories are parsed and included in
the project.

### Excluding folders and file extensions

Directories and files can be excluded from the project by specifying
their paths in the `excl_paths` variable in the options file.
Paths can be absolute or relative to `root_dir`.

Excluded directories **will not** exclude all sub-directories.
Source files with a common suffix may also be excluded using the
`excl_suffixes` variable.

> NOTE: All directory inputs fields (`excl_paths`, `source_dirs`, `include_dirs`) support
> [Python glob patterns](https://docs.python.org/3/library/glob.html) e.g. `/**`, `*`, etc.

### Including source directories

By default all source directories under `root_dir` are recursively included.
Source file directories can also be specified manually by specifying
their paths in the `source_dirs` variable in the configuration options file.
Paths can be absolute or relative to `root_dir`.

When defining `source_dirs` in the configuration options filethe default behaviour (i.e. including
all files in all subdirectories under `root_dir`) is overriden. To include them
back again one can do

```json
{
  "source_dirs": ["/**", "all", "other", "dirs"]
}
```

> NOTE: `root_dir` does not need to be specified manually as it is always included.

### Preprocessing

**Note:** Preprocessor support is not "complete", see below. For
preprocessed files the `fortls` will only analyze code within
preprocessor conditional regions if the conditional test can be
evaluated by the server or if the region is the _default_ path (ie. a
bare `#else` region).

**Note:** Currently, `#include` statements are only used for
preprocessing (ie. tracking definitions). Fortran objects defined in
these files will not be processed.

File suffixes for preprocessing can be controlled with the variable
`pp_suffixes` in a workspace's configuration options file. When this variable is
used _only_ those files with the specified suffixes will be
preprocessed. If an empty array is specified then _no_ preprocessing
will be performed on any files. By default, or if the variable is
omitted or `null`, only files with upper case suffixes are preprocessed.

Preprocessor definitions can be set for each project, to improve support
for Fortran files using conditional compilation, using the `pp_defs`
variable in the configuration options file. Preprocessing is performed _only_ for
files where the file extension is all caps (ie. ".F90", ".F", etc.).
Currently, support for preprocessing is limited to variables declared in
the project's configuration options file or in the source file of interest as
`#include` files and inheritance through `USE` statements are yet not
supported. Variable substitution is also performed within files, but is
currently limited to non-recursive cases. For example, `#define PP_VAR1 PP_VAR2` will cause `PP_VAR1` to be replaced with the text `PP_VAR2`
throughout the file, not that value of `PP_VAR2`.

Include directories can be specified using the variable `include_dirs`
in a workspace's configuration options file. These directories are _only_ used to
search for preprocessor `#include`'d files. The directory containing the
file where an `#include` statement is encountered is always searched.
File search is performed starting with the containing directory followed
by the specified `include_dirs` specified paths, in order (left to
right).

```json
{
  "source_dirs": ["subdir1/**", "subdir2"],
  "excl_paths": ["subdir3/**", "subdir1/file_to_skip.F90"],
  "excl_suffixes": ["_skip.f90"],
  "pp_suffixes": [".f03", ".F90"],
  "pp_defs": { "HAVE_PACKAGE": "" },
  "include_dirs": ["rel_include/dir_path", "/abs/include/dir/path"],
  "lowercase_intrinsics": false,
  "debug_log": false
}
```

<!-- ## Editor examples (Atom)

Document symbols (`textDocument/documentSymbol`):

![image](https://raw.githubusercontent.com/gnikit/fortran-language-server/master/images/fortls_outline.png)

Auto-complete (`textDocument/completion`):

![image](https://raw.githubusercontent.com/gnikit/fortran-language-server/master/images/fortls_autocomplete.gif)

Signature help (`textDocument/signatureHelp`):

![image](https://raw.githubusercontent.com/gnikit/fortran-language-server/master/images/fortls_sigHelp.gif)

Goto definition (`textDocument/definition`):

![image](https://raw.githubusercontent.com/gnikit/fortran-language-server/master/images/fortls_gotodef.gif)

Hover (`textDocument/hover`):

![image](https://raw.githubusercontent.com/gnikit/fortran-language-server/master/images/fortls_hover.gif)

Find references (`textDocument/references`):

![image](https://raw.githubusercontent.com/gnikit/fortran-language-server/master/images/fortls_refs.png)

Diagnostics:

![image](https://raw.githubusercontent.com/gnikit/fortran-language-server/master/images/fortls_diag.png) -->

## Implemented server requests

| Request                       | Description                                            |
| ----------------------------- | ------------------------------------------------------ |
| `workspace/symbol`            | Get workspace-wide symbols                             |
| `textDocument/documentSymbol` | Get document symbols e.g. functions, subroutines, etc. |
| `textDocument/completion`     | Suggested tab-completion when typing                   |
| `textDocument/signatureHelp`  | Get signature information at a given cursor position   |
| `textDocument/definition`     | GoTo implementation/Peek implementation                |
| `textDocument/references`     | Find all/Peek references                               |
| `textDocument/rename`         | Rename a symbol across the workspace                   |
| `textDocument/codeAction`     | **Experimental** Generate code                         |

## Acknowledgements

This project would not have been possible without the original work of [@hansec](https://github.com/hansec/)
and the original [`fortran-language-server`](https://github.com/hansec/fortran-language-server)

## Support

If you want to support this project you can do it through

[![Alt](https://www.paypalobjects.com/webstatic/mktg/Logo/pp-logo-150px.png)](https://paypal.me/inikit)
[!["Buy Me A Coffee"](https://www.buymeacoffee.com/assets/img/custom_images/orange_img.png)](https://www.buymeacoffee.com/gnikit)

Support the original project go [here](https://paypal.me/hansec)

## Bug reports

When [filing bugs](https://github.com/gnikit/fortran-language-server/issues/new)
please provide example code to reproduce the observed issue.

## License

This project is made available under the [MIT License](https://github.com/gnikit/fortran-language-server/blob/master/LICENSE).
