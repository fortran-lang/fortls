# fortls - the Fortran Language Server

![PyPI](https://img.shields.io/pypi/v/fortls)
![PyPI - Python Version](https://img.shields.io/pypi/pyversions/fortls)
[![Tests](https://github.com/gnikit/fortls/actions/workflows/main.yml/badge.svg)](https://github.com/gnikit/fortls/actions/workflows/main.yml)
[![Documentation](https://github.com/gnikit/fortls/actions/workflows/docs.yml/badge.svg)](https://github.com/gnikit/fortls/actions/workflows/docs.yml)
[![Codecov](https://img.shields.io/codecov/c/gh/gnikit/fortls)](https://app.codecov.io/gh/gnikit/fortls/)
[![GitHub license](https://img.shields.io/github/license/gnikit/fortls)](https://github.com/gnikit/fortls/blob/dev/LICENSE)
[![Code style: black](https://img.shields.io/badge/code%20style-black-000000.svg)](https://github.com/psf/black)

`fortls` is an implementation of the [Language Server Protocol](https://github.com/Microsoft/language-server-protocol)
(LSP) for Fortran using Python (3.7+).

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

For a complete and detailed list of the differences between the two Language Servers
see the Documentation section: [Unique fortls features (not in fortran-language-server)](https://gnikit.github.io/fortls/fortls_changes.html)

The name of executable for this project has been chosen to remain `fortls`
to allow for integration with pre-existing plugins and workflows but it is
potentially subject to change.

## Features

- Project-wide and Document symbol detection and Renaming
- Hover support, Signature help and Auto-completion
- GoTo/Peek implementation and Find/Peek references
- Preprocessor support
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

>**Warning**: it is not recommended having `fortls` and `fortran-language-server`
>simultaneously installed, since they use the same binary name. If you are having trouble
>getting `fortls` to work try uninstalling `fortran-language-server` and reinstalling `fortls`.
>
>```sh
>pip uninstall fortran-language-server
>pip install fortls --upgrade
>```


## Settings

`fortls` can be configured through both the command line e.g.
`fortls --hover_signature` or through a Configuration json file.
The two interfaces are identical and a full list of the available options can
be found in the [Documentation](https://gnikit.github.io/fortls/options.html)
or through `fortls -h`

An example for a Configuration file is given below

```json
{
  "incremental_sync": true,
  "lowercase_intrinsics": true,
  "hover_signature": true,
  "use_signature_help": true,
  "excl_paths": ["tests/**", "tools/**"],
  "excl_suffixes": ["_skip.f90"],
  "include_dirs": ["include/**"],
  "pp_suffixes": [".F90", ".h"],
  "pp_defs": { "HAVE_HDF5": "", "MPI_Comm": "integer" }
}
```

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

When [filing bugs](https://github.com/gnikit/fortls/issues/new)
please provide example code to reproduce the observed issue.

## License

This project is made available under the [MIT License](https://github.com/gnikit/fortls/blob/dev/LICENSE).
