![alt](https://raw.githubusercontent.com/fortran-lang/fortls/master/assets/logo.png)

# fortls - Fortran Language Server

[![Powered by NumFOCUS](https://img.shields.io/badge/powered%20by-NumFOCUS-orange.svg?style=flat-square&colorA=E1523D&colorB=007D8A)](https://numfocus.org)
![PyPI](https://img.shields.io/pypi/v/fortls?style=flat-square)
![PyPI - Python Version](https://img.shields.io/pypi/pyversions/fortls?style=flat-square)
[![PyPI - Downloads](https://img.shields.io/pypi/dm/fortls?style=flat-square&label=PyPi)](https://pepy.tech/project/fortls)
![Conda](https://img.shields.io/conda/dn/conda-forge/fortls?label=Anaconda&style=flat-square)
![GitHub License](https://img.shields.io/github/license/fortran-lang/fortls?style=flat-square)
![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/fortran-lang/fortls/main.yml?branch=master&label=CI&style=flat-square)
![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/fortran-lang/fortls/docs.yml?branch=master&label=Docs&style=flat-square)
![Codecov](https://img.shields.io/codecov/c/github/fortran-lang/fortls?style=flat-square)
[![Code style: black](https://img.shields.io/badge/code%20style-black-000000.svg?style=flat-square)](https://github.com/psf/black)
![GitHub Repo stars](https://img.shields.io/github/stars/fortran-lang/fortls?color=yellow&style=flat-square)
[![DOI](https://zenodo.org/badge/412392321.svg?style=flat-square)](https://zenodo.org/badge/latestdoi/412392321)

<!-- [<img alt="https://github.com/sponsors/gnikit" src="https://img.shields.io/static/v1?style=social&label=Sponsor&message=%E2%9D%A4&logo=GitHub&color&link=%3Curl%3E" height="30" />](https://github.com/sponsors/gnikit)
[<img alt="https://paypal.me/inikit" src="https://img.shields.io/static/v1?style=social&label=Donate&message=%E2%9D%A4&logo=Paypal&color&link=%3Curl%3E" height="30" />](https://paypal.me/inikit) -->

![alt](https://raw.githubusercontent.com/fortran-lang/fortls/master/assets/animations/intro-demo.gif)

`fortls` is an implementation of the [Language Server Protocol](https://github.com/Microsoft/language-server-protocol)
(LSP) for Fortran using Python (3.7+).

All code editors that support LSP can integrate with `fortls` see the section
[Editor Integration](https://fortls.fortran-lang.org/editor_integration.html#editor-integration) in the documentation.
Some supported code editors include:
[Visual Studio Code](https://fortls.fortran-lang.org/editor_integration.html#visual-studio-code),
[Atom](https://fortls.fortran-lang.org/editor_integration.html#atom),
[Sublime Text](https://fortls.fortran-lang.org/editor_integration.html#sublime-text),
[(Neo)Vim](https://fortls.fortran-lang.org/editor_integration.html#vim-neovim-gvim),
and [Emacs](https://fortls.fortran-lang.org/editor_integration.html#emacs).

## Features

- Project-wide and Document symbol detection and Renaming
- Hover support, Signature help and Auto-completion
- GoTo/Peek implementation and Find/Peek references
- Preprocessor support
- Documentation parsing ([Doxygen](http://www.doxygen.org/) and
  [FORD](https://github.com/Fortran-FOSS-Programmers/ford) styles)
- Access to multiple intrinsic modules and functions
  - `ISO_FORTRAN_ENV` GCC 11.2.0
  - `ISO_C_BINDING` GCC 11.2.0
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
  - Use of non-imported variables/objects in interface blocks
  - Statement placement errors (`CONTAINS`, `IMPLICIT`, `IMPORT`)
- Code actions
  - Generate type-bound procedures and implementation templates for
    deferred procedures

### Notes/Limitations

- Signature help and hover does not handle elegantly overloaded functions i.e. interfaces

## Documentation

The full documentation for `fortls` can be found at
[fortls.fortran-lang.org](https://fortls.fortran-lang.org/).

## Installation

### PyPi

```sh
pip install fortls
```

### Anaconda

```sh
conda install -c conda-forge fortls
```

for more information about the Anaconda installation [see](https://github.com/conda-forge/fortls-feedstock#about-fortls)

### Common installation problems

It is **NOT** recommended having `fortls` and `fortran-language-server`
simultaneously installed, since they use the same executable name. If you are having trouble
getting `fortls` to work try uninstalling `fortran-language-server` and reinstalling `fortls`.

With `pip`

```sh
pip uninstall fortran-language-server
pip install fortls --upgrade
```

or with Anaconda

```sh
conda uninstall fortran-language-server
conda install -c conda-forge fortls
```

## Settings

`fortls` can be configured through both the command line e.g.
`fortls --hover_signature` or through a Configuration json file.
The two interfaces are identical and a full list of the available options can
be found in the [Documentation](https://fortls.fortran-lang.org/options.html)
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

| Request                          | Description                                            |
| -------------------------------- | ------------------------------------------------------ |
| `workspace/symbol`               | Get workspace-wide symbols                             |
| `textDocument/documentSymbol`    | Get document symbols e.g. functions, subroutines, etc. |
| `textDocument/completion`        | Suggested tab-completion when typing                   |
| `textDocument/signatureHelp`     | Get signature information at a given cursor position   |
| `textDocument/definition`        | GoTo definition/Peek definition                        |
| `textDocument/references`        | Find all/Peek references                               |
| `textDocument/documentHighlight` | Same as `textDocument/references`                      |
| `textDocument/hover`             | Show messages and signatures upon hover                |
| `textDocument/implementation`    | GoTo implementation/Peek implementation                |
| `textDocument/rename`            | Rename a symbol across the workspace                   |
| `textDocument/didOpen`           | Document synchronisation upon opening                  |
| `textDocument/didSave`           | Document synchronisation upon saving                   |
| `textDocument/didClose`          | Document synchronisation upon closing                  |
| `textDocument/didChange`         | Document synchronisation upon changes to the document  |
| `textDocument/codeAction`        | **Experimental** Generate code                         |

## Future plans

`fortls` has reached a point where it is feature complete and stable enough to be used in many modern Fortran projects without any issues.
It does however still have fundamental limitations,
namely its ability to understand all Fortran syntax and semantics that has been used throughout the 65+ years of the language. **The good news is that we have a plan to address this issue!**

We are excited to announce that we are working on creating a new Fortran Language Server
based on the actively developed [LFortran](https://lfortran.org/) compiler 🎉.
The new Language Server will be able to understand all Fortran syntax, be faster,
and give more accurate autocompletion, hover and diagnostic information. That means we plan on investing any future funding on creating our new language server and ultimately creating a better user experience for everyone.

<!--  Have a look at our roadmap (link) and consider contributing to our efforts (donations and contribution tasks link).  -->

### What about `fortls`?

Not to worry, `fortls` will continue to be here. We will keep `fortls` in active maintenance mode with bug fixes and new features from volunteer contributors, but otherwise we will be focusing our efforts into making the new language server using LFortran's parser a reality.

## `fortls` vs `fortran-language-server`

This project was originally based on `fortran-language-server` LSP implementation, but the two projects have since diverged.

`fortls` (this project) is now developed independently of the upstream `hansec/fortran-language-server` project and contains numerous new features and bug fixes
the original `fortran-language-server` does not.

For a complete and detailed list of the differences between the two Language Servers
see the Documentation section: [Unique fortls features (not in fortran-language-server)](https://fortls.fortran-lang.org/fortls_changes.html)

The name of executable for this project has been chosen to remain `fortls`
to allow for integration with pre-existing plugins and workflows, but it could
change in the future.

## Acknowledgements

This project would not have been possible without the original work of [@hansec](https://github.com/hansec/)
in [`fortran-language-server`](https://github.com/hansec/fortran-language-server)

## Support

You can support Fortran-lang as a whole by donating at
[Fortran-lang - NumFOCUS](https://numfocus.org/donate-for-fortran-lang).

## Bug reports

When [filing bugs](https://github.com/fortran-lang/fortls/issues/new)
please provide example code to reproduce the observed issue.

## Security Policy

To report a security vulnerability please follow the instructions in our
[Security page](https://github.com/fortran-lang/fortls/security/policy).

## License

This project is made available under the [MIT License](https://github.com/fortran-lang/fortls/blob/master/LICENSE).
