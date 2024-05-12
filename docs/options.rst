Configuration options
=====================

``fortls`` can be configured through the command line interface and/or
through a configuration file (by default named ``.fortls``).
The options available from the command line and through the configuration file
are identical and interchangeable.

.. important:: Options defined in the configuration file have precedence over command line arguments.

The following sections discuss the available settings in detail.


.. _cmd_interface:

Configuration using the command line
------------------------------------

.. argparse::
   :module: fortls
   :func: cli
   :prog: fortls
   :nodefault:

Configuration using a file
--------------------------

A configuration file is a JSONC (JSON with comments) file that contains project specific
settings for ``fortls``. By default, the Language Server will recognise 3 default
names ``.fortlsrc``, ``.fortls.json`` and ``.fortls`` (in that order)
under the ``root_path`` of the project, e.g. ``root_path/.fortlsrc``.
A different configuration file name can be passed with the command line
interface options ``--config`` e.g. ``fortls --config my_project.json``.

The settings that can be specified in the configuration file are identical to
the ones available through the command line interface having removed the leading
``--`` characters. For the command line interface see :ref:`cmd_interface`.

Available options
#################

All the ``fortls`` settings with their default arguments can be found below

.. code-block:: json

   {
      "nthreads": 4,
      "notify_init": false,
      "incremental_sync": false,
      "recursion_limit": 1000,
      "sort_keywords": false,
      "disable_autoupdate": false,
      "debug_log": false,

      "source_dirs": ["./**"],
      "incl_suffixes": [],
      "excl_suffixes": [],
      "excl_paths": [],

      "autocomplete_no_prefix": false,
      "autocomplete_no_snippets": false,
      "autocomplete_name_only": false,
      "lowercase_intrinsics": false,
      "use_signature_help": false,

      "hover_signature": false,
      "hover_language": "fortran90",

      "max_line_length": -1,
      "max_comment_line_length": -1,
      "disable_diagnostics": false,

      "pp_suffixes": [],
      "include_dirs": [],
      "pp_defs": {},

      "symbol_skip_mem": false,

      "enable_code_actions": false
   }

Sources file parsing
####################

source_dirs
***********

.. code-block:: json

   {
      "source_dirs": ["./**", "/external/fortran/src"]
   }

By default all directories under the current project will be recursively parsed
for Fortran sources. Alternatively, one can define a series of directories
for ``fortls`` to look for source files

.. note:: glob fnmatch style patterns  are allowed

incl_suffixes
*************

.. code-block:: json

   {
      "incl_suffixes": [".h", ".FYP", "inc"]
   }

``fortls`` will parse only files with ``incl_suffixes`` extensions found in
``source_dirs``. Using the above example, ``fortls`` will match files by the
``file.h`` and ``file.FYP``, but not ``file.fyp`` or ``filefyp``.
It will also match ``file.inc`` and ``fileinc`` but not ``file.inc2``.

By default, ``incl_suffixes`` are defined as
.F .f .F03 .f03 .F05 .f05 .F08 .f08 .F18 .f18 .F77 .f77 .F90 .f90 .F95 .f95 .FOR .for .FPP .fpp.
Additional source file extensions can be defined in ``incl_suffixes``.

.. note:: The default file extensions cannot be overwritten. ``incl_suffixes`` will only append to the default extensions.


excl_suffixes
*************

.. code-block:: json

   {
      "excl_suffixes": ["_tmp.f90", "_hdf5.F90"]
   }

If certain files or suffixes do not need to be parsed these can be excluded by
deffining ``excl_suffixes``


excl_paths
**********

Entire directories can be excluded from parsing by including them in ``excl_paths``.

.. note:: glob fnmatch style patterns  are allowed

``excl_paths`` uses glob patterns so if you want to exclude a directory and all
its subdirectories from being parsed you should define it like so

.. code-block:: json

   {
      "excl_paths": ["exclude_dir/**"]
   }

Preprocessor
############

pp_suffixes
***********

.. code-block:: json

   {
      "pp_suffixes" : [".h", ".F90", ".fpp"]
   }

By default preprocessor definitions are parsed for all Fortran source files
with uppercase extensions e.g. ``.F90``, ``.F``, ``.F08``, etc.. However, the
default behaviour can be overriden by defining ``pp_defs``.


include_dirs
************

.. code-block:: json

   {
      "include_dirs": ["include", "preprocessor", "/usr/include"]
   }

By default ``fortls`` will scan the project's directories for files with extensions
``PP_SUFFIXES`` to parse for **preprocessor definitions**. However, if the preprocessor
files are external to the project, their locations can be specific via
``include_dirs``.

.. note:: glob fnmatch style patterns are allowed
.. warning:: Source files detected in ``include_dirs`` will not be parsed for Fortran objects unless they are also included in ``source_dirs``.


pp_defs
*******

.. code-block:: json

   {
      "pp_defs": {
         "HAVE_PETSC": ""
         "Mat": "type(tMat)"
      }
   }

Additional **preprocessor definitions** from what are specified in files found in
``include_dirs`` can be defined in ``pp_defs``.

.. note:: Definitions in ``pp_defs`` will override definitions from ``include_dirs``


Limitations
***********

- Recursive substitution is not available e.g.

   .. code-block:: cpp

      #define VAR1 10
      #define VAR2 VAR1


Debug Options (command line only)
---------------------------------

Options for debugging language server

-  ``--debug_filepath DEBUG_FILEPATH``         File path for language server tests
-  ``--debug_rootpath DEBUG_ROOTPATH``         Root path for language server tests
-  ``--debug_parser``                          Test source code parser on specified file
-  ``--debug_preproc``                         Test preprocessor on specified file
-  ``--debug_hover``                           Test `textDocument/hover` request for specified file and position
-  ``--debug_rename RENAME_STRING``            Test `textDocument/rename` request for specified file and position
-  ``--debug_actions``                         Test `textDocument/codeAction` request for specified file and position
-  ``--debug_symbols``                         Test `textDocument/documentSymbol` request for specified file
-  ``--debug_completion``                      Test `textDocument/completion` request for specified file and position
-  ``--debug_signature``                       Test `textDocument/signatureHelp` request for specified file and position
-  ``--debug_definition``                      Test `textDocument/definition` request for specified file and position
-  ``--debug_references``                      Test `textDocument/references` request for specified file and position
-  ``--debug_diagnostics``                     Test diagnostic notifications for specified file
-  ``--debug_implementation``                  Test `textDocument/implementation` request for specified file and position
-  ``--debug_workspace_symbols QUERY_STRING``  Test `workspace/symbol` request
-  ``--debug_line INTEGER``                    Line position for language server tests (1-indexed)
-  ``--debug_char INTEGER``                    Character position for language server tests (1-indexed)
-  ``--debug_full_result``                     Print full result object instead of condensed version
