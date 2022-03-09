Editor Integration
===================

Visual Studio Code
------------------

The Language Server is natively supported through the `Modern Fortran`_ extension.
Install ``fortls`` then install the extension and all of the server's features should be instantly available.

.. _Modern Fortran: https://marketplace.visualstudio.com/items?itemName=krvajalm.linter-gfortran

.. note::
    Make sure that ``fortls`` is reachable in your ``$PATH``. If not you can specify the option
    ``"fortran.fortls.path": "/custom/path/to/fortls"``

Atom
----

Firstly ``fortls`` then install the `language-fortran`_ plugin by `@dparkins`_ to get Fortran syntax highlighting.
Finally, install either `fortran-lsp`_ by `@gnikit`_ or `ide-fortran`_ by `@hansec`_

.. warning::
    `fortran-lsp`_ has been created solely for the ``fortls`` Language Server, hence it natively interfaces with ``fortls``.
    `ide-fortran`_ was created for an older, now deprecated, Fortran Language Server hence the options
    available through the extension are not representative of ``fortls``'s interface.

.. _language-fortran: https://atom.io/packages/language-fortran
.. _@dparkins: https://github.com/dparkins
.. _fortran-lsp: https://atom.io/packages/fortran-lsp
.. _@gnikit: https://github.com/gnikit
.. _ide-fortran: https://atom.io/packages/ide-fortran
.. _@hansec: https://github.com/hansec

Sublime Text
------------

Firstly, install ``fortls`` then install the `LSP`_ package from package control.
Finally, install the `Fortran`_ package and add the following in your configuration

.. code-block:: json

    {
        "clients": {
            "fortls": {
                "enabled": true,
                "command": ["fortls", "--notify_init"],
                "selector": "source.modern-fortran | source.fixedform-fortran"
            }
        }
    }

For more details see the LSP `documentation`_.

.. _LSP: https://github.com/sublimelsp/LSP
.. _Fortran: https://packagecontrol.io/packages/Fortran
.. _documentation: https://lsp.sublimetext.io/language_servers/#fortran


Vim, Neovim, GVim
-----------------

Firstly install the plugin `LanguageClient-neovim`_. Then edit your ``~/.vimrc`` settings file
to set ``fortls`` for Fortran files

.. code-block:: vim

    " Required for operations modifying multiple buffers like rename. set hidden
    let g:LanguageClient_serverCommands = {
        " Add any default arguments you want fortls to have inside []
        \ 'fortls': ['--hover_signature', '--use_signature_help'],
        \ }

    " note that if you are using Plug mapping you should not use `noremap` mappings.
    nmap <F5> <Plug>(lcn-menu)
    " Or map each action separately
    nmap <silent>K <Plug>(lcn-hover)
    nmap <silent> gd <Plug>(lcn-definition)
    nmap <silent> <F2> <Plug>(lcn-rename)

.. _LanguageClient-neovim: https://github.com/autozimu/LanguageClient-neovim

EMACS
-----

Install the `lsp-mode`_ plugin. This should then allow for the variables
`lsp-clients-fortls-args`_ and `lsp-clients-fortls-executable`_ to be defined in the ``~/.emacs`` configuration file.

.. _lsp-mode: https://emacs-lsp.github.io/lsp-mode/page/installation
.. _lsp-clients-fortls-args: https://emacs-lsp.github.io/lsp-mode/page/lsp-fortran/#lsp-clients-fortls-args
.. _lsp-clients-fortls-executable: https://emacs-lsp.github.io/lsp-mode/page/lsp-fortran/#lsp-clients-fortls-executable

Visual Studio 2017
------------------

Installing this `VS17 extension`_ should enable ``fortls`` features in Visual Studio

.. _VS17 extension: https://github.com/michaelkonecny/vs-fortran-ls-client

Kakoune
-------

Install `kak-lsp <https://github.com/kak-lsp/>`_.

Edit the kak-lsp.toml config file to include:

.. code-block:: sh

  [language.fortran]
  filetypes = ["fortran"]
  roots = [".git", ".fortl
  command = "fortls"
  args = ["--symbol_skip_mem", "--incremental_sync", "--autocomplete_no_prefix", "--lowercase_intrisics"]

Edit your kakrc config to enable kak-lsp, adding fortran as a filetype:

.. code-block:: sh

   eval %sh{kak-lsp --kakoune -s $kak_session}
   # lsp-enable
   hook global WinSetOption filetype=(fortran) %{
     lsp-enable-window
   }


