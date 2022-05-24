Editor Integration
===================

`Visual Studio Code <https://code.visualstudio.com/>`__
-------------------------------------------------------

The Language Server is natively supported through the `Modern Fortran`_ extension.
Install ``fortls`` then install the extension and all the server's features should be instantly available.

.. _Modern Fortran: https://marketplace.visualstudio.com/items?itemName=fortran-lang.linter-gfortran

.. important::
    Make sure that ``fortls`` is reachable in your ``$PATH``. If not you can specify the option
    ``"fortran.fortls.path": "/custom/path/to/fortls"``

`Atom <https://atom.io/>`__
---------------------------

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

`Sublime Text <https://www.sublimetext.com/>`__
-----------------------------------------------

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


`neovim <https://neovim.io/>`__
-------------------------------

.. warning::
    For neovim versions < 0.5.0 follow the instructions in the :ref:`vim` section.

Neovim version >= 0.5.0 `natively supports LSP <https://neovim.io/doc/lsp/>`_.
To enable the native LSP functionality install the `lspconfig`_ plugin with your
favourite plugin manager.

Then in your configuration file (i.e. ``init.lua``) add the following:

.. code-block:: lua

    require'lspconfig'.fortls.setup{}


If additional ``fortls`` options need to be passed to you can do that through
the ``cmd`` option in ``setup{}``


.. code-block:: lua

    require'lspconfig'.fortls.setup{
        cmd = {
            'fortls',
            '--lowercase_intrisics',
            '--hover_signature',
            '--hover_language=fortran',
            '--use_signature_help'
        }
    }

.. important::
    If you are just starting with ``neovim`` it is strongly recommended using
    the `Suggested configuration`_ from `lspconfig`_ for keybingings and server
    attaching. **Remember to attach the server during setup{}**

.. _lspconfig: https://github.com/neovim/nvim-lspconfig
.. _Suggested configuration: https://github.com/neovim/nvim-lspconfig#suggested-configuration


.. _vim:

`Vim <https://www.vim.org/>`__
------------------------------

Vim does not support LSP natively, so a 3rd party extensions need to be installed.
A few options are available:

`YouCompleteMe <https://ycm-core.github.io/YouCompleteMe/>`__
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

`YouCompleteMe <https://github.com/ycm-core/YouCompleteMe>`__
is a popular Vim plugin and code-completion engine that also provides an LSP interface.
You can therefore use it to register Language Servers like ``fortls``.

For more information about configuring an arbitrary Language Server in YouCompleteMe,
`see here <https://ycm-core.github.io/YouCompleteMe/#plugging-an-arbitrary-lsp-server>`__.

.. code-block:: vim

    " YouCompleteMe configuration options
    let g:ycm_language_server =
        \[
        \   {
        \       'name': 'fortls',
        \       'cmdline': ['fortls', '--hover_language', 'fortran', '--notify_init', '--hover_signature', '--use_signature_help'],
        \       'filetypes': ['fortran'],
        \       'project_root_files': ['.fortls'],
        \   },
        \]
    nmap <leader>yfw <Plug>(YCMFindSymbolInWorkspace)
    nmap <leader>yfd <Plug>(YCMFindSymbolInDocument)


`LanguageClient-neovim <https://github.com/autozimu/LanguageClient-neovim>`__
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Firstly install the plugin `LanguageClient-neovim`_. Then edit your ``~/.vimrc`` settings file
to set ``fortls`` for Fortran files

.. code-block:: vim

    " Required for operations modifying multiple buffers like rename. set hidden
    let g:LanguageClient_serverCommands = {
        " Add any default arguments you want fortls to have inside []
        \ 'fortran': ['fortls', '--hover_signature', '--hover_language', 'fortran', '--use_signature_help'],
        \ }

    " note that if you are using Plug mapping you should not use `noremap` mappings.
    nmap <F5> <Plug>(lcn-menu)
    " Or map each action separately
    nmap <silent>K <Plug>(lcn-hover)
    nmap <silent> gd <Plug>(lcn-definition)
    nmap <silent> <F2> <Plug>(lcn-rename)

.. _LanguageClient-neovim: https://github.com/autozimu/LanguageClient-neovim


`EMACS <https://www.gnu.org/software/emacs/>`__
-----------------------------------------------

`LSP Mode <https://emacs-lsp.github.io/lsp-mode>`__
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Install the `lsp-mode`_ plugin. This should then allow for the variables
`lsp-clients-fortls-args`_ and `lsp-clients-fortls-executable`_ to be defined in the ``~/.emacs`` configuration file.

.. _lsp-mode: https://emacs-lsp.github.io/lsp-mode/page/installation
.. _lsp-clients-fortls-args: https://emacs-lsp.github.io/lsp-mode/page/lsp-fortran/#lsp-clients-fortls-args
.. _lsp-clients-fortls-executable: https://emacs-lsp.github.io/lsp-mode/page/lsp-fortran/#lsp-clients-fortls-executable


`Eglot <https://github.com/joaotavora/eglot>`__
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Install the `eglot`_ package which supports fortls out of the box.
This can be done in emacs version > 26.1 via ``M-x package-install RET eglot RET``.
Arguments to ``fortls`` can be provided in the form

.. code-block:: elisp

    (add-to-list 'eglot-server-programs '(f90-mode . ("fortls" "--notify_init" "--nthreads=4")))

.. _eglot: https://github.com/joaotavora/eglot


Visual Studio 2017
------------------

Installing this `VS17 extension`_ should enable ``fortls`` features in Visual Studio

.. _VS17 extension: https://github.com/michaelkonecny/vs-fortran-ls-client

`Kakoune <https://kakoune.org/>`__
----------------------------------

Install `kak-lsp <https://github.com/kak-lsp/>`_.

Edit the ``kak-lsp.toml`` config file to include:

.. code-block:: sh

  [language.fortran]
  filetypes = ["fortran"]
  roots = [".git", ".fortls"]
  command = "fortls"
  args = ["--symbol_skip_mem", "--incremental_sync", "--autocomplete_no_prefix", "--lowercase_intrisics"]

Edit your ``kakrc`` config to enable ``kak-lsp``, adding ``fortran`` as a filetype:

.. code-block:: sh

   eval %sh{kak-lsp --kakoune -s $kak_session}
   # lsp-enable
   hook global WinSetOption filetype=(fortran) %{
     lsp-enable-window
   }
