Get Started
###########

.. article-info::
    :avatar: ../assets/f.svg
    :avatar-link: https://github.com/gnikit
    :author: `gnikit <https://github.com/gnikit>`__
    :date: |today|
    :read-time: 1 min read
    :class-avatar: sd-animate-grow50-rot20


``fortls`` is a tool known as a language server that interfaces with your code editor
(VS Code, Vim, etc.) to provide features like code completion, code navigation, hover messages, and many more.

Download
********

The project is available for download through the **PyPi** and **Anaconda** package managers

.. tab-set::

    .. tab-item:: PyPi

        .. code-block:: sh

            pip install fortls


        For more information see `pypi/fortls`_

        .. _pypi/fortls: https://pypi.python.org/pypi/fortls

    .. tab-item:: Anaconda

        .. code-block:: sh

            conda install -c conda-forge fortls

        For more installation instructions, see `conda-forge/fortls`_.

        .. _conda-forge/fortls: https://github.com/conda-forge/fortls-feedstock#about-fortls

    .. tab-item:: Brew

        .. code-block:: sh

            brew install fortls

        For more installation instructions, see `brew/fortls`_.

        .. _brew/fortls: https://formulae.brew.sh/formula/fortls

    .. tab-item:: Source

        Alternatively, one can install the development version from **GitHub** via

        .. code-block:: sh

            pip install --user --upgrade git+git://github.com/fortran-lang/fortls


.. warning::
    It is **NOT** possible having ``fortls`` and ``fortran-language-server``
    simultaneously installed, since they use the same executable name. If you are having trouble
    getting ``fortls`` to work try uninstalling ``fortran-language-server`` and reinstalling ``fortls``.


Usage
*****

To make full use of ``fortls`` in your workflow you need to

- integrate it into your code editor, see: :doc:`editor_integration`
- (Optional) configure any additional settings to ``fortls``, see: :doc:`options`

Integration
===========

Depending on the code editor used, different steps will have to be followed to integrate ``fortls``.
Luckily, we support numerous code editors and have detailed instructions in the
:doc:`editor_integration` section.

.. card:: Example: VS Code

    Setting up ``fortls`` with `VS Code`_ is as simple as installing
    the `Modern Fortran`_ extension.

.. _VS Code: https://code.visualstudio.com
.. _Modern Fortran: https://marketplace.visualstudio.com/items?itemName=fortran-lang.linter-gfortran


Configuration
=============

The Language Server by default is configured with reasonable settings however,
depending on the project additional settings might need to be configured, such
as source file paths, or additional preprocessor definitions.

Instructions on how to do this and much more can be found in the :doc:`options` section.
