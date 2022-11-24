:sd_hide_title:

==============
fortls
==============


.. div:: landing-title
    :style: padding: 0.1rem 0.5rem 0.6rem 0; background-image: linear-gradient(315deg, #2753e3 0%, #734f96 74%); clip-path: polygon(0px 0px, 100% 0%, 100% 100%, 0% calc(100% - 1.5rem)); -webkit-clip-path: polygon(0px 0px, 100% 0%, 100% 100%, 0% calc(100% - 1.5rem));

    .. grid::
        :reverse:
        :gutter: 2 3 3 3
        :margin: 4 4 1 2

        .. grid-item::
            :columns: 12 6 6 6

            .. image:: ../assets/logo2-animated.svg
                :alt: fortls
                :width: 100%

        .. grid-item::
            :columns: 12 6 6 6
            :child-align: justify
            :class: sd-text-white sd-fs-3

            A Language Server for Fortran providing code completion, diagnostics, hovering and more.

            .. button-ref:: quickstart
                :ref-type: doc
                :outline:
                :color: white
                :class: sd-px-4 sd-fs-5

                Get Started


.. .. grid:: 2
..     :gutter: 0
..     :class-container: sd-text-center sd-pt-4
..     :class-row: sd-align-minor-center

..     .. grid-item::
..         .. button-link:: https://github.com/sponsors/gnikit
..             :ref-type: ref
..             :outline:
..             :color: danger
..             :class: sd-px-2 sd-fs-4

..             Become a **Sponsor**
..             :octicon:`mark-github;2em;sd-text-black`
..             :octicon:`heart-fill;2em;sd-text-danger`

..     .. grid-item::
..         .. button-link:: https://www.paypal.com/paypalme/inikit
..             :ref-type: ref
..             :color: primary
..             :class: sd-px-2 sd-fs-5

..             Make a **Donation**
..             :fa:`fa-paypal`


.. div::  sd-text-center

    *A tool to supercharge Fortran development!*

    .. tab-set::
        :class: sd-align-major-center


        .. tab-item:: Completion
            :class-label: sd-rounded-2 sd-border-1 sd-my-2 sd-mx-2 sd-px-2 sd-py-1

            .. image:: ../assets/lsp/completion-ani.gif

        .. tab-item:: Hover
            :class-label: sd-rounded-2 sd-border-1 sd-my-2 sd-mx-2 sd-px-2 sd-py-1

            .. image:: ../assets/lsp/hover2.png

        .. tab-item:: Rename
            :class-label: sd-rounded-2 sd-border-1 sd-my-2 sd-mx-2 sd-px-2 sd-py-1

            .. image:: ../assets/lsp/rename2.gif

        .. tab-item:: Symbols
            :class-label: sd-rounded-2 sd-border-1 sd-my-2 sd-mx-2 sd-px-2 sd-py-1

            .. image:: ../assets/lsp/symbols-crop.png

        .. tab-item:: References
            :class-label: sd-rounded-2 sd-border-1 sd-my-2 sd-mx-2 sd-px-2 sd-py-1

            .. image:: ../assets/lsp/definition-peek.png

        .. tab-item:: Diagnostics
            :class-label: sd-rounded-2 sd-border-1 sd-my-2 sd-mx-2 sd-px-2 sd-py-1

            .. image:: ../assets/lsp/diagnostics1.png


.. TODO: here go the sponsors

.. toctree::
    :hidden:

    quickstart.rst

.. toctree::
    :maxdepth: 2
    :caption: Components
    :hidden:

    features.rst
    editor_integration.rst
    options.rst
    fortls_changes.md

.. toctree::
    :maxdepth: 2
    :caption: Get Involved
    :hidden:

    contributing.rst

.. toctree::
    :maxdepth: 2
    :caption: Contact Us
    :hidden:

    contact.rst

.. toctree::
    :hidden:
    :caption: Development

    modules.rst

.. grid:: 1 2 3 3
    :margin: 4 4 0 0
    :gutter: 1


    .. grid-item-card:: :octicon:`desktop-download;5em;sd-text-primary`
        :link-type: any
        :link: Download
        :class-body: sd-text-center

        Download


    .. grid-item-card:: :material-sharp:`import_contacts;5em;sd-text-primary`
        :class-body: sd-text-center
        :link: features
        :link-type: doc

        Features

    .. grid-item-card:: :material-outlined:`settings;5em;sd-text-primary`
        :link-type: doc
        :link: options
        :class-body: sd-text-center

        Configuration Options

    .. grid-item-card:: :octicon:`browser;5em;sd-text-primary`
        :link-type: doc
        :link: editor_integration
        :class-body: sd-text-center

        Editor Integration

    .. grid-item-card:: :material-round:`mail;5em;sd-text-primary`
        :link-type: doc
        :link: contact
        :class-body: sd-text-center

        Contact Us

    .. grid-item-card:: :octicon:`git-pull-request;5em;sd-text-primary`
        :link-type: doc
        :link: contributing
        :class-body: sd-text-center

        Contribute


..
   Include native markdown into native rst
   .. include:: README.md
      :parser: myst_parser.sphinx_
