#!/usr/bin/env python
from setuptools import find_packages, setup

README = open("README.rst", "r").read()

setup(
    name="fortran-language-server",
    # Versions should comply with PEP440.  For a discussion on single-sourcing
    # the version across setup.py and the project code, see
    # https://packaging.python.org/en/latest/single_source_version.html
    version="1.12.0",
    description="FORTRAN Language Server for the Language Server Protocol",
    long_description=README,
    # The project's main homepage.
    url="https://github.com/hansec/fortran-language-server",
    download_url=(
        "https://github.com/hansec/fortran-language-server/archive/v1.12.0.tar.gz"
    ),
    author="Chris Hansen",
    author_email="hansec@uw.edu",
    # You can just specify the packages manually here if your project is
    # simple. Or you can use find_packages().
    packages=find_packages(exclude=["contrib", "docs", "test"]),
    package_data={"fortls": ["*.json"]},
    # List run-time dependencies here.  These will be installed by pip when
    # your project is installed. For an analysis of "install_requires" vs pip's
    # requirements files see:
    # https://packaging.python.org/en/latest/requirements.html
    install_requires=[
        'future; python_version < "3"',
        'argparse; python_version < "2.7" or python_version in "3.0, 3.1"',
    ],
    # To provide executable scripts, use entry points in preference to the
    # "scripts" keyword. Entry points provide cross-platform support and allow
    # pip to create the appropriate form of executable for the target platform.
    entry_points={
        "console_scripts": [
            "fortls = fortls.__init__:main",
        ]
    },
)
