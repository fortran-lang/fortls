#!/usr/bin/env python

"""Builds the Fortran Language Server - dev
"""
import pathlib

from setuptools import find_packages, setup

from fortls import __version__

# The directory containing this file
HERE = pathlib.Path(__file__).resolve().parent

# The text of the README file is used as a description
README = (HERE / "README.md").read_text()

NAME = "fortls"

setup(
    name=NAME,
    version=__version__,
    url="https://github.com/gnikit/fortran-language-server",
    author="Giannis Nikiteas",
    author_email="giannis.nikiteas@gmail.com",
    description="FORTRAN Language Server - dev version",
    long_description=README,
    long_description_content_type="text/markdown",
    license="MIT",
    classifiers=[
        "Development Status :: 4 - Beta",
        "Intended Audience :: Developers",
        "Intended Audience :: Science/Research",
        "License :: OSI Approved :: MIT License",
        "Natural Language :: English",
        "Programming Language :: Python",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Fortran",
        "Operating System :: Microsoft :: Windows",
        "Operating System :: POSIX",
        "Operating System :: Unix",
        "Operating System :: MacOS",
    ],
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
