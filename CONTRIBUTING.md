# Contributing

👍🎉 Thank you for taking the time to contribute! 🎉👍

In this file you will find all the steps necessary to guide you through your first contribution to the project.

Please note our [Code of Conduct](https://github.com/fortran-lang/fortls/blob/master/CODE_OF_CONDUCT.md) and adhere to it in all your interactions with this project.

## 📚 Getting Started

A good place to start is the [Issues tab](https://github.com/fortran-lang/fortls/issues) on GitHub. Look for any issues with the `help wanted` tag.

### Downloading ⬇️

Firstly, fork the repository from <https://github.com/fortran-lang/fortls>.

Then clone the forked repository into your local machine.

```sh
git@github.com:<YOUR-USERNAME>/fortls.git
```

Where `<YOUR-USERNAME>` should be your GitHub username.

### Dependencies

To build this project you will need [Python](https://www.python.org/) `>= 3.7` and [pip](https://www.python.org/) `>= 21.0`.
To install all Python dependencies open a terminal go into the `fortls` cloned folder and run:

```sh
pip install -e ".[dev,docs]"
```

### Testing 🧪

To verify that your cloning of the GitHub repository worked as expected open a terminal and run:

```sh
pytest -v
```

This will run the entire unit test suite. You can also run this to verify that you haven't broken anything in the code.

👉 **Tip!** You can run individual tests by selecting the path to the Python file and the method

```sh
pytest test/test_interface.py::test_version_update_pypi
```

### Developing & Debugging 🐞️

❗️ Before you start developing, open a terminal inside `fortls` and run:

```sh
pre-commit install
```

This will ensure that all you commits meet the formatting standards of the project.

---

You can now start writing code! Your local `fortls` version will be updated with every code change you make, so you can use your normal code editor to checkout the `fortls` features that you have implemented.
It is however considerably easier to create compact unittests to check if your changes have worked.

A `fortls` test normally involves writing a Python function which sends a JSONRPC request to the server and then test checks for the correct server response.
Often times small bits of Fortran source code also have to be submited to be used by the test.
You can find varisous test examples in the `tests` directory.

👉 **Tip!** You can attach a debugger to the main `fortls` source code during unittesting which should allow you to pause, break, step into, etc. while testing, thus making it easier to find mistakes.

### Merging

To merge your changes to the main `fortls` repository push your branch on GitHub and open a [Pull Request](https://github.com/fortran-lang/fortls/pulls). Ping `@gnikit` to review your PR.
