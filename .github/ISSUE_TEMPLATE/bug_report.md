---
name: Bug report
about: Create a report to help us improve
title: ''
labels: bug
assignees: ''

---

**Describe the bug**
A clear and concise description of what the bug is.

**To Reproduce**
Try and reproduce the `fortls` error through the debug interface, for more see `fortls --debug_help`. Usually debug requests start like `fortls --debug_filepath your_file.f90 --debug_rootpath . ...`.

Start with posting:
1. a **Minimal Working Example** to demonstrate the bug
2. the `fortls` command to reproduce the issue, or your `fortls`
3. the output of the `fortls` command
4. Any additional JSONRPC requests like the ones produced with `--debug_log`

Alternatively, you can try and describe the steps that you followed to encounter the bug:
1. Go to '...'
2. Click on '....'
3. Scroll down to '....'
4. See error

**Expected behavior**
A clear and concise description of what you expected to happen.

**Screenshots & Animations**
If applicable, add screenshots or GIF/MP4 animations to help explain your problem.

**Setup information (please complete the following information):**
 - OS: [e.g. Linux, Mac]
 - Python Version [e.g. 3.10]
 - fortls Version [e.g. 2.3]
 - Code editor used [e.g. VS Code, Vim]
 - the Fortran extension for the code editor and its version [e.g. Modern Fortran v3.0.0] (if applicable)

**Configuration information (please complete the following information):**
- Your `.fortlsrc` or `.fortls.json` or `.fortls` configuration file OR any other JSON config being used (if any)
- Any settings specified through your extension [e.g. for VS Code settings from `settings.json`]

**Additional context**
Add any other context about the problem here.
