import re

p = re.compile(r'[ ]*#[ ]*include[ ]*(["\w\.\\/]*)', re.I)
tests = ['#include "petscpc.h"', '#include "dir/file.h"', "#include 'file.h'"]
for t in tests:
    m = p.match(t)
    print(f"{t!r} -> {m.groups() if m else None}")
