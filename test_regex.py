import re

p = re.compile(r'[ ]*#[ ]*include[ ]*(["\w\.]*)', re.I)
m = p.match('#include "subdir/test_inc2.f90"')
print("Match:", m.groups() if m else None)

# Test the new regex with slash support
p2 = re.compile(r'[ ]*#[ ]*include[ ]*(["\w\./\\]*)', re.I)
m2 = p2.match('#include "subdir/test_inc2.f90"')
print("Match2:", m2.groups() if m2 else None)
