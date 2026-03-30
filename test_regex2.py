import re

# OLD regex from the code
OLD_PP_INCLUDE = re.compile(r'[ ]*#[ ]*include[ ]*(["\w\.]*)', re.I)

# NEW regex after fix
NEW_PP_INCLUDE = re.compile(r'[ ]*#[ ]*include[ ]*(["\w\.\\/]*)', re.I)

test_lines = [
    '#include "file.inc"',
    '#include "subdir/file.inc"',
    '#include "../file.inc"',
    '#include "./file.inc"',
    "#include 'file.inc'",
    '#include "dir/subdir/test_inc2.f90"',
]

print("Testing OLD regex:")
for line in test_lines:
    m = OLD_PP_INCLUDE.match(line)
    if m:
        print(f"  '{line}' -> '{m.group(1)}'")
    else:
        print(f"  '{line}' -> NO MATCH")

print("\nTesting NEW regex:")
for line in test_lines:
    m = NEW_PP_INCLUDE.match(line)
    if m:
        print(f"  '{line}' -> '{m.group(1)}'")
    else:
        print(f"  '{line}' -> NO MATCH")
