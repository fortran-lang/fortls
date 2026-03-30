import os
import sys
sys.path.insert(0, '.')
from fortls.parsers.internal.parser import FortranFile, preprocess_file

# Test parsing #include with path
test_dir = "test/test_source"
file_path = os.path.join(test_dir, "test_inc.f90")

print(f"Testing file: {file_path}")
print(f"File exists: {os.path.isfile(file_path)}")

# Create FortranFile and load
ff = FortranFile(file_path)
err, changed = ff.load_from_disk()
print(f"Load error: {err}, changed: {changed}")

# Test preprocess
include_dirs = {os.path.abspath(test_dir)}
print(f"include_dirs: {include_dirs}")

contents_pp, pp_skips, pp_defines, pp_defs = preprocess_file(
    ff.contents_split,
    file_path=file_path,
    pp_defs={},
    include_dirs=include_dirs,
    debug=False
)

print(f"\nProcessed {len(contents_pp)} lines")
print("\nContents after preprocessing:")
for i, line in enumerate(contents_pp[:15]):
    print(f"{i+1}: {line}")
