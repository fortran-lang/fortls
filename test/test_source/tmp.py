import os

from fparser.common.readfortran import FortranFileReader
from fparser.two.parser import ParserFactory
from fparser.two.utils import Base, walk

os.chdir("/home/gn/Code/Python/fortls/test/test_source")

reader = FortranFileReader("test.f90", include_dirs=["subdir"], ignore_comments=False)
parser = ParserFactory().create(std="f2008")
parse_tree = parser(reader)

for i, node in enumerate(walk(parse_tree)):
    if node is None:
        continue
    if not isinstance(node, Base):
        continue
    print(i, type(node), ": ", node)
