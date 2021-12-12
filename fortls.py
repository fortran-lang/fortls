#!/usr/bin/env python3
if __name__ == "__main__":
    import sys
    import os

    root_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
    sys.path.insert(0, root_dir)
    import fortls

    fortls.main()
