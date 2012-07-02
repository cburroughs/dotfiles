#!/usr/bin/env python

import urllib
import sys
import os

def run():
    input_stream = sys.stdin
    for line in input_stream:
        try: 
            sys.stdout.write(urllib.unquote(line))
        except:
            sys.exit(1)
if __name__ == "__main__":
    if sys.stdin.isatty() or '--help' in sys.argv or '-h' in sys.argv:
        print "Usage: cat data | %s" % os.path.basename(sys.argv[0])
        sys.exit(1)
    run()
