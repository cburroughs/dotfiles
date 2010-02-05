#!/usr/bin/env python

import sys

import demjson
import yaml

def main(argv):
    from optparse import OptionParser
    parser = OptionParser()
    (options, args) = parser.parse_args()
    fname = args[0]
    raw_str = open(fname).read()
    py_obj = yaml.load(raw_str)
    pprint.pprint(py_obj)
    print demjson.encode(py_obj)

    pass

if __name__ == '__main__':
    main(sys.argv[1])
