#!/usr/bin/env python

import pprint
import sys

import demjson
import yaml

def main(argv):
    from optparse import OptionParser
    parser = OptionParser()
    (options, args) = parser.parse_args()
    fname = args[0]
    raw_str = open(fname).read()
    py_obj = demjson.decode(raw_str)
    #pprint.pprint(py_obj)
    print yaml.dump(py_obj, encoding='utf-8', 
                    default_flow_style=False,
                    width=70, indent=4).replace('!!python/unicode', '')


    pass

if __name__ == '__main__':
    main(sys.argv[1])
