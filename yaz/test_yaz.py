#!/usr/bin/env python3.6

import unittest



class Foo(unittest.TestCase):
    def test_foo(self):
        assert False

    def test_bar(self):
        assert True


if __name__ == '__main__':
    unittest.main()
