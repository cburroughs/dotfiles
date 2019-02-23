#!/usr/bin/env python3.6

import unittest


from yaz import *

class Foo(unittest.TestCase):
    #def test_foo(self):
    #    assert False

    def test_bar(self):
        assert True


class TestConfigSnapshots(unittest.TestCase):
    def test_happy(self):
        target = Config.Snapshots.from_json({'daily': 7})
        self.assertEqual(7, target.daily)


class TestConfigDestination(unittest.TestCase):
    def test_happy(self):
        target = Config.Destination.from_json({'user': 'mimir', 'hostname': 'well',
                                               'dataset': 'tank/eyes'})
        self.assertEqual('mimir', target.user)
        self.assertEqual('well', target.hostname)
        self.assertEqual('tank/eyes', target.dataset)
        self.assertEqual('tank', target.pool)


class TestConfig(unittest.TestCase):
    def test_happy(self):
        d = {
            'sigil': 'ᚩ',
            'pool': 'cauldron',
            'snapshots': {
                'daily': 5
            },
            'destination': {
                'user': 'mimir',
                'hostname': 'well',
                'dataset': 'tank/eyes'
            }
        }

        cfg = Config.from_json(d)
        self.assertEqual('ᚩ', cfg.sigil)
        self.assertEqual('cauldron', cfg.pool)
        self.assertEqual(5, cfg.snapshots.daily)
        self.assertEqual('well', cfg.destination.hostname)


class TestRemoteShellCmd(unittest.TestCase):
    def test_simple(self):
        class Whoami(ShellCmd):
            def cmd_line(self):
                return 'whoami'
        c = RemoteShellCmd(Config.Destination('mirmir', 'well', 'tank/eyes'), Whoami())
        self.assertEqual("ssh mirmir@well 'whoami'", c.cmd_line())


class TestAllPoolPropertiesCmd(unittest.TestCase):
    def test_simple(self):
        c = AllPoolPropertiesCmd('tank')
        self.assertEqual('zpool get -Hp all tank', c.cmd_line())





if __name__ == '__main__':
    unittest.main()
