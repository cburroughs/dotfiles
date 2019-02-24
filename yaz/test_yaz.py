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


class TestRootYazSnapshot(unittest.TestCase):
    now = 1550948411

    def test_as_str(self):
        s = RootYazSnapshot('ᚩ', 'daily', 15, self.now)
        self.assertEqual('yaz-ᚩ-daily-f-5c71983b',
                         s.as_str())

    def test_decode(self):
        s = RootYazSnapshot.decode_from_str('yaz-ᚩ-daily-f-5c71983b')
        self.assertEqual('ᚩ', s.sigil)
        self.assertEqual('daily', s.freq)
        self.assertEqual(15, s.seq)
        self.assertEqual(self.now, s.timestamp)


    def test_next(self):
        s = RootYazSnapshot('ᚩ', 'daily', 15, self.now)
        self.assertEqual('yaz-ᚩ-daily-10-5c71983c',
                         s.next_in_seq(now=self.now+1).as_str())

    def test_time_for_next(self):
        s = RootYazSnapshot('ᚩ', 'daily', 15, self.now)
        self.assertFalse(s.is_time_for_next(now=self.now+10))
        self.assertTrue(s.is_time_for_next(now=self.now+86400*2))


class TestRemoteShellCmd(unittest.TestCase):
    def test_simple(self):
        class Whoami(ShellCmd):
            def cmd_line(self):
                return 'whoami'
        c = RemoteShellCmd(Config.Destination('mirmir', 'well', 'tank/eyes'), Whoami())
        self.assertEqual("ssh mirmir@well 'whoami'", c.cmd_line())


class TestAllPoolPropertiesCmd(unittest.TestCase):
    def test(self):
        c = AllPoolPropertiesCmd('tank')
        self.assertEqual('zpool get -Hp all tank', c.cmd_line())


class TestRemotePipeShellCmd(unittest.TestCase):
    def test(self):
        class Mario(ShellCmd):
            def cmd_line(self):
                return 'mario'
        class World4(ShellCmd):
            def cmd_line(self):
                return 'world -4'
        c = RemotePipeShellCmd(Mario(),
                               Config.Destination('mirmir', 'well', 'tank/eyes'),
                               World4())
        self.assertEqual("mario | ssh mirmir@well 'world -4'",
                         c.cmd_line())

class TestTakePoolSnapshotCmd(unittest.TestCase):
    def test(self):
        c = TakePoolSnapshotCmd('cauldron', 'frog')
        self.assertEqual('zfs snapshot -r cauldron@frog',
                         c.cmd_line())

class TestDestroyPoolSnapshotCmd(unittest.TestCase):
    def test(self):
        c = DestroyPoolSnapshotCmd('cauldron', 'frog')
        self.assertEqual('zfs destroy -r cauldron@frog',
                         c.cmd_line())

class TestInitialSendCmd(unittest.TestCase):
    def test(self):
        c = InitialSendCmd('cauldron', 'frog')
        self.assertEqual('zfs send -cR cauldron@frog',
                         c.cmd_line())

class TestIncrementalSendCmd(unittest.TestCase):
    def test(self):
        c = IncrementalSendCmd('cauldron', 'gall-of-goat', 'slip-of-yew')
        self.assertEqual('zfs send -cR -I gall-of-goat cauldron@slip-of-yew',
                         c.cmd_line())

class TestRecvCmd(ShellCmd):
    def test_simple(self):
        c = RecvCmd(Config.Destination('mirmir', 'well', 'tank/eyes'))
        self.assertEqual('zfs recv -du tank/eyes', c.cmd_line())

    def test_force(self):
        c = RecvCmd(Config.Destination('mirmir', 'well', 'tank/eyes'), force=True)
        self.assertEqual('zfs recv -F -du tank/eyes', c.cmd_line())


if __name__ == '__main__':
    unittest.main()
