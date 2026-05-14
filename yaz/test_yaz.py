#!/usr/bin/env python3

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
        c = InitialSendCmd('cauldron@frog')
        self.assertEqual('zfs send -pc cauldron@frog',
                         c.cmd_line())

class TestIncrementalSendCmd(unittest.TestCase):
    def test(self):
        c = IncrementalSendCmd('cauldron/eyes', 'gall-of-goat', 'slip-of-yew')
        self.assertEqual(
            'zfs send -pc -I cauldron/eyes@gall-of-goat cauldron/eyes@slip-of-yew',
            c.cmd_line())

    def test_raw(self):
        c = IncrementalSendCmd('cauldron/eyes', 'gall-of-goat', 'slip-of-yew', raw=True)
        self.assertEqual(
            'zfs send -pcw -I cauldron/eyes@gall-of-goat cauldron/eyes@slip-of-yew',
            c.cmd_line())


class TestConfigIgnoreDatasets(unittest.TestCase):
    def _cfg(self, ignore):
        return Config('s', 'tank',
                      Config.Snapshots(7),
                      Config.Destination('u', 'h', 'zroot/backup'),
                      ignore)

    def test_default_empty(self):
        d = {
            'sigil': 's', 'pool': 'tank',
            'snapshots': {'daily': 7},
            'destination': {'user': 'u', 'hostname': 'h', 'dataset': 'zroot/backup'},
        }
        cfg = Config.from_json(d)
        self.assertEqual([], cfg.ignore_datasets)
        self.assertFalse(cfg.is_ignored('tank/HOME'))

    def test_from_json_with_list(self):
        d = {
            'sigil': 's', 'pool': 'tank',
            'snapshots': {'daily': 7},
            'destination': {'user': 'u', 'hostname': 'h', 'dataset': 'zroot/backup'},
            'ignore_datasets': ['tank/HOME/cache'],
        }
        cfg = Config.from_json(d)
        self.assertEqual(['tank/HOME/cache'], cfg.ignore_datasets)

    def test_exact_match(self):
        cfg = self._cfg(['tank/HOME'])
        self.assertTrue(cfg.is_ignored('tank/HOME'))

    def test_prefix_match(self):
        cfg = self._cfg(['tank/HOME'])
        self.assertTrue(cfg.is_ignored('tank/HOME/csb'))
        self.assertTrue(cfg.is_ignored('tank/HOME/csb/cache'))

    def test_no_boundary_false_positive(self):
        cfg = self._cfg(['tank/HOME'])
        self.assertFalse(cfg.is_ignored('tank/HOMEX'))
        self.assertFalse(cfg.is_ignored('tank/HOMEY/sub'))

    def test_sibling_not_ignored(self):
        cfg = self._cfg(['tank/HOME'])
        self.assertFalse(cfg.is_ignored('tank/var'))
        self.assertFalse(cfg.is_ignored('tank'))

    def test_multi_entry(self):
        cfg = self._cfg(['tank/HOME', 'tank/var/tmp'])
        self.assertTrue(cfg.is_ignored('tank/HOME/csb'))
        self.assertTrue(cfg.is_ignored('tank/var/tmp'))
        self.assertTrue(cfg.is_ignored('tank/var/tmp/foo'))
        self.assertFalse(cfg.is_ignored('tank/var'))


class TestGroupSnapshotsByDataset(unittest.TestCase):
    def test_groups(self):
        out = (
            'tank@yaz-a-daily-0-1\n'
            'tank@yaz-a-daily-1-2\n'
            'tank/HOME@yaz-a-daily-0-1\n'
            'tank/HOME@yaz-a-daily-1-2\n'
            'tank/var@yaz-a-daily-0-1\n'
        )
        by_ds = group_snapshots_by_dataset(out)
        self.assertEqual({'yaz-a-daily-0-1', 'yaz-a-daily-1-2'}, by_ds['tank'])
        self.assertEqual({'yaz-a-daily-0-1', 'yaz-a-daily-1-2'}, by_ds['tank/HOME'])
        self.assertEqual({'yaz-a-daily-0-1'}, by_ds['tank/var'])

    def test_empty_and_blank_lines(self):
        self.assertEqual({}, group_snapshots_by_dataset(''))
        self.assertEqual({}, group_snapshots_by_dataset('\n\n'))

    def test_ignores_non_snapshot_lines(self):
        out = 'tank\ntank/HOME\ntank@s1\n'
        by_ds = group_snapshots_by_dataset(out)
        self.assertEqual({'tank': {'s1'}}, by_ds)


class TestMapLocalToRemote(unittest.TestCase):
    def test_pool_root(self):
        self.assertEqual('zroot/backup/ys76/alpha',
                         map_local_to_remote('tank', 'tank', 'zroot/backup/ys76/alpha'))

    def test_child(self):
        self.assertEqual('zroot/backup/ys76/alpha/HOME',
                         map_local_to_remote('tank/HOME', 'tank', 'zroot/backup/ys76/alpha'))

    def test_grandchild(self):
        self.assertEqual('zroot/backup/ys76/alpha/HOME/csb',
                         map_local_to_remote('tank/HOME/csb', 'tank',
                                             'zroot/backup/ys76/alpha'))

    def test_outside_pool_asserts(self):
        with self.assertRaises(AssertionError):
            map_local_to_remote('other/dataset', 'tank', 'zroot/backup')

class TestRecvCmd(ShellCmd):
    def test_simple(self):
        c = RecvCmd(Config.Destination('mirmir', 'well', 'tank/eyes'))
        self.assertEqual('zfs recv -du tank/eyes', c.cmd_line())

    def test_force(self):
        c = RecvCmd(Config.Destination('mirmir', 'well', 'tank/eyes'), force=True)
        self.assertEqual('zfs recv -F -du tank/eyes', c.cmd_line())


if __name__ == '__main__':
    unittest.main()
