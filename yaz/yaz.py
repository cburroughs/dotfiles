#!/usr/bin/env python3

import abc
import argparse
import json
import logging
import os
import subprocess
import sys
import time

from typing import Dict, List


# Yet Another ZFS replication script

# Design space:
# * Prefer config files to property magic
# * "Extra" snapshots are okay, don't fight with zfs-auto-snapshot
# * Replicate a workstation pool, not a NAS or container host
# * Learning python 3 along the way.

# Helpful *development* tools
# virtual environment: python3.6  -m venv env
# mypy, pycodestyle, coverage


LOG = logging.getLogger(__name__)


class Config():

    class Snapshots():

        def __init__(self, daily: int):
            self.daily = daily

        @staticmethod
        def from_json(d: Dict):
            # TODO: error early on other than daily
            return Config.Snapshots(d['daily'])

    class Destination():

        def __init__(self, user: str, hostname: str, dataset: str):
            self.user = user
            self.hostname = hostname
            self.dataset = dataset
            self.pool = dataset.split('/')[0]

        @staticmethod
        def from_json(d: Dict):
            return Config.Destination(d['user'], d['hostname'], d['dataset'])

    def __init__(self, sigil: str, pool: str,
                 snapshots, destination):
        self.sigil = sigil
        self.pool = pool
        self.snapshots = snapshots
        self.destination = destination

    @staticmethod
    def from_json(d: Dict):
        return Config(d['sigil'], d['pool'],
                      Config.Snapshots.from_json(d['snapshots']),
                      Config.Destination.from_json(d['destination']))

    @staticmethod
    def load_config(fname: str):
        with open(fname, 'r') as f:
            return Config.from_json(json.load(f))


def parse_features(lines: str) -> Dict[str, str]:
    features = {}
    for line in lines.split('\n'):
        if 'feature@' in line:
            columns = line.split('\t')
            features[columns[1]] = columns[2]
    return features


class RootYazSnapshot():
    # form yaz-${sigil}-${freq}-${hex_seq}-${hex_epoch_sec}

    # The use of multiple "sigils" is to help transition to new versions, and
    # reserve the potential to allow multiple full backups to sit side by side
    # "freq" is always daily, but leaves room for others in the future

    def __init__(self, sigil: str, freq: str, seq: int, timestamp: int):
        assert freq == 'daily'
        self.sigil = sigil
        self.freq = freq
        self.seq = seq
        self.timestamp = timestamp

    def as_str(self) -> str:
        hex_seq = hex(self.seq)[2:]
        hex_ts = hex(self.timestamp)[2:]
        return f'yaz-{self.sigil}-{self.freq}-{hex_seq}-{hex_ts}'

    def next_in_seq(self, now=None) -> 'RootYazSnapshot':
        if now is None:
            now = int(time.time())
        return RootYazSnapshot(self.sigil, self.freq, self.seq + 1, now)

    def is_time_for_next(self, now=None) -> bool:
        if now is None:
            now = int(time.time())
        approx_sec_day = 86400 * 0.98
        return now > approx_sec_day + self.timestamp

    @staticmethod
    def decode_from_str(name: str) -> 'RootYazSnapshot':
        tupe = name.split('-')
        assert tupe[0] == 'yaz'
        seq = int(tupe[3], 16)
        ts = int(tupe[4], 16)
        return RootYazSnapshot(tupe[1], tupe[2], seq, ts)


class YazSnapshots():

    def __init__(self, snapshots: List[RootYazSnapshot]):
        # TODO: assert monotonic increasing seq?
        self.snapshots = sorted(snapshots, key=lambda s: s.seq)

    def is_empty(self) -> bool:
        empty = len(self.snapshots) == 0
        if empty:
            LOG.warning('no existing yaz snapshots found')
        return empty

    def begin_sequence(self, cfg) -> RootYazSnapshot:
        LOG.warning('beginning snapshot sequence')
        snap = RootYazSnapshot(cfg.sigil, 'daily', 0, int(time.time()))
        self.snapshots.append(snap)
        return snap

    def next_in_seq(self, force: bool = True):
        if force or self.snapshots[-1].is_time_for_next():
            snap = self.snapshots[-1].next_in_seq()
            self.snapshots.append(snap)
            return snap
        else:
            return None

    def should_prune_eldest(self, cfg):
        return len(self.snapshots) > cfg.snapshots.daily

    def pop_eldest(self):
        return self.snapshots.pop(0)

    @staticmethod
    def from_cmd_output(output: str, sigil: str) -> 'YazSnapshots':
        snapshots = []
        for line in output.split('\n'):
            if not line:
                continue
            if '/' in line:
                continue
            snap = line.split('@')[1]
            if snap.startswith('yaz-' + sigil):
                snapshots.append(RootYazSnapshot.decode_from_str(snap))
        return YazSnapshots(snapshots)

    def __repr__(self):
        return f'YazSnapshots(snapshots={self.snapshots})'


##### shell command objects #####


class ShellCmd(abc.ABC):

    @abc.abstractmethod
    def cmd_line(self) -> str:
        pass

    def check_output(self):
        LOG.info(f'check_output cmd: {self.cmd_line()}')
        return subprocess.check_output(self.cmd_line(), shell=True)

    def check_call(self):
        LOG.info(f'check_call cmd: {self.cmd_line()}')
        return subprocess.check_call(self.cmd_line(), shell=True)


class RemoteShellCmd(ShellCmd):

    def __init__(self, dest: Config.Destination, cmd: ShellCmd):
        self.user = dest.user
        self.hostname = dest.hostname
        self.cmd = cmd

    def cmd_line(self) -> str:
        return f"ssh {self.user}@{self.hostname} '{self.cmd.cmd_line()}'"


class RemotePipeShellCmd(ShellCmd):

    def __init__(self, local_cmd: ShellCmd,
                 dest: Config.Destination,
                 remote_cmd: ShellCmd):
        self.local_cmd = local_cmd
        self.dest = dest
        self.remote_cmd = remote_cmd

    def cmd_line(self):
        return (f'{self.local_cmd.cmd_line()} | ' +
                f'ssh {self.dest.user}@{self.dest.hostname} ' +
                f"'{self.remote_cmd.cmd_line()}'")


class AllPoolPropertiesCmd(ShellCmd):

    def __init__(self, pool: str):
        self.pool = pool

    def cmd_line(self):
        return f'zpool get -Hp all {self.pool}'


class ListPoolSnapshotsCmd(ShellCmd):

    def __init__(self, pool: str, recursive=True):
        self.pool = pool
        self.recursive_flag = '-r' if recursive else ''

    def cmd_line(self):
        return f'zfs list -Hp {self.recursive_flag} -t snapshot -o name {self.pool}'


class ListDatasetSnapshotsCmd(ShellCmd):
    # The snapshots on a single dataset

    def __init__(self, dataset: str):
        self.dataset = dataset

    def cmd_line(self):
        return f'zfs list -Hp -r -d 1 -t snapshot -o name {self.dataset}'


class ListPoolDatasetsCmd(ShellCmd):

    def __init__(self, pool: str, recursive=True):
        self.pool = pool
        self.recursive_flag = '-r' if recursive else ''

    def cmd_line(self):
        return f'zfs list -Hp {self.recursive_flag} -o name {self.pool}'


class TakePoolSnapshotCmd(ShellCmd):

    def __init__(self, pool: str, snap_name: str):
        self.pool = pool
        self.snap_name = snap_name

    def cmd_line(self):
        return f'zfs snapshot -r {self.pool}@{self.snap_name}'


class DestroyPoolSnapshotCmd(ShellCmd):

    def __init__(self, pool: str, snap_name: str):
        assert len(pool) > 0
        assert len(snap_name) > 0
        self.pool = pool
        self.snap_name = snap_name

    def cmd_line(self):
        return f'zfs destroy -r {self.pool}@{self.snap_name}'


class InitialSendCmd(ShellCmd):
    # After much trial and error, it is best to avoid -R on the initial send and
    # use each "fully qualified" snapshot name.  This is because resume is only
    # per dataset, you can't resume the full -R recursion.  Thus we pass in each
    # full "pool/dataset@snap" from `zfs list` instead of interpolating it.

    def __init__(self, snap_name: str):
        self.snap_name = snap_name

    def cmd_line(self):
        return f"zfs send -pc {self.snap_name}"


class IncrementalSendCmd(ShellCmd):

    def __init__(self, pool: str, from_snap: str, to_snap: str):
        self.pool = pool
        self.from_snap = from_snap
        self.to_snap = to_snap

    def cmd_line(self):
        return f"zfs send -cR -I {self.from_snap} {self.pool}@{self.to_snap}"


## TODO: Why does -F seem to destory old snapshots from my laptop, but not desktop?
class RecvCmd(ShellCmd):
    # zfs recv -R with -F will destory snapshots and datasets and is thus no
    # protection against accidentally destroying either.  Indeed it will
    # replicate mistake to the backup!  By default this does not force the recv
    # then, trading safety for (unbounded) storage growth on the backup.  Either
    # a manual run with --force-recv, or a separate less frequent cron, is thus
    # needed

    def __init__(self, dest: Config.Destination, force: bool = False, resume: bool = False):
        self.dest = dest
        self.force = force
        self.force_flag = '-F ' if force else ''
        self.resume_flag = 's' if resume else ''

    def cmd_line(self):
        return f'zfs recv {self.force_flag} -{self.resume_flag}du {self.dest.dataset}'

##### cmds #####


def check_feature_compatibility(config):
    local_props = AllPoolPropertiesCmd(config.pool).check_output()
    local_features = parse_features(local_props.decode())
    remote_props = RemoteShellCmd(config.destination,
                                  AllPoolPropertiesCmd(config.destination.pool)).check_output()
    remote_features = parse_features(remote_props.decode())
    ok = True
    for key, val in local_features.items():
        if val == 'enabled':
            if key not in remote_features:
                LOG.error(f'feature {key} not in remote_features')
                ok = False
            elif key in remote_features and remote_features[key] == 'disabled':
                LOG.error(f'feature {key} disabled in remote_features')
                ok = False
    if ok:
        LOG.info('ok: features match')
    else:
        raise Exception('feature mismatch!')


def verify_remote_dataset_exits_but_empty(config):
    remote_datasets = RemoteShellCmd(
        config.destination,
        ListPoolDatasetsCmd(config.destination.dataset.split('/')[0])).check_output()
    remote_datasets = remote_datasets.decode().split('\n')
    if config.destination.dataset not in remote_datasets:
        msg = f'target dataset does not exist on remote'
        LOG.error(msg)
        raise Exception(msg)
    remote_snapshots = RemoteShellCmd(
        config.destination,
        ListPoolSnapshotsCmd(config.destination.dataset)).check_output()
    remote_snapshots = list(filter(lambda s: '@' in s,
                                   remote_snapshots.decode().split('\n')))
    if len(remote_snapshots) > 0:
        msg = f'dataset {config.destination.dataset} has {len(remote_snapshots)} snapshot can not seed'
        LOG.error(msg)
        raise Exception(msg)


def cmd_check_features(args):
    config = Config.load_config(args.config)
    check_feature_compatibility(config)


def cmd_just_snap(args):
    # for testing, behavior may differ
    config = Config.load_config(args.config)
    raw_snaps = ListPoolSnapshotsCmd(config.pool).check_output().decode()
    snapshots = YazSnapshots.from_cmd_output(raw_snaps, config.sigil)
    if snapshots.is_empty():
        snap = snapshots.begin_sequence(config)
        TakePoolSnapshotCmd(config.pool, snap.as_str()).check_call()
    else:
        snap = snapshots.next_in_seq()
        if snap is None:
            LOG.info('no snapshot action needed')
        else:
            TakePoolSnapshotCmd(config.pool, snap.as_str()).check_call()
    while snapshots.should_prune_eldest(config):
        eldest = snapshots.pop_eldest()
        remain = len(snapshots.snapshots)
        LOG.info(f'removing eldest snapshot {eldest.as_str()}, {remain} left')
        DestroyPoolSnapshotCmd(config.pool, eldest.as_str()).check_call()


def cmd_initial_seed(args):
    config = Config.load_config(args.config)
    check_feature_compatibility(config)
    raw_snaps = ListPoolSnapshotsCmd(config.pool).check_output().decode()
    snapshots = YazSnapshots.from_cmd_output(raw_snaps, config.sigil)
    if not snapshots.is_empty():
        msg = 'snapshots present! can not take initial seed'
        LOG.error(msg)
        raise Exception(msg)
    verify_remote_dataset_exits_but_empty(config)

    snap = snapshots.begin_sequence(config)
    TakePoolSnapshotCmd(config.pool, snap.as_str()).check_call()
    LOG.info('appears safe to start initial send')

    # avoiding -R on initial send because because it means we can't reusme the whole thing
    seed_snaps = ListPoolSnapshotsCmd(config.pool).check_output()
    seed_snaps = list(filter(lambda s: snap.as_str() in s,  seed_snaps.decode().split('\n')))
    cmds = []

    assert (config.pool + '@') in seed_snaps[0]
    cmds.append(RemotePipeShellCmd(InitialSendCmd(seed_snaps[0]),
                                   config.destination,
                                   RecvCmd(config.destination, force=True, resume=True)))

    for seed_snap in seed_snaps[1:]:  # skip special first one that is just the pool name
        if not seed_snap:
            continue
        # One of the seed snaps just created above
        if snap.as_str() in seed_snap:
            cmd = RemotePipeShellCmd(InitialSendCmd(seed_snap),
                                     config.destination,
                                     RecvCmd(config.destination, resume=True))
            cmds.append(cmd)
    LOG.info('upcoming commands...')
    for cmd in cmds:
        LOG.info(f'     {cmd.cmd_line()}')
    # on failure, human will have to complete manualy with tokens and continue from the above list
    LOG.info('starting initial send...')
    for cmd in cmds:
        cmd.check_call()


def cmd_backup(args):
    config = Config.load_config(args.config)

    raw_snaps = ListPoolSnapshotsCmd(config.pool).check_output().decode()
    snapshots = YazSnapshots.from_cmd_output(raw_snaps, config.sigil)

    # TODO: Need to rejiger logic (maybe with a flag?) so that we can see if
    # there is work to do and do that before taking another snapshot.  Otherwise
    # if there has been an error with a child dataset, on the backup destination
    # we will keep building up snapshots on the parent, but never make any
    # progress.  As a possible alternative or compliment, adjust the
    # remote_snapshots log below so that instead of just looking at the remote
    # parent, we look at the snapshots of all child datasets, and use the one
    # that is missing the most.  Unsure how this would interact with -F recv.
    snap = snapshots.next_in_seq(args.force_snapshot)
    if snap is None:
        LOG.info('most recent snapshot is new enough, nothing to do')
        return
    check_feature_compatibility(config)
    TakePoolSnapshotCmd(config.pool, snap.as_str()).check_call()

    raw_remote_snaps = RemoteShellCmd(
        config.destination,
        ListDatasetSnapshotsCmd(config.destination.dataset)).check_output().decode()
    remote_snapshots = list(map(lambda s: s.split('@')[1],
                                filter(lambda s: s, raw_remote_snaps.split('\n'))))
    LOG.debug(f'remote snapshots: {remote_snapshots}')
    cmds = []
    for idx, yaz_snap in enumerate(snapshots.snapshots):
        if yaz_snap.as_str() in remote_snapshots:
            continue
        else:
            prev = snapshots.snapshots[idx-1].as_str()
            cmd = RemotePipeShellCmd(IncrementalSendCmd(config.pool,
                                                        prev,
                                                        yaz_snap.as_str()),
                                     config.destination,
                                     RecvCmd(config.destination, force=args.force_recv))
            cmds.append(cmd)
    if not cmds:
        LOG.info('remote up to date, nothing to send')
        return
    LOG.info('upcoming commands...')
    for cmd in cmds:
        LOG.info(f'     {cmd.cmd_line()}')
    for cmd in cmds:
        cmd.check_call()
    LOG.info('all incremental sends complete')
    while args.prune and snapshots.should_prune_eldest(config):
        eldest = snapshots.pop_eldest()
        remain = len(snapshots.snapshots)
        LOG.info(f'removing eldest snapshot {eldest.as_str()}, {remain} left')
        DestroyPoolSnapshotCmd(config.pool, eldest.as_str()).check_call()


##### mainline #####


def make_parser():
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers()

    parser.add_argument('--log-level', dest='log_level', type=str, default='info',
                        choices=['critical', 'error', 'warning', 'info', 'debug'])
    parser.add_argument('--config', type=str, required=True,)

    cf_p = subparsers.add_parser('check-features')
    cf_p.set_defaults(func=cmd_check_features)

    js_p = subparsers.add_parser('just-snap')
    js_p.set_defaults(func=cmd_just_snap)

    is_p = subparsers.add_parser('initial-seed')
    is_p.set_defaults(func=cmd_initial_seed)

    b_p = subparsers.add_parser('backup')
    b_p.add_argument('--force-snapshot', dest='force_snapshot', action='store_true')
    b_p.add_argument('--no-force-snapshot', dest='force_snapshot', action='store_false')
    b_p.add_argument('--force-recv', dest='force_recv', action='store_true')
    b_p.add_argument('--no-force-recv', dest='force_recv', action='store_false')
    b_p.add_argument('--prune', dest='prune', action='store_true')
    b_p.add_argument('--no-prune', dest='prune', action='store_false')
    b_p.set_defaults(force_snapshot=False)
    b_p.set_defaults(force_recv=False)
    b_p.set_defaults(prune=True)
    b_p.set_defaults(func=cmd_backup)

    return parser


def main(argv):
    parser = make_parser()
    args = parser.parse_args(argv)
    numeric_level = getattr(logging, args.log_level.upper(), None)
    if not isinstance(numeric_level, int):
        raise ValueError('Invalid log level: %s' % loglevel)
    fmt = '[%(asctime)s] - %(filename)s:%(lineno)d - %(levelname)s - %(message)s'
    logging.basicConfig(format=fmt, level=numeric_level)

    if not hasattr(args, 'func'):
        print('ERROR: missing subcommand',  file=sys.stderr)
        parser.print_help()
    args.func(args)


if __name__ == '__main__':
    main(sys.argv[1:])


# Some references
# * https://medium.com/@ageitgey/learn-how-to-use-static-type-checking-in-python-3-6-in-10-minutes-12c86d72677b
# * https://serverfault.com/questions/137468/better-logging-for-cronjobs-send-cron-output-to-syslog
# * https://unix.stackexchange.com/questions/263677/how-to-one-way-mirror-an-entire-zfs-pool-to-another-zfs-pool
# * https://old.reddit.com/r/zfs/comments/7fqu1y/a_small_survey_of_zfs_remote_replication_tools

# Example permissions
# root@yggdrasil[~]# zfs allow  backup_y54 create,receive tank/backups/chris/y54/SIGIL
# root@yggdrasil[~]# zfs allow -c allow,atime,clone,compression,create,destroy,diff,hold,logbias,primarycache,promote,receive,recordsize,refreservation,release,rollback,secondarycache,send,setuid,snapdir,snapshot,sync,userprop,volsize,clone,compression,destroy,diff,hold,promote,receive,release,rollback,send,snapshot tank/backups/chris/y54/SIGIL

# NOTE: Because mountpoint is replicated (seemed good to avoid errors) then
# canmount=noauto needs to be set or multiple datasets try to mount in the same
# place

# NOTE: I ended up just not replicating mountpoint because of hard to debug
# issues
#cannot receive mountpoint property on tank/backups/chris/y54/delta: permission denied
#cannot receive atime property on tank/backups/chris/y54/delta: permission denied
# cannot receive compression property on tank/backups/chris/y54/delta: permission denied


# DEBUG:  mount -o ro -t zfs zroot/backup/ys76/alpha/HOME /mnt/backup
# ^^ zfs magic (delete queue?) may result in modifications when mounted
