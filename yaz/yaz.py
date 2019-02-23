#!/usr/bin/env python3.6

import abc
import argparse
import json
import logging
import subprocess
import sys

from typing import Dict, List


# note about mypy, pycodestyle,  other venv tools?  coverage
# note about how to do a pyton3 venv

# form yaz-${sigil}-${freq}-${hex_inc}-${hex_epoch_sec}
# sigil from config, something to make these unique'

# Yet Another ZFS replication script design: use config files, not soo much
# property magic.  Extra snapshots are okay, don't interfere with or compete
# with existing time based snapshots.  Simple enough that I can understand what
# is going on, focused on replciaitng an entire desktop pool of reasonable size,
# not a gigantic storage pool with thousands of containers.  Learn python 3
#

# cmds: check-features, just-snap, just-prune-local, just-prune-remote(fuzzy on how snapshots work? don't think this is needed), initial-seed-send

# LEFT OFF: Need to create a bhyve VM (FreeBSDD?) for testing, how all of -I and other optiosn work is unclaer.  Also testing...

LOG = logging.getLogger(__name__)

class Config():

    class Snapshots():

        def __init__(self, daily: int):
            self.daily = daily

        @staticmethod
        def from_json(d: Dict):
            # TODO: warn on other than daily
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


class AllPoolPropertiesCmd(ShellCmd):

    def __init__(self, pool: str):
        self.pool = pool

    def cmd_line(self):
        return f'zpool get -Hp all {self.pool}'


##### cmds #####

def cmd_foo(args):
    LOG.info('hi')
    print('hello foo')


# TODO: pull out into it's own function, many things will chec this first
def cmd_check_features(args):
    config = Config.load_config('example.json')
    local_props =  AllPoolPropertiesCmd(config.pool).check_output()
    local_features = parse_features(local_props.decode())
    remote_props = RemoteShellCmd(config.destination,
                                  AllPoolPropertiesCmd(config.destination.pool)).check_output()
    remote_features = parse_features(remote_props.decode())
    ok = True
    for key,val in local_features.items():
        if val == 'enabled':
            if key not in remote_features:
                LOG.error(f'!feature {key} not in remote_features')
                ok = False
            elif key in remote_features and remote_features[key] != 'enabled':
                LOG.error(f'!feature {key} disable remote_features')
                ok = False
    return ok

def cmd_just_snap(args):
    config = load_config('example.json')
    pass

##### mainline #####


def make_parser():
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers()

    parser.add_argument('--log-level', dest='log_level', type=str, default='warning',
                        choices=['critical', 'error', 'warning', 'info', 'debug'])

    foo_p = subparsers.add_parser('foo')
    foo_p.set_defaults(func=cmd_foo)

    cf_p = subparsers.add_parser('check-features')
    cf_p.set_defaults(func=cmd_check_features)

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



# https://medium.com/@ageitgey/learn-how-to-use-static-type-checking-in-python-3-6-in-10-minutes-12c86d72677b


# https://serverfault.com/questions/137468/better-logging-for-cronjobs-send-cron-output-to-syslog



# DEBUG:  mount -t zfs zroot/backup/ys76/GENTOO/build-dir /mnt/backup
