#!/usr/bin/env python3.6

import abc
import argparse
import json
import sys
import subprocess

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


def load_config(fname):
    with open(fname, 'r') as f:
        return json.load(f)

# TODO: Some light config validation, not full json schema

def parse_features(lines):
    features = {}
    for line in lines.split('\n'):
        if 'feature@' in line:
            columns = line.split('\t')
            features[columns[1]] = columns[2]
    return features

##### shell command objects #####

class ShellCmd(abc.ABC):

    @abc.abstractmethod
    def cmd_line(self):
        pass

    def check_output(self):
        return subprocess.check_output(self.cmd_line(), shell=True)

    def check_call(self):
        return subprocess.check_call(self.cmd_line(), shell=True)


class RemoteShellCmd(ShellCmd):

    def __init__(self, username, hostname, cmd):
        self.username = username
        self.hostname = hostname
        self.cmd = cmd

    def cmd_line(self):
        return f"ssh {self.username}@{self.hostname} '{self.cmd.cmd_line()}'"


class AllPoolPropertiesCmd(ShellCmd):

    def __init__(self, pool):
        self.pool = pool

    def cmd_line(self):
        return f'zpool get -Hp all {self.pool}'


##### cmds #####

def cmd_foo(args):
    print('hello foo')


def cmd_check_features(args):
    config = load_config('example.json')
    # ShellCmd object pattern, and remove cmd object.. ABC types?
    local_props =  AllPoolPropertiesCmd(config['pool']).check_output()
    local_features = parse_features(local_props.decode())
    remote_props = RemoteShellCmd(config['destination']['user'],
                                  config['destination']['hostname'],
                                  AllPoolPropertiesCmd(config['destination']['dataset'].split('/')[0])).check_output()
    remote_features = parse_features(remote_props.decode())
    ok = True
    print(local_features)
    print(remote_features)
    for key,val in local_features.items():
        if val == 'enabled':
            if key not in remote_features:
                print(f'!feature {key} not in remote_features')
                ok = False
            elif key in remote_features and remote_features[key] != 'enabled':
                print(f'!feature {key} disable remote_features')
                ok = False
    return ok

def cmd_just_snap(args):
    config = load_config('example.json')
    pass

##### mainline #####


def make_parser():
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers()

    foo_p = subparsers.add_parser('foo')
    foo_p.set_defaults(func=cmd_foo)

    cf_p = subparsers.add_parser('check-features')
    cf_p.set_defaults(func=cmd_check_features)

    return parser


def main(argv):
    parser = make_parser()
    args = parser.parse_args(argv)
    if not hasattr(args, 'func'):
        print('ERROR: missing subcommand',  file=sys.stderr)
        parser.print_help()
    args.func(args)


if __name__ == '__main__':
    main(sys.argv[1:])



# https://medium.com/@ageitgey/learn-how-to-use-static-type-checking-in-python-3-6-in-10-minutes-12c86d72677b


# https://serverfault.com/questions/137468/better-logging-for-cronjobs-send-cron-output-to-syslog



# DEBUG:  mount -t zfs zroot/backup/ys76/GENTOO/build-dir /mnt/backup
