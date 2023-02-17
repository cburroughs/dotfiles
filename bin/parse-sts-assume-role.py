#!/usr/bin/env python3

import sys
import json

#aws sts assume-role --profile=foo --role-arn arn:bar  --role-session-name cli
if __name__ == '__main__':
    raw = sys.stdin.read()
    resp = json.loads(raw)
    credentials = resp['Credentials']

    if len(sys.argv) > 1 and sys.argv[1] == 'json':
        o = {'AWS_ACCESS_KEY_ID': credentials['AccessKeyId'],
             'AWS_SECRET_ACCESS_KEY':  credentials['SecretAccessKey'],
             'AWS_SESSION_TOKEN': credentials['SessionToken']}
        print(json.dumps(o,indent=4))
    else:
        print(f'export AWS_ACCESS_KEY_ID="{credentials["AccessKeyId"]}"')
        print(f'export AWS_SECRET_ACCESS_KEY="{credentials["SecretAccessKey"]}"')
        print(f'export AWS_SESSION_TOKEN="{credentials["SessionToken"]}"')
